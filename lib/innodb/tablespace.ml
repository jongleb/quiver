open Base.Result.Let_syntax

type error =
  | Template of string
  | Shape of int
  | Row_too_long of int
  | Value_too_long of int
  | Unstorable of int
  | Out_of_order
  | Overlapping_runs
  | Too_many_extents
  | Too_many_rows
[@@deriving sexp_of ~portable]

let group = 16384

(** The largest record that stays on its page: InnoDB moves columns off-page
    from half an empty page on. *)
let largest_record = 8125

(** Bytes of a reference to a column stored off the record. *)
let reference = 20

(** A record's key as a node pointer holds it: the lengths of its variable fields as they
    precede the record, then the fields' bytes. *)
type key = { lengths : string; data : string }

(** Where a record's key comes from. *)
type keys = Stored  (** The record's own fields. *) | Numbered  (** The run numbers its rows. *)

type t = {
  layout : Layout.t;
  fields : Layout.field iarray;
  key_fields : Layout.storage list;
  keys : keys;
  runs : int Portable.Atomic.t;  (** Runs started, numbering their rows apart. *)
  file : File.t;
  template : string;
  space : int;
  segments : string;
  leaf_inode : int;
  top_inode : int;
  leaf_segment : int;
  top_segment : int;
  template_size : int;
  extents : int Portable.Atomic.t;
}

let create ~template (layout : Layout.t) file =
  let u16 at = String.get_uint16_be template at in
  let u32 at = Int32.to_int (String.get_int32_be template at) land 0xFFFF_FFFF in
  let%bind () =
    if String.length template < 5 * Page.size then Error (`Tablespace (Template "shorter than 5 pages"))
    else Ok ()
  in
  let root = layout.root * Page.size in
  let%bind () =
    match (layout.root, u16 (root + 24), u16 (root + 54), u16 ((2 * Page.size) + 24)) with
    | 4, 17855, 0, 3 -> Ok ()
    | _ -> Error (`Tablespace (Template "no empty root on page 4 or no inode page 2"))
  in
  let%bind () =
    if Int64.to_int (String.get_int64_be template (root + 66)) = layout.index then Ok ()
    else Error (`Tablespace (Template "the root belongs to another index"))
  in
  let leaf_inode = u16 (root + 74 + 8) and top_inode = u16 (root + 84 + 8) in
  let segment offset = Int64.to_int (String.get_int64_be template ((2 * Page.size) + offset)) in
  Ok
    {
      layout;
      fields = Iarray.of_list layout.fields;
      key_fields = Base.List.take (List.map (fun (field : Layout.field) -> field.storage) layout.fields) layout.key;
      keys = (match layout.fields with { storage = Row_id; _ } :: _ -> Numbered | _ -> Stored);
      runs = Portable.Atomic.make 0;
      file;
      template;
      space = u32 34;
      segments = String.sub template (root + 74) 20;
      leaf_inode;
      top_inode;
      leaf_segment = segment leaf_inode;
      top_segment = segment top_inode;
      template_size = u32 46;
      extents = Portable.Atomic.make 1;
    }

type records = { image : string; rows : int }

let put_int bytes pos n v =
  let rec go k =
    if k < n then (
      Bytes.set_uint8 bytes (pos + k) ((v lsr (8 * (n - 1 - k))) land 0xFF);
      go (k + 1))
  in
  go 0

let shaped t (columns @ local) =
  let rec go j =
    if j = Iarray.length t.fields then Ok ()
    else
      let field = Iarray.get t.fields j in
      if field.source < 0 then go (j + 1)
      else if field.source >= Iarray.length columns then Error (`Tablespace (Shape field.source))
      else
        let (Quiver_core.Column.Any column) = Iarray.get columns field.source in
        match (field.storage, Quiver_core.Column.repr column) with
        | Integer { bytes; number = Plain | Year | Days; _ }, Int when bytes < 8 -> go (j + 1)
        | Integer { number = Plain; _ }, Int64
        | Integer { number = Member _ | Members _; _ }, Text
        | (Float | Double), Float
        | (Datetime _ | Timestamp _ | Time _), Int64
        | (Decimal _ | Binary _ | Char _ | Text _ | Variable _ | Json), Text ->
            go (j + 1)
        | _ -> Error (`Tablespace (Shape field.source))
  in
  go 0 [@nontail]

let present (field : Layout.field) (columns @ local) i =
  match field.nullability with
  | Not_null -> Quiver_core.Column.Present
  | Null_bit _ ->
      let (Quiver_core.Column.Any column) = Iarray.get columns field.source in
      Quiver_core.Column.presence column i [@nontail]

let member members bytes pos length =
  let same member =
    let rec go k = k = length || (Bytes.get bytes (pos + k) = String.get member k && go (k + 1)) in
    String.length member = length && go 0
  in
  let rec go k = function
    | [] -> if length = 0 then This 0 else Null
    | member :: rest -> if same member then This k else go (k + 1) rest
  in
  go 1 members [@nontail]

let members members bytes pos length =
  let rec go from at acc =
    if at < pos + length && Bytes.get bytes at <> ',' then go from (at + 1) acc
    else
      match member members bytes from (at - from) with
      | Null -> Null
      | This 0 when length > 0 -> Null
      | This k ->
          let acc = if k = 0 then acc else acc lor (1 lsl (k - 1)) in
          if at < pos + length then go (at + 1) (at + 1) acc else This acc
  in
  go pos pos 0 [@nontail]

(** A decimal's text: the mask its groups are stored with, all ones below zero, and where its digits are. *)
type digits = { mask : int; whole : int; wholes : int; fraction : int; fractions : int }

let digits bytes pos length ~precision ~scale = exclave_
  let stop = pos + length in
  let rec run at = if at < stop && Bytes.get bytes at >= '0' && Bytes.get bytes at <= '9' then run (at + 1) else at in
  let rec zeros at = if at < stop && Bytes.get bytes at = '0' then zeros (at + 1) else at in
  let rec nonzero at = at < stop && ((Bytes.get bytes at >= '1' && Bytes.get bytes at <= '9') || nonzero (at + 1)) in
  let start = if length > 0 && Bytes.get bytes pos = '-' then pos + 1 else pos in
  let mask = if start > pos && nonzero start then -1 else 0 in
  let point = run start in
  let whole = zeros start in
  let fraction = if point < stop && Bytes.get bytes point = '.' then point + 1 else point in
  let last = if fraction > point then run fraction else point in
  let wholes = point - whole and fractions = last - fraction in
  if last <> stop || point = start || wholes > precision - scale || fractions > scale then Null
  else This { mask; whole; wholes; fraction; fractions }

let signed sign n v = match (sign : Layout.sign) with Signed -> v lxor (1 lsl ((8 * n) - 1)) | Unsigned -> v

let text_bytes (field : Layout.field) (columns @ local) =
  let (Quiver_core.Column.Any column) = Iarray.get columns field.source in
  match Quiver_core.Column.repr column with
  | Text -> Quiver_core.Column.text_bytes column [@nontail]
  | Bool | Int | Int64 | Float -> Bytes.create 0

let text_offset (field : Layout.field) (columns @ local) i =
  let (Quiver_core.Column.Any column) = Iarray.get columns field.source in
  match Quiver_core.Column.repr column with
  | Text -> Quiver_core.Column.text_offset column i [@nontail]
  | Bool | Int | Int64 | Float -> 0

let text_length (field : Layout.field) (columns @ local) i =
  let (Quiver_core.Column.Any column) = Iarray.get columns field.source in
  match Quiver_core.Column.repr column with
  | Text -> Quiver_core.Column.text_length column i [@nontail]
  | Bool | Int | Int64 | Float -> 0

let put_fraction bytes pos fsp fraction =
  match fsp with
  | 0 -> pos
  | 1 | 2 ->
      put_int bytes pos 1 (fraction / 10000);
      pos + 1
  | 3 | 4 ->
      put_int bytes pos 2 (fraction / 100);
      pos + 2
  | _ ->
      put_int bytes pos 3 fraction;
      pos + 3

let put_datetime bytes pos fsp date micros =
  let seconds = micros / 1_000_000 in
  let ymd = ((((date lsr 9) * 13) + ((date lsr 5) land 15)) lsl 5) lor (date land 31) in
  let hms = ((seconds / 3600) lsl 12) lor (((seconds / 60) mod 60) lsl 6) lor (seconds mod 60) in
  put_int bytes pos 5 ((ymd lsl 17) lor hms + 0x80_0000_0000);
  put_fraction bytes (pos + 5) fsp (micros mod 1_000_000)

let date_number days =
  let date = Quiver_core.Moment.date_of_days days in
  (Core.Date.year date lsl 9) lor (Core.Month.to_int (Core.Date.month date) lsl 5) lor Core.Date.day date

let stamp fields =
  let date = fields lsr 37 and time = fields land ((1 lsl 37) - 1) in
  let year = date lsr 9 and month = (date lsr 5) land 15 and day = date land 31 in
  if date = 0 && time = 0 then This 0
  else if year < 1970 || month < 1 || month > 12 || day < 1 then Null
  else if day > Core.Date.days_in_month ~year ~month:(Core.Month.of_int_exn month) then Null
  else
    let days = Quiver_core.Moment.days_since_epoch (Core.Date.create_exn ~y:year ~m:(Core.Month.of_int_exn month) ~d:day) in
    let micros = (days * Quiver_core.Moment.micros_per_day) + time in
    if micros / 1_000_000 > 0x7FFF_FFFF then Null else This micros

let latin1_byte code =
  if code < 0x80 || (code >= 0xA0 && code <= 0xFF) then This code
  else
    match code with
    | 0x81 | 0x8D | 0x8F | 0x90 | 0x9D -> This code
    | 0x20AC -> This 0x80
    | 0x201A -> This 0x82
    | 0x0192 -> This 0x83
    | 0x201E -> This 0x84
    | 0x2026 -> This 0x85
    | 0x2020 -> This 0x86
    | 0x2021 -> This 0x87
    | 0x02C6 -> This 0x88
    | 0x2030 -> This 0x89
    | 0x0160 -> This 0x8A
    | 0x2039 -> This 0x8B
    | 0x0152 -> This 0x8C
    | 0x017D -> This 0x8E
    | 0x2018 -> This 0x91
    | 0x2019 -> This 0x92
    | 0x201C -> This 0x93
    | 0x201D -> This 0x94
    | 0x2022 -> This 0x95
    | 0x2013 -> This 0x96
    | 0x2014 -> This 0x97
    | 0x02DC -> This 0x98
    | 0x2122 -> This 0x99
    | 0x0161 -> This 0x9A
    | 0x203A -> This 0x9B
    | 0x0153 -> This 0x9C
    | 0x017E -> This 0x9E
    | 0x0178 -> This 0x9F
    | _ -> Null

let width lead = if lead < 0x80 then 1 else if lead < 0xE0 then 2 else if lead < 0xF0 then 3 else 4

let latin1_at source k =
  let lead = Bytes.get_uint8 source k in
  let tail n = Bytes.get_uint8 source (k + n) land 0x3F in
  match width lead with
  | 1 -> This lead
  | 2 when lead >= 0xC2 -> latin1_byte (((lead land 0x1F) lsl 6) lor tail 1)
  | 3 -> latin1_byte (((lead land 0x0F) lsl 12) lor (tail 1 lsl 6) lor tail 2)
  | _ -> Null

let latin1_length source at length =
  let stop = at + length in
  let rec go k n =
    if k = stop then This n
    else
      let next = k + width (Bytes.get_uint8 source k) in
      if next > stop then Null else match latin1_at source k with Null -> Null | This _ -> go next (n + 1)
  in
  go at 0

let put_latin1 source at length bytes pos =
  let stop = at + length in
  let rec go k pos =
    if k >= stop then pos
    else
      let byte = match latin1_at source k with This byte -> byte | Null -> 0x3F in
      Bytes.set_uint8 bytes pos byte;
      go (k + width (Bytes.get_uint8 source k)) (pos + 1)
  in
  go at pos

let put_field bytes pos (field : Layout.field) (columns @ local) i =
  match field.storage with
  | Row_id | Trx_id ->
      Bytes.fill bytes pos 6 '\000';
      pos + 6
  | Roll_ptr ->
      Bytes.set_uint8 bytes pos 0x80;
      Bytes.fill bytes (pos + 1) 6 '\000';
      pos + 7
  | Integer { number = Member _ | Members _; _ }
  | Decimal _ | Binary _ | Char _ | Text _ | Variable _ | Json -> (
      let source = text_bytes field columns and at = text_offset field columns i and length = text_length field columns i in
      match field.storage with
      | Integer { bytes = n; sign; number = Member listed } ->
          let k = match member listed source at length with This k -> k | Null -> 0 in
          put_int bytes pos n (signed sign n k);
          pos + n
      | Integer { bytes = n; sign; number = Members listed } ->
          let k = match members listed source at length with This k -> k | Null -> 0 in
          put_int bytes pos n (signed sign n k);
          pos + n
      | Decimal { precision; scale } -> (
          match digits source at length ~precision ~scale with
          | Null -> pos
          | This { mask; whole; wholes; fraction; fractions } ->
              let integral = precision - scale in
              let digit p =
                if p < integral then
                  let k = integral - 1 - p in
                  if k < wholes then Char.code (Bytes.get source (whole + wholes - 1 - k)) - 48 else 0
                else
                  let k = p - integral in
                  if k < fractions then Char.code (Bytes.get source (fraction + k)) - 48 else 0
              in
              let rec group p count value = if count = 0 then value else group (p + 1) (count - 1) ((value * 10) + digit p) in
              let rec put pos p =
                if p >= precision then pos
                else
                  let count =
                    if p = 0 && integral mod 9 <> 0 then integral mod 9
                    else if p < integral then 9
                    else Int.min 9 (precision - p)
                  in
                  let size = Int.min 4 ((count + 1) / 2) in
                  put_int bytes pos size (group p count 0 lxor mask);
                  put (pos + size) (p + count)
              in
              let next = put pos 0 in
              Bytes.set_uint8 bytes pos (Bytes.get_uint8 bytes pos lxor 0x80);
              next)
      | Binary n ->
          Bytes.blit source at bytes pos length;
          Bytes.fill bytes (pos + length) (n - length) '\000';
          pos + n
      | Char { least; charset = Latin1; _ } ->
          let next = put_latin1 source at length bytes pos in
          Bytes.fill bytes next (pos + least - next) ' ';
          pos + least
      | Char { least; _ } ->
          Bytes.blit source at bytes pos length;
          let padding = Int.max 0 (least - length) in
          Bytes.fill bytes (pos + length) padding ' ';
          pos + length + padding
      | Text { charset = Latin1; _ } -> put_latin1 source at length bytes pos
      | _ ->
          Bytes.blit source at bytes pos length;
          pos + length)
  | Integer _ | Float | Double | Datetime _ | Timestamp _ | Time _ -> (
      let (Quiver_core.Column.Any column) = Iarray.get columns field.source in
      match (field.storage, Quiver_core.Column.repr column) with
      | Integer { bytes = 8; sign = Signed; _ }, Int64 ->
          Bytes.set_int64_be bytes pos
            (Int64.logxor (Int64_u.to_int64 (Quiver_core.Column.get_int64 column i)) Int64.min_int);
          pos + 8
      | Integer { bytes = 8; sign = Unsigned; _ }, Int64 ->
          Bytes.set_int64_be bytes pos (Int64_u.to_int64 (Quiver_core.Column.get_int64 column i));
          pos + 8
      | Integer { bytes = n; _ }, Int64 ->
          put_int bytes pos n (Int64.to_int (Int64_u.to_int64 (Quiver_core.Column.get_int64 column i)));
          pos + n
      | Integer { bytes = n; sign; number }, Int ->
          let v = Quiver_core.Column.get column i in
          let v =
            match number with
            | Year -> if v = 0 then 0 else v - 1900
            | Days -> date_number v
            | Plain | Member _ | Members _ -> v
          in
          put_int bytes pos n (signed sign n v);
          pos + n
      | Float, Float ->
          Bytes.set_int32_le bytes pos (Int32.bits_of_float (Float_u.to_float (Quiver_core.Column.get_float column i)));
          pos + 4
      | Double, Float ->
          Bytes.set_int64_le bytes pos (Int64.bits_of_float (Float_u.to_float (Quiver_core.Column.get_float column i)));
          pos + 8
      | Datetime { fsp; clock }, Int64 -> (
          let v = Int64.to_int (Int64_u.to_int64 (Quiver_core.Column.get_int64 column i)) in
          match clock with
          | Fields -> put_datetime bytes pos fsp (v lsr 37) (v land ((1 lsl 37) - 1))
          | Microseconds ->
              let days = Base.Int.( /% ) v Quiver_core.Moment.micros_per_day in
              put_datetime bytes pos fsp (date_number days) (v - (days * Quiver_core.Moment.micros_per_day)))
      | Timestamp { fsp; clock }, Int64 ->
          let v = Int64.to_int (Int64_u.to_int64 (Quiver_core.Column.get_int64 column i)) in
          let micros =
            match clock with
            | Microseconds -> v
            | Fields -> (
                match stamp v with
                | This micros -> micros
                | Null -> 0)
          in
          put_int bytes pos 4 (micros / 1_000_000);
          put_fraction bytes (pos + 4) fsp (micros mod 1_000_000)
      | Time fsp, Int64 -> (
          let v = Int64.to_int (Int64_u.to_int64 (Quiver_core.Column.get_int64 column i)) in
          let a = abs v in
          let seconds = a / 1_000_000 in
          let hms = ((seconds / 3600) lsl 12) lor (((seconds / 60) mod 60) lsl 6) lor (seconds mod 60) in
          let packed = (hms lsl 24) + (a mod 1_000_000) in
          let nr = if v < 0 then -packed else packed in
          match fsp with
          | 0 ->
              put_int bytes pos 3 (0x80_0000 + (nr asr 24));
              pos + 3
          | 5 | 6 ->
              put_int bytes pos 6 (nr + 0x8000_0000_0000);
              pos + 6
          | _ ->
              put_int bytes pos 3 (0x80_0000 + (nr asr 24));
              put_fraction bytes (pos + 3) fsp (nr mod (1 lsl 24)))
      | _ -> pos)

let encode t (columns @ local) ~rows =
  let n_fields = Iarray.length t.fields in
  let encoded = Array.make n_fields "" in
  let rec sizes j i extra data =
    if j = n_fields then Ok (extra, data)
    else
      let field = Iarray.get t.fields j in
      match present field columns i with
      | Null -> sizes (j + 1) i extra data
      | Present -> (
          let varying big length =
            if (not big) && length > 255 then Error (`Tablespace (Value_too_long length))
            else
              let lengths = if big && length >= 128 then 2 else 1 in
              sizes (j + 1) i (extra + lengths) (data + length)
          in
          let fixed () =
            match Layout.length field.storage with
            | Fixed n -> sizes (j + 1) i extra (data + n)
            | Varying _ -> sizes (j + 1) i extra data
          in
          let text charset =
            let source = text_bytes field columns and at = text_offset field columns i and length = text_length field columns i in
            let rec three k =
              if k = at + length then Ok length
              else if Bytes.get_uint8 source k >= 0xF0 then Error (`Tablespace (Unstorable field.source))
              else three (k + 1)
            in
            match (charset : Layout.charset) with
            | Utf8mb4 -> Ok length
            | Utf8mb3 -> three at
            | Latin1 -> (
                match latin1_length source at length with
                | This length -> Ok length
                | Null -> Error (`Tablespace (Unstorable field.source)))
          in
          match field.storage with
          | Variable { big } ->
              let length = text_length field columns i in
              varying big length [@nontail]
          | Text { big; charset } -> (
              match text charset with Error error -> Error error | Ok length -> varying big length [@nontail])
          | Json -> (
              let source = text_bytes field columns and at = text_offset field columns i and length = text_length field columns i in
              match Json.binary source ~pos:at ~length with
              | Error (`Json _) -> Error (`Tablespace (Unstorable field.source))
              | Ok binary ->
                  Array.set encoded j binary;
                  varying true (String.length binary) [@nontail])
          | Char { least; charset = Latin1; _ } -> (
              match text Latin1 with
              | Error error -> Error error
              | Ok length -> if length > least then Error (`Tablespace (Value_too_long length)) else fixed () [@nontail])
          | Char { least; big; charset } -> (
              match text charset with Error error -> Error error | Ok length -> varying big (Int.max least length) [@nontail])
          | Binary n ->
              let length = text_length field columns i in
              if length > n then Error (`Tablespace (Value_too_long length)) else fixed () [@nontail]
          | Integer { number = Member listed; _ } -> (
              let source = text_bytes field columns and at = text_offset field columns i and length = text_length field columns i in
              match member listed source at length with
              | Null -> Error (`Tablespace (Unstorable field.source))
              | This _ -> fixed () [@nontail])
          | Integer { number = Members listed; _ } -> (
              let source = text_bytes field columns and at = text_offset field columns i and length = text_length field columns i in
              match members listed source at length with
              | Null -> Error (`Tablespace (Unstorable field.source))
              | This _ -> fixed () [@nontail])
          | Decimal { precision; scale } -> (
              let source = text_bytes field columns and at = text_offset field columns i and length = text_length field columns i in
              match digits source at length ~precision ~scale with
              | Null -> Error (`Tablespace (Unstorable field.source))
              | This _ -> fixed () [@nontail])
          | Timestamp { clock = Fields; _ } -> (
              let (Quiver_core.Column.Any column) = Iarray.get columns field.source in
              match Quiver_core.Column.repr column with
              | Int64 -> (
                  match stamp (Int64.to_int (Int64_u.to_int64 (Quiver_core.Column.get_int64 column i))) with
                  | Null -> Error (`Tablespace (Unstorable field.source))
                  | This _ -> fixed () [@nontail])
              | _ -> Error (`Tablespace (Shape field.source)))
          | Integer _ | Float | Double | Datetime _ | Timestamp _ | Time _ | Row_id | Trx_id | Roll_ptr -> fixed () [@nontail])
  in
  let outside = Bytes.make n_fields '\000' in
  let value_length j i =
    let field = Iarray.get t.fields j in
    match field.storage with
    | Json -> String.length (Array.get encoded j)
    | Text { charset = Latin1; _ } -> (
        match latin1_length (text_bytes field columns) (text_offset field columns i) (text_length field columns i) with
        | This length -> length
        | Null -> 0)
    | _ -> text_length field columns i
  in
  let rec fill bytes start origin j pos lengths tail i =
    if j < n_fields then
      let field = Iarray.get t.fields j in
      match (present field columns i, field.nullability) with
      | Null, Null_bit bit ->
          let at = origin - 6 - (bit / 8) in
          Bytes.set_uint8 bytes at (Bytes.get_uint8 bytes at lor (1 lsl (bit mod 8)));
          fill bytes start origin (j + 1) pos lengths tail i
      | _ when Bytes.get outside j <> '\000' ->
          let length = value_length j i in
          Bytes.set_int32_be bytes pos (Int32.of_int t.space);
          Bytes.set_int32_be bytes (pos + 4) 0l;
          Bytes.set_int32_be bytes (pos + 8) 1l;
          Bytes.set_int32_be bytes (pos + 12) 0l;
          Bytes.set_int32_be bytes (pos + 16) (Int32.of_int length);
          Bytes.set_uint8 bytes lengths 0xC0;
          Bytes.set_uint8 bytes (lengths - 1) reference;
          Bytes.set_uint16_be bytes tail (pos - start);
          Bytes.set_int32_be bytes (tail + 2) (Int32.of_int length);
          let () =
            match field.storage with
            | Json -> Bytes.blit_string (Array.get encoded j) 0 bytes (tail + 6) length
            | Text { charset = Latin1; _ } ->
                let (_ : int) =
                  put_latin1 (text_bytes field columns) (text_offset field columns i) (text_length field columns i) bytes (tail + 6)
                in
                ()
            | _ -> Bytes.blit (text_bytes field columns) (text_offset field columns i) bytes (tail + 6) length
          in
          fill bytes start origin (j + 1) (pos + reference) (lengths - 2) (tail + 6 + length) i
      | _ ->
          let next =
            match field.storage with
            | Json ->
                let binary = Array.get encoded j in
                Bytes.blit_string binary 0 bytes pos (String.length binary);
                pos + String.length binary
            | _ -> put_field bytes pos field columns i
          in
          let lengths =
            match Layout.length field.storage with
            | Varying { big } ->
                let length = next - pos in
                if big && length >= 128 then (
                  Bytes.set_uint8 bytes lengths (0x80 lor (length lsr 8));
                  Bytes.set_uint8 bytes (lengths - 1) (length land 0xFF);
                  lengths - 2)
                else (
                  Bytes.set_uint8 bytes lengths length;
                  lengths - 1)
            | Fixed _ -> lengths
          in
          fill bytes start origin (j + 1) next lengths tail i
  in
  let length_outside j i =
    let field = Iarray.get t.fields j in
    match (field.storage, present field columns i) with
    | (Variable { big = true } | Text { big = true; _ } | Json), Present when Bytes.get outside j = '\000' -> value_length j i
    | _ -> 0
  in
  let rec longest j best i =
    if j = n_fields then best
    else
      let length = length_outside j i in
      if length > 2 * reference && (best < 0 || length > length_outside best i) then longest (j + 1) j i
      else longest (j + 1) best i
  in
  let rec fit ((extra, data) as sized) i =
    let size = extra + 5 + data in
    if size <= largest_record then Ok sized
    else
      match longest t.layout.key (-1) i with
      | -1 -> Error (`Tablespace (Row_too_long size))
      | j ->
          let length = length_outside j i in
          let lengths = if length >= 128 then 2 else 1 in
          Bytes.set outside j '\001';
          fit (extra - lengths + 2, data - length + reference) i
  in
  let rec moved j i count bytes =
    if j = n_fields then (count, bytes)
    else if Bytes.get outside j <> '\000' then
      moved (j + 1) i (count + 1) (bytes + 6 + value_length j i)
    else moved (j + 1) i count bytes
  in
  let rec row bytes pos i =
    if i = rows then Ok { image = Bytes.sub_string bytes 0 pos; rows }
    else (
      Bytes.fill outside 0 n_fields '\000';
      let fitted = match sizes 0 i t.layout.null_bytes 0 with Error _ as failed -> failed | Ok sized -> fit sized i in
      match fitted with
      | Error _ as failed -> failed
      | Ok (extra, data) ->
          let size = extra + 5 + data in
          let count, tail = moved 0 i 0 0 in
          let bytes =
            if pos + 6 + size + tail <= Bytes.length bytes then bytes
            else Bytes.extend bytes 0 (Int.max (Bytes.length bytes) (6 + size + tail))
          in
          Bytes.set_uint16_be bytes pos size;
          Bytes.set_uint16_be bytes (pos + 2) (extra + 5);
          Bytes.set_uint16_be bytes (pos + 4) count;
          let start = pos + 6 in
          let origin = start + extra + 5 in
          Bytes.fill bytes start (extra + 5) '\000';
          fill bytes start origin 0 origin (origin - 6 - t.layout.null_bytes) (start + size) i;
          row bytes (start + size + tail) (i + 1))
  in
  match shaped t columns with
  | Error _ as failed -> failed
  | Ok () -> row (Bytes.create (Int.max 64 (rows * 32))) 0 0 [@nontail]

let image records = records.image

let claim t =
  let rec next () =
    let k = Portable.Atomic.fetch_and_add t.extents 1 in
    if k mod 256 = 0 then next () else k
  in
  let k = next () in
  if 64 * (k + 1) > 0xFFFF_FFF0 then Error (`Tablespace Too_many_extents) else Ok k

(** Whether a run has taken a key yet. *)
type seen = Nothing | Key

type run = {
  owner : t;
  base : int;  (** The first row number of a [Numbered] run. *)
  rank : int;  (** Its place among the table's runs, for keys of [Source_order]. *)
  mutable numbered : int;  (** Rows it numbered. *)
  mutable last : key;
  mutable seen : seen;
  mutable page : Page.t option;
  mutable number : int;
  mutable previous : int;
  mutable first : int;
  mutable low : key;
  mutable extent : (int * int) option;  (** Extent and the pages used in it. *)
  mutable used : (int * int) list;
  mutable pointers : (key * int) list;  (** First key and number of each page, last first. *)
}

let run owner ~rank =
  {
    owner;
    base = (Portable.Atomic.fetch_and_add owner.runs 1 lsl 32) + 1;
    rank;
    numbered = 0;
    last = { lengths = ""; data = "" };
    seen = Nothing;
    page = None;
    number = Page.null;
    previous = Page.null;
    first = Page.null;
    low = { lengths = ""; data = "" };
    extent = None;
    used = [];
    pointers = [];
  }

let next_number run =
  let%map k, used =
    match run.extent with
    | Some (k, used) when used < 64 -> Ok (k, used)
    | Some full ->
        run.used <- full :: run.used;
        let%map k = claim run.owner in
        (k, 0)
    | None ->
        let%map k = claim run.owner in
        (k, 0)
  in
  run.extent <- Some (k, used + 1);
  (64 * k) + used

let write_page run page ~next =
  let t = run.owner in
  Page.close page;
  Page.seal (Page.bytes page) ~kind:Index ~number:run.number ~prev:run.previous ~next ~space:t.space;
  File.write t.file run.number (Page.bytes page)

let store run image ~pos ~length =
  let t = run.owner in
  let first_room = Page.size - 96 - (10 * 60) - 8 and data_room = Page.size - 49 - 8 in
  let index_room = (Page.size - 39 - 8) / 60 in
  let pieces = 1 + ((Int.max 0 (length - first_room) + data_room - 1) / data_room) in
  let index_pages = (Int.max 0 (pieces - 10) + index_room - 1) / index_room in
  let rec numbers n acc = if n = 0 then Ok (List.rev acc) else Result.bind (next_number run) (fun k -> numbers (n - 1) (k :: acc)) in
  let%bind first = next_number run in
  let%bind data = numbers (pieces - 1) [] in
  let%bind index = numbers index_pages [] in
  let first_page = Bytes.make Page.size '\000' in
  let index_bytes = Array.of_list (List.map (fun _ -> Bytes.make Page.size '\000') index) in
  let index = Array.of_list index in
  let slots = 10 + (index_room * index_pages) in
  let slot s =
    if s < 10 then (first_page, first, 96 + (60 * s))
    else
      let q = (s - 10) / index_room in
      (index_bytes.(q), index.(q), 39 + (60 * ((s - 10) mod index_room)))
  in
  let address bytes at (page, offset) =
    Bytes.set_int32_be bytes at (Int32.of_int page);
    Bytes.set_uint16_be bytes (at + 4) offset
  in
  let null = (Page.null, 0) in
  let link base members =
    let count = Array.length members in
    let place s =
      let _, page, offset = slot members.(s) in
      (page, offset)
    in
    let around k = if k >= 0 && k < count then place k else null in
    Bytes.set_int32_be first_page base (Int32.of_int count);
    address first_page (base + 4) (around 0);
    address first_page (base + 10) (around (count - 1));
    Array.iteri
      (fun k s ->
        let bytes, _, offset = slot s in
        address bytes offset (around (k - 1));
        address bytes (offset + 6) (around (k + 1));
        Bytes.set_int32_be bytes (offset + 12) 0l;
        address bytes (offset + 16) null;
        address bytes (offset + 22) null)
      members
  in
  link 64 (Array.init pieces Fun.id);
  link 80 (Array.init (slots - pieces) (fun k -> pieces + k));
  let rec free s =
    if s < slots then (
      let bytes, _, offset = slot s in
      Bytes.set_int32_be bytes (offset + 48) (Int32.of_int Page.null);
      free (s + 1))
  in
  free pieces;
  let piece s ~page ~length =
    let bytes, _, offset = slot s in
    Bytes.set_int32_be bytes (offset + 48) (Int32.of_int page);
    Bytes.set_uint16_be bytes (offset + 52) length;
    Bytes.set_int32_be bytes (offset + 56) 1l
  in
  let here = Int.min length first_room in
  Bytes.set_int32_be first_page 40 1l;
  Bytes.set_int32_be first_page 54 (Int32.of_int here);
  Bytes.blit_string image pos first_page 696 here;
  piece 0 ~page:first ~length:here;
  let rec write_data s at = function
    | [] -> Ok ()
    | number :: rest ->
        let length = Int.min data_room (length - at) in
        let bytes = Bytes.make Page.size '\000' in
        Bytes.set_int32_be bytes 39 (Int32.of_int length);
        Bytes.blit_string image (pos + at) bytes 49 length;
        piece s ~page:number ~length;
        Page.seal bytes ~kind:Lob_data ~number ~prev:0 ~next:Page.null ~space:t.space;
        let%bind () = File.write t.file number bytes in
        write_data (s + 1) (at + length) rest
  in
  let%bind () = write_data 1 here data in
  let%bind () =
    Base.List.fold_result (List.init index_pages Fun.id) ~init:() ~f:(fun () q ->
        let next = if q > 0 then index.(q - 1) else Page.null in
        Page.seal index_bytes.(q) ~kind:Lob_index ~number:index.(q) ~prev:0 ~next ~space:t.space;
        File.write t.file index.(q) index_bytes.(q))
  in
  let next = if index_pages > 0 then index.(index_pages - 1) else Page.null in
  Page.seal first_page ~kind:Lob_first ~number:first ~prev:0 ~next ~space:t.space;
  let%map () = File.write t.file first first_page in
  first

let add run { image; rows } =
  let t = run.owner in
  let stored origin =
    let header = origin - 5 - t.layout.null_bytes in
    let rec go fields at data =
      match fields with
      | [] -> { lengths = String.sub image at (header - at); data = String.sub image origin data }
      | storage :: rest -> (
          match Layout.length storage with
          | Fixed n -> go rest at (data + n)
          | Varying { big } ->
              let first = String.get_uint8 image (at - 1) in
              if big && first land 0x80 <> 0 then
                go rest (at - 2) (data + (((first land 0x3F) lsl 8) lor String.get_uint8 image (at - 2)))
              else go rest (at - 1) (data + first))
    in
    go t.key_fields header 0
  in
  let rec page_for size =
    match run.page with
    | Some page when Page.room page size = Fits -> Ok page
    | Some page ->
        let%bind next = next_number run in
        let%bind () = write_page run page ~next in
        run.previous <- run.number;
        run.number <- next;
        run.page <- None;
        page_for size
    | None ->
        let%map () =
          if run.number = Page.null then (
            let%map number = next_number run in
            run.number <- number;
            run.first <- number)
          else Ok ()
        in
        let page = Page.create ~level:0 ~index:t.layout.index in
        run.page <- Some page;
        page
  in
  let rec record pos i =
    if i = rows then Ok ()
    else
      let length = String.get_uint16_be image pos and origin = String.get_uint16_be image (pos + 2) in
      let count = String.get_uint16_be image (pos + 4) in
      let start = pos + 6 in
      let%bind key =
        match t.keys with
        | Stored -> Ok (stored (start + origin))
        | Numbered ->
            if run.numbered < 0xFFFF_FFFF && run.base < 1 lsl 48 then (
              let id = Bytes.create 6 in
              Bytes.set_uint16_be id 0 ((run.base + run.numbered) lsr 32);
              Bytes.set_int32_be id 2 (Int32.of_int ((run.base + run.numbered) land 0xFFFF_FFFF));
              run.numbered <- run.numbered + 1;
              Ok { lengths = ""; data = Bytes.to_string id })
            else Error (`Tablespace Too_many_rows)
      in
      let%bind () =
        match (t.layout.order, run.seen) with
        | Bytes_order, Key when String.compare key.data run.last.data <= 0 -> Error (`Tablespace Out_of_order)
        | Bytes_order, (Key | Nothing) | Source_order, _ -> Ok ()
      in
      let%bind page = page_for length in
      let () =
        match (Page.count page, run.seen) with
        | 0, Nothing ->
            run.low <- key;
            run.pointers <- (key, run.number) :: run.pointers
        | 0, Key -> run.pointers <- (key, run.number) :: run.pointers
        | _ -> ()
      in
      let placed = Page.place page image ~pos:start ~length ~origin - origin in
      let () = match t.keys with Stored -> () | Numbered -> Bytes.blit_string key.data 0 (Page.bytes page) (placed + origin) 6 in
      let rec outside k at =
        if k = count then Ok at
        else
          let reference = String.get_uint16_be image at and bytes = String.get_int32_be image (at + 2) in
          let bytes = Int32.to_int bytes land 0xFFFF_FFFF in
          let%bind first = store run image ~pos:(at + 6) ~length:bytes in
          Bytes.set_int32_be (Page.bytes page) (placed + reference + 4) (Int32.of_int first);
          outside (k + 1) (at + 6 + bytes)
      in
      let%bind next = outside 0 (start + length) in
      run.last <- key;
      run.seen <- Key;
      record next (i + 1)
  in
  record 0 0

type pages = {
  first : int;
  last : int;
  rank : int;
  low : key;  (** The run's first key. *)
  high : key;  (** Its last key. *)
  pointers : (key * int) list;  (** First key and number of each page. *)
  used : (int * int) list;  (** Extents and the pages used in each. *)
}

type finished = Empty | Pages of pages

let close run =
  match run.page with
  | None -> Ok Empty
  | Some page ->
      let%map () = write_page run page ~next:Page.null in
      Pages
        {
          first = run.first;
          last = run.number;
          rank = run.rank;
          low = run.low;
          high = run.last;
          pointers = List.rev run.pointers;
          used = Option.fold ~none:run.used ~some:(fun current -> current :: run.used) run.extent;
        }

let stamp bytes =
  let sum = Int32.of_int (Page.checksum bytes ~pos:0) in
  Bytes.set_int32_be bytes 0 sum;
  Bytes.set_int32_be bytes (Page.size - 8) sum

let finish t finished =
  let filled = List.filter_map (function Empty -> None | Pages pages -> Some pages) finished in
  let%bind runs =
    match t.layout.order with
    | Source_order -> Ok (List.sort (fun a b -> Int.compare a.rank b.rank) filled)
    | Bytes_order ->
        let runs = List.sort (fun a b -> String.compare a.low.data b.low.data) filled in
        let rec apart = function
          | a :: (b :: _ as rest) ->
              if String.compare a.high.data b.low.data < 0 then apart rest else Error (`Tablespace Overlapping_runs)
          | [ _ ] | [] -> Ok runs
        in
        apart runs
  in
  let leaves = Base.List.sum (module Base.Int) runs ~f:(fun pages -> List.length pages.pointers) in
  let u32 bytes at = Int32.to_int (Bytes.get_int32_be bytes at) land 0xFFFF_FFFF in
  let read n =
    let bytes = Bytes.create Page.size in
    let%map () = File.read t.file n bytes in
    bytes
  in
  let seal bytes ~number ~prev ~next =
    Page.seal bytes ~kind:Index ~number ~prev ~next ~space:t.space;
    File.write t.file number bytes
  in
  let%bind leaf_used =
    match runs with
    | [ { first; last; used; _ } ] when first = last ->
        let%bind bytes = read first in
        let%bind () = File.write t.file first (Bytes.make Page.size '\000') in
        Bytes.blit_string t.segments 0 bytes 74 20;
        let%map () = seal bytes ~number:t.layout.root ~prev:Page.null ~next:Page.null in
        List.map
          (fun (k, n) ->
            let pages = List.init n Fun.id in
            if k = first / 64 then (k, List.filter (fun page -> page <> first mod 64) pages) else (k, pages))
          used
    | _ ->
        let edges = Array.of_list runs in
        let relink r =
          let pages = edges.(r) in
          let prev = if r > 0 then edges.(r - 1).last else Page.null in
          let next = if r + 1 < Array.length edges then edges.(r + 1).first else Page.null in
          if pages.first = pages.last then
            let%bind bytes = read pages.first in
            seal bytes ~number:pages.first ~prev ~next
          else
            let%bind first = read pages.first in
            let%bind () = seal first ~number:pages.first ~prev ~next:(u32 first 12) in
            let%bind last = read pages.last in
            seal last ~number:pages.last ~prev:(u32 last 8) ~next
        in
        let%map () =
          Base.List.fold_result (List.init (Array.length edges) Fun.id) ~init:() ~f:(fun () r -> relink r)
        in
        List.concat_map (fun pages -> List.map (fun (k, n) -> (k, List.init n Fun.id)) pages.used) runs
  in
  let frags = ref [] and top_used = ref [] and top_extent = ref None in
  let upper_number () =
    match List.length !frags with
    | n when n < 31 ->
        frags := (5 + n) :: !frags;
        Ok (5 + n)
    | _ ->
        let%map k, used =
          match !top_extent with
          | Some (k, used) when used < 64 -> Ok (k, used)
          | Some full ->
              top_used := full :: !top_used;
              let%map k = claim t in
              (k, 0)
          | None ->
              let%map k = claim t in
              (k, 0)
        in
        top_extent := Some (k, used + 1);
        (64 * k) + used
  in
  let root page =
    Page.close page;
    Bytes.blit_string t.segments 0 (Page.bytes page) 74 20;
    seal (Page.bytes page) ~number:t.layout.root ~prev:Page.null ~next:Page.null
  in
  let rec level height pointers =
    let firsts =
      List.rev
        (List.fold_left
           (fun pages ({ lengths; data } as key, child) ->
             let extra = String.length lengths + t.layout.null_bytes and size = String.length data + 4 in
             let pages =
               match pages with
               | (page, _) :: _ when Page.room page (extra + 5 + size) = Fits -> pages
               | _ -> (Page.create ~level:height ~index:t.layout.index, key) :: pages
             in
             let page, _ = List.hd pages in
             let flag : Page.flag = if List.length pages = 1 && Page.count page = 0 then Leftmost else Plain in
             let origin = Page.append page ~extra ~data:size Node_pointer flag in
             Bytes.blit_string lengths 0 (Page.bytes page) (origin - 5 - extra) (String.length lengths);
             Bytes.blit_string data 0 (Page.bytes page) origin (String.length data);
             Bytes.set_int32_be (Page.bytes page) (origin + String.length data) (Int32.of_int child);
             pages)
           [] pointers)
    in
    let pages = List.map fst firsts in
    match pages with
    | [ top ] -> root top
    | _ ->
        let%bind numbers = Base.Result.all (List.map (fun _ -> upper_number ()) pages) in
        let numbers = Array.of_list numbers in
        let%bind () =
          Base.List.fold_result (List.mapi (fun j page -> (j, page)) pages) ~init:() ~f:(fun () (j, page) ->
              Page.close page;
              let prev = if j > 0 then numbers.(j - 1) else Page.null in
              let next = if j + 1 < Array.length numbers then numbers.(j + 1) else Page.null in
              seal (Page.bytes page) ~number:numbers.(j) ~prev ~next)
        in
        level (height + 1)
          (List.mapi
             (fun j (_, key) -> (key, numbers.(j)))
             firsts)
  in
  let%bind () =
    match leaves with
    | 0 -> root (Page.create ~level:0 ~index:t.layout.index)
    | 1 -> Ok ()
    | _ -> level 1 (List.concat_map (fun pages -> pages.pointers) runs)
  in
  let top_used =
    List.map
      (fun (k, n) -> (k, List.init n Fun.id))
      (Option.fold ~none:!top_used ~some:(fun current -> current :: !top_used) !top_extent)
  in
  let frags = List.rev !frags in
  let extents = List.map fst leaf_used @ List.map fst top_used in
  let size =
    match extents with
    | [] -> Base.List.fold frags ~init:t.template_size ~f:(fun size page -> Int.max size (page + 1))
    | _ -> 64 * (Base.List.fold extents ~init:0 ~f:Int.max + 1)
  in
  let groups = (size + group - 1) / group in
  let template_page n = Bytes.of_string (String.sub t.template (n * Page.size) Page.size) in
  let xdes =
    Array.init groups (fun g ->
        if g = 0 then template_page 0
        else
          let page = Bytes.make Page.size '\000' in
          Bytes.set_int32_be page 4 (Int32.of_int (g * group));
          Bytes.set_uint16_be page 24 9;
          Bytes.set_int32_be page 34 (Int32.of_int t.space);
          page)
  in
  let inode = template_page 2 in
  let entry k = (xdes.(k / 256), k / 256 * group, 150 + (40 * (k mod 256))) in
  let address page offset base at =
    Bytes.set_int32_be base at (Int32.of_int page);
    Bytes.set_uint16_be base (at + 4) offset
  in
  let node k =
    let _, page, offset = entry k in
    (page, offset + 8)
  in
  let link base at members =
    let members = Array.of_list members in
    let count = Array.length members in
    Bytes.set_int32_be base at (Int32.of_int count);
    let ends j = if j >= 0 && j < count then node members.(j) else (Page.null, 0) in
    let first_page, first_offset = ends 0 and last_page, last_offset = ends (count - 1) in
    address first_page first_offset base (at + 4);
    address last_page last_offset base (at + 10);
    Array.iteri
      (fun j k ->
        let bytes, _, offset = entry k in
        let prev_page, prev_offset = ends (j - 1) and next_page, next_offset = ends (j + 1) in
        address prev_page prev_offset bytes (offset + 8);
        address next_page next_offset bytes (offset + 14))
      members
  in
  let describe k ~segment ~state used =
    let bytes, _, offset = entry k in
    Bytes.set_int64_be bytes offset (Int64.of_int segment);
    Bytes.set_int32_be bytes (offset + 20) (Int32.of_int state);
    let bit i =
      let pair = if List.mem i used then 0b10 else 0b11 in
      let at = offset + 24 + (2 * i / 8) and shift = 2 * i mod 8 in
      Bytes.set_uint8 bytes at (Bytes.get_uint8 bytes at land lnot (0b11 lsl shift) lor (pair lsl shift));
      i + 1
    in
    let (_ : int) = Base.Fn.apply_n_times ~n:64 bit 0 in
    ()
  in
  let firsts =
    List.init groups (fun g ->
        let used = if g = 0 then [ 0; 1; 2; 3; 4 ] @ List.map (fun page -> page mod 64) frags else [ 0; 1 ] in
        (256 * g, used))
  in
  let free_frag = List.filter (fun (_, used) -> List.length used < 64) firsts in
  let full_frag = List.filter (fun (_, used) -> List.length used = 64) firsts in
  List.iter (fun (k, used) -> describe k ~segment:0 ~state:2 used) free_frag;
  List.iter (fun (k, used) -> describe k ~segment:0 ~state:3 used) full_frag;
  let segment_extents used ~segment ~inode_at =
    let empty = List.filter (fun (_, pages) -> pages = []) used in
    let full = List.filter (fun (_, pages) -> List.length pages = 64) used in
    let partial = List.filter (fun (_, pages) -> pages <> [] && List.length pages < 64) used in
    List.iter (fun (k, pages) -> describe k ~segment ~state:4 pages) (full @ partial);
    link inode (inode_at + 12) [];
    link inode (inode_at + 28) (List.map fst partial);
    link inode (inode_at + 44) (List.map fst full);
    Bytes.set_int32_be inode (inode_at + 8)
      (Int32.of_int (Base.List.sum (module Base.Int) partial ~f:(fun (_, pages) -> List.length pages)));
    List.map fst empty
  in
  let leaf_empty = segment_extents (List.sort compare leaf_used) ~segment:t.leaf_segment ~inode_at:t.leaf_inode in
  let top_empty = segment_extents (List.sort compare top_used) ~segment:t.top_segment ~inode_at:t.top_inode in
  let free = List.sort compare (leaf_empty @ top_empty) in
  List.iter (fun k -> describe k ~segment:0 ~state:1 []) free;
  let header = xdes.(0) in
  link header 62 free;
  link header 78 (List.map fst free_frag);
  link header 94 (List.map fst full_frag);
  Bytes.set_int32_be header 46 (Int32.of_int size);
  Bytes.set_int32_be header 50 (Int32.of_int (Int.max 64 size));
  Bytes.set_int32_be header 58
    (Int32.of_int (Base.List.sum (module Base.Int) free_frag ~f:(fun (_, used) -> List.length used)));
  List.iteri (fun j page -> Bytes.set_int32_be inode (t.top_inode + 64 + (4 * (j + 1))) (Int32.of_int page)) frags;
  let%bind () =
    Base.List.fold_result (List.init groups Fun.id) ~init:() ~f:(fun () g ->
        let page = xdes.(g) in
        stamp page;
        let%bind () = File.write t.file (g * group) page in
        if g > 0 then (
          let ibuf = template_page 1 in
          Bytes.set_int32_be ibuf 4 (Int32.of_int ((g * group) + 1));
          stamp ibuf;
          File.write t.file ((g * group) + 1) ibuf)
        else Ok ())
  in
  stamp inode;
  let%bind () = File.write t.file 2 inode in
  let%bind () = File.write t.file 1 (template_page 1) in
  let%bind () = File.write t.file 3 (template_page 3) in
  let%map () = File.resize t.file ~pages:size in
  size
