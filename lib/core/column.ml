type error =
  | Out_of_range of { ltype : Ltype.t; value : int }
  | Inexact_float32 of float
  | Value_too_large of int
[@@deriving sexp_of, equal]

type _ repr =
  | Bool : bool repr
  | Int : int repr
  | Int64 : int64 repr
  | Float : float repr
  | Text : string repr

type presence = Null | Present [@@deriving sexp_of, equal]

type buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Bytes per slot of a narrow integer column. *)
type width = W1 | W2 | W4

(** Physical layout of the data buffer, tied to the value type. *)
type _ layout =
  | Bits : bool layout
  | Ints : { width : width; lo : int; hi : int } -> int layout
  | Int64s : int64 layout
  | Float32s : float layout
  | Float64s : float layout
  | Var : var -> string layout
      (** Data buffer holds [length + 1] int32 offsets into the heap. *)

and var = { mutable heap : Bytes.t; mutable used : int }
(** Variable-width bytes and how much of them is in use. *)

type 'a t = {
  ltype : Ltype.t @@ global;
  layout : 'a layout @@ global;
  mutable length : int;
  mutable validity : buffer;
  mutable data : buffer;
}

type any = Any : 'a t -> any

let buffer size = Bigarray.Array1.create Bigarray.char Bigarray.c_layout size

(* Columns are recycled between batches and keep their buffers, so growth stops
   after the first batches of a dump: the checker trusts the two growth paths
   and checks every other one. *)
let[@inline never] [@zero_alloc assume] grown (buf : buffer) needed =
  let size = Bigarray.Array1.dim buf in
  let bigger = buffer (Int.max needed (2 * size)) in
  Base_bigstring.blit ~src:buf ~src_pos:0 ~dst:bigger ~dst_pos:0 ~len:size;
  bigger

let[@inline] ensure_data t needed =
  if needed > Bigarray.Array1.dim t.data then t.data <- grown t.data needed

let[@inline] ensure_validity t needed =
  if needed > Bigarray.Array1.dim t.validity then
    t.validity <- grown t.validity needed

let[@inline] set_bit (buf : buffer) i =
  let byte = i lsr 3 in
  Bigarray.Array1.set buf byte
    (Char.unsafe_chr
       (Char.code (Bigarray.Array1.get buf byte) lor (1 lsl (i land 7))))

let[@inline] clear_bit (buf : buffer) i =
  let byte = i lsr 3 in
  Bigarray.Array1.set buf byte
    (Char.unsafe_chr
       (Char.code (Bigarray.Array1.get buf byte) land lnot (1 lsl (i land 7))))

let[@inline] test_bit (buf : buffer) i =
  Char.code (Bigarray.Array1.get buf (i lsr 3)) land (1 lsl (i land 7)) <> 0

let build ltype ~validity ~data ~heap =
  let make : type a. a layout -> a t =
   fun layout ->
    let () =
      match layout with
      | Var _ -> Base_bigstring.set_int32_le_exn data ~pos:0 0
      | Bits | Ints _ | Int64s | Float32s | Float64s -> ()
    in
    { ltype; layout; length = 0; validity; data }
  in
  let ints width lo hi = Any (make (Ints { width; lo; hi })) in
  match (ltype : Ltype.t) with
  | Bool -> Any (make Bits)
  | Int8 -> ints W1 (-0x80) 0x7F
  | Uint8 -> ints W1 0 0xFF
  | Int16 -> ints W2 (-0x8000) 0x7FFF
  | Uint16 -> ints W2 0 0xFFFF
  | Year -> ints W2 0 Moment.max_year
  | Unchecked_date -> ints W4 0 Temporal.max_packed_date
  | Int32 | Date ->
      ints W4 (Int32.to_int Int32.min_int) (Int32.to_int Int32.max_int)
  | Uint32 -> ints W4 0 0xFFFF_FFFF
  | Int64 | Uint64 | Time _ | Datetime _ | Timestamp _ | Unchecked_datetime _ ->
      Any (make Int64s)
  | Float32 -> Any (make Float32s)
  | Float64 -> Any (make Float64s)
  | Decimal _ | String | Bytes | Json | Geometry | Enum _ | Set _ ->
      Any (make (Var { heap = heap (); used = 0 }))

let create ltype =
  build ltype ~validity:(buffer 8) ~data:(buffer 64) ~heap:(fun () ->
      Bytes.create 256)

let recycle (Any old) ltype =
  build ltype ~validity:old.validity ~data:old.data ~heap:(fun () ->
      match old.layout with
      | Var { heap; _ } -> heap
      | Bits | Ints _ | Int64s | Float32s | Float64s -> Bytes.create 256)

let repr : type a. a t @ local -> a repr =
 fun t ->
  match t.layout with
  | Bits -> Bool
  | Ints _ -> Int
  | Int64s -> Int64
  | Float32s -> Float
  | Float64s -> Float
  | Var _ -> Text

let ltype t = t.ltype
let length t = t.length

let data_bytes : type a. a layout -> int -> int =
 fun layout n ->
  match layout with
  | Bits -> (n + 7) / 8
  | Ints { width = W1; _ } -> n
  | Ints { width = W2; _ } -> 2 * n
  | Ints { width = W4; _ } | Float32s -> 4 * n
  | Int64s | Float64s -> 8 * n
  | Var _ -> 4 * (n + 1)

let byte_size : type a. a t @ local -> int =
 fun t ->
  let n = t.length in
  ((n + 7) / 8)
  + data_bytes t.layout n
  +
  match t.layout with
  | Var { used; _ } -> used
  | Bits | Ints _ | Int64s | Float32s | Float64s -> 0

let[@inline] advance t presence =
  let i = t.length in
  ensure_validity t ((i / 8) + 1);
  let () =
    match presence with
    | Present -> set_bit t.validity i
    | Null -> clear_bit t.validity i
  in
  t.length <- i + 1

let[@inline] write_int t width i v =
  match width with
  | W1 ->
      ensure_data t (i + 1);
      Bigarray.Array1.set t.data i (Char.unsafe_chr (v land 0xFF))
  | W2 ->
      ensure_data t (2 * (i + 1));
      Base_bigstring.set_uint16_le_exn t.data ~pos:(2 * i) (v land 0xFFFF)
  | W4 ->
      ensure_data t (4 * (i + 1));
      Base_bigstring.unsafe_set_int32_le t.data ~pos:(4 * i) v

let[@inline] write_int64 t i v =
  ensure_data t (8 * (i + 1));
  Base_bigstring.set_int64_t_le t.data ~pos:(8 * i) (Int64_u.to_int64 v)

let[@inline] write_float32 t i v =
  ensure_data t (4 * (i + 1));
  Base_bigstring.set_int32_t_le t.data ~pos:(4 * i) (Int32.bits_of_float v)

let[@inline never] [@zero_alloc assume] grow_heap (var : var) next =
  let grown = Bytes.create (Int.max next (2 * Bytes.length var.heap)) in
  Bytes.blit var.heap 0 grown 0 var.used;
  var.heap <- grown

(* An error value is built only once per failed dump: the checker treats the
   paths returning it like raising ones. *)
let[@inline never] [@zero_alloc assume error] refuse error =
  Error (`Column error)

let[@inline] claim t (var : var) len =
  let start = var.used in
  let next = start + len in
  if next > Int32.to_int Int32.max_int then refuse (Value_too_large next)
  else (
    if next > Bytes.length var.heap then grow_heap var next;
    var.used <- next;
    ensure_data t (4 * (t.length + 2));
    Base_bigstring.set_int32_le_exn t.data ~pos:(4 * (t.length + 1)) next;
    Ok ())

let[@inline][@zero_alloc] add_int64 (t : int64 t @ local) v =
  match t.layout with
  | Int64s ->
      write_int64 t t.length v;
      advance t Present;
      Ok ()

let[@inline][@zero_alloc] add_float (t : float t @ local) v =
  match t.layout with
  | Float32s ->
      let v = Float_u.to_float v in
      if Float.equal (Int32.float_of_bits (Int32.bits_of_float v)) v then (
        write_float32 t t.length v;
        advance t Present;
        Ok ())
      else refuse (Inexact_float32 v)
  | Float64s ->
      write_int64 t t.length (Int64_u.bits_of_float v);
      advance t Present;
      Ok ()

let[@zero_alloc] add : type a. a t @ local -> a -> (unit, [> `Column of error ]) result
    =
 fun t v ->
  let i = t.length in
  match t.layout with
  | Bits ->
      ensure_data t ((i / 8) + 1);
      if v then set_bit t.data i else clear_bit t.data i;
      advance t Present;
      Ok ()
  | Ints { width; lo; hi } ->
      let v : int = v in
      if v < lo || v > hi then
        refuse (Out_of_range { ltype = t.ltype; value = v })
      else (
        write_int t width i v;
        advance t Present;
        Ok ())
  | Int64s -> add_int64 t (Int64_u.of_int64 v)
  | Float32s -> add_float t (Float_u.of_float v)
  | Float64s -> add_float t (Float_u.of_float v)
  | Var var -> (
      let start = var.used in
      match claim t var (String.length v) with
      | Ok () ->
          Bytes.blit_string v 0 var.heap start (String.length v);
          advance t Present;
          Ok ()
      | Error _ as error -> error)

let[@zero_alloc] add_bigstring (t : string t @ local) src ~off ~len =
  match t.layout with
  | Var var -> (
      let start = var.used in
      match claim t var len with
      | Ok () ->
          Base_bigstring.To_bytes.blit ~src ~src_pos:off ~dst:var.heap
            ~dst_pos:start ~len;
          advance t Present;
          Ok ()
      | Error _ as error -> error)

let[@zero_alloc] add_null : type a. a t @ local -> unit =
 fun t ->
  let i = t.length in
  let () =
    match t.layout with
    | Bits ->
        ensure_data t ((i / 8) + 1);
        clear_bit t.data i
    | Ints { width; _ } -> write_int t width i 0
    | Int64s | Float64s -> write_int64 t i #0L
    | Float32s -> write_float32 t i 0.
    | Var { used; _ } ->
        ensure_data t (4 * (i + 2));
        Base_bigstring.set_int32_le_exn t.data ~pos:(4 * (i + 1)) used
  in
  advance t Null

let[@inline] presence t i = if test_bit t.validity i then Present else Null

let[@inline] get_int64 (t : int64 t @ local) i =
  match t.layout with
  | Int64s ->
      Int64_u.of_int64 (Base_bigstring.get_int64_t_le t.data ~pos:(8 * i))

let[@inline] get_float (t : float t @ local) i =
  match t.layout with
  | Float32s ->
      Float_u.of_float
        (Int32.float_of_bits
           (Base_bigstring.get_int32_t_le t.data ~pos:(4 * i)))
  | Float64s ->
      Float_u.of_float
        (Int64.float_of_bits
           (Base_bigstring.get_int64_t_le t.data ~pos:(8 * i)))

let get : type a. a t @ local -> int -> a =
 fun t i ->
  match t.layout with
  | Bits -> test_bit t.data i
  | Ints { width = W1; lo; _ } ->
      let v = Char.code (Bigarray.Array1.get t.data i) in
      if lo < 0 then (v lxor 0x80) - 0x80 else v
  | Ints { width = W2; lo; _ } ->
      let v = Base_bigstring.get_uint16_le t.data ~pos:(2 * i) in
      if lo < 0 then (v lxor 0x8000) - 0x8000 else v
  | Ints { width = W4; lo; _ } ->
      let v = Base_bigstring.get_int32_le t.data ~pos:(4 * i) in
      if lo < 0 then v else v land 0xFFFF_FFFF
  | Int64s -> Int64_u.to_int64 (get_int64 t i)
  | Float32s -> Float_u.to_float (get_float t i)
  | Float64s -> Float_u.to_float (get_float t i)
  | Var { heap; _ } ->
      let start = Base_bigstring.get_int32_le t.data ~pos:(4 * i) in
      Bytes.sub_string heap start
        (Base_bigstring.get_int32_le t.data ~pos:(4 * (i + 1)) - start)

let[@inline] text_length (t : string t @ local) i =
  Base_bigstring.get_int32_le t.data ~pos:(4 * (i + 1))
  - Base_bigstring.get_int32_le t.data ~pos:(4 * i)

let text_bytes (t : string t @ local) = match t.layout with Var { heap; _ } -> heap

let text_offset (t : string t @ local) i =
  Base_bigstring.get_int32_le t.data ~pos:(4 * i)

let clear : type a. a t @ local -> unit =
 fun t ->
  t.length <- 0;
  match t.layout with
  | Var v -> v.used <- 0
  | Bits | Ints _ | Int64s | Float32s | Float64s -> ()

let encode : type a. a t @ local -> Byte_buffer.t @ local -> unit =
 fun t out ->
  let append (source : buffer) length =
    Byte_buffer.ensure_cap out (out.len + length);
    Base_bigstring.To_bytes.blit ~src:source ~src_pos:0 ~dst:out.bs
      ~dst_pos:out.len ~len:length;
    out.len <- out.len + length
  in
  append t.validity ((t.length + 7) / 8);
  append t.data (data_bytes t.layout t.length);
  match t.layout with
  | Var { heap; used } -> Byte_buffer.append_subbytes out heap 0 used
  | Bits | Ints _ | Int64s | Float32s | Float64s -> ()

let decode : type a.
    a t @ local -> Bytes.t -> off:int -> stop:int -> rows:int -> int option =
 fun t source ~off ~stop ~rows ->
  let validity = (rows + 7) / 8 and data = data_bytes t.layout rows in
  if rows < 0 || off + validity + data > stop then None
  else (
    clear t;
    ensure_validity t validity;
    ensure_data t data;
    Base_bigstring.From_bytes.blit ~src:source ~src_pos:off ~dst:t.validity
      ~dst_pos:0 ~len:validity;
    Base_bigstring.From_bytes.blit ~src:source ~src_pos:(off + validity)
      ~dst:t.data ~dst_pos:0 ~len:data;
    let next = off + validity + data in
    match t.layout with
    | Bits | Ints _ | Int64s | Float32s | Float64s ->
        t.length <- rows;
        Some next
    | Var var ->
        let offset i = Base_bigstring.get_int32_le t.data ~pos:(4 * i) in
        let rec monotonic i =
          i > rows || (offset i >= offset (i - 1) && monotonic (i + 1))
        in
        let used = offset rows in
        if offset 0 = 0 && monotonic 1 && next + used <= stop then (
          if used > Bytes.length var.heap then
            var.heap <-
              Bytes.create (Int.max used (2 * Bytes.length var.heap));
          Bytes.blit source next var.heap 0 used;
          var.used <- used;
          t.length <- rows;
          Some (next + used))
        else None)
