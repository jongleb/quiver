open Base.Result.Let_syntax

type error =
  | Size of int
  | Checksum of int
  | Number of int
  | Not_index of int
  | Level of int
  | Empty of int
  | Links of int
  | Records of int
  | Directory of int
  | Leftmost of int
  | Order of int
  | Pointer of int
  | Lob of int
  | Space of string
[@@deriving sexp_of ~portable]

type t = { pages : int; rows : int }

let group = 16384

(** Which list of the space or of a segment an extent belongs to. *)
type list_kind = Free | Free_frag | Full_frag | Segment_free | Not_full | Full

let tablespace (layout : Layout.t) ~pages ~read =
  let fail error = Error (`Check error) in
  let u8 bytes at = Bytes.get_uint8 bytes at in
  let u16 bytes at = Bytes.get_uint16_be bytes at in
  let u32 bytes at = Int32.to_int (Bytes.get_int32_be bytes at) land 0xFFFF_FFFF in
  let fields = Array.of_list layout.fields in
  let page n =
    if n < 0 || n >= pages then fail (Number n)
    else
      let bytes = read n in
      let stored = u32 bytes 0 in
      let%bind () =
        if Page.checksum bytes ~pos:0 = stored && u32 bytes (Page.size - 8) = stored then Ok ()
        else fail (Checksum n)
      in
      let%map () = if u32 bytes 4 = n then Ok () else fail (Number n) in
      bytes
  in
  let%bind header = page 0 in
  let size = u32 header 46 in
  let%bind () = if size = pages then Ok () else fail (Size (pages * Page.size)) in
  let%bind root = page layout.root in
  let used = Hashtbl.create 1024 in
  List.iter (fun n -> Hashtbl.replace used n ()) [ 0; 1; 2; 3 ];
  Base.Fn.apply_n_times ~n:((size + group - 1) / group)
    (fun g ->
      if g > 0 then (
        Hashtbl.replace used (g * group) ();
        Hashtbl.replace used ((g * group) + 1) ());
      g + 1)
    0
  |> ignore;
  let leaf_sizes bytes n origin =
    let rec go j lengths data =
      if j = Array.length fields then Ok (origin - 5 - (lengths + 1), data)
      else
        let field = fields.(j) in
        let null =
          match field.nullability with
          | Not_null -> 0
          | Null_bit bit -> u8 bytes (origin - 6 - (bit / 8)) land (1 lsl (bit mod 8))
        in
        if null <> 0 then go (j + 1) lengths data
        else
          match Layout.length field.storage with
          | Varying _ when lengths < 120 -> fail (Records n)
          | Varying { big } ->
              let first = u8 bytes lengths in
              if big && first land 0x80 <> 0 then
                let length = ((first land 0x3F) lsl 8) lor u8 bytes (lengths - 1) in
                if lengths < 121 || (first land 0x40 <> 0 && length <> 20) then fail (Records n)
                else go (j + 1) (lengths - 2) (data + length)
              else go (j + 1) (lengths - 1) (data + first)
          | Fixed size -> go (j + 1) lengths (data + size)
    in
    go 0 (origin - 6 - layout.null_bytes) 0
  in
  let key_span bytes n origin =
    let header = origin - 5 - layout.null_bytes in
    let rec go j at data =
      if j = layout.key then Ok (header - at, data)
      else
        match Layout.length fields.(j).storage with
        | Fixed size -> go (j + 1) at (data + size)
        | Varying _ when at <= 120 -> fail (Records n)
        | Varying { big } ->
            let first = u8 bytes (at - 1) in
            if big && first land 0x80 <> 0 then
              if first land 0x40 <> 0 || at <= 121 then fail (Records n)
              else go (j + 1) (at - 2) (data + (((first land 0x3F) lsl 8) lor u8 bytes (at - 2)))
            else go (j + 1) (at - 1) (data + first)
    in
    go 0 header 0
  in
  let key_at bytes n origin =
    let%map lengths, data = key_span bytes n origin in
    (Bytes.sub_string bytes (origin - 5 - layout.null_bytes - lengths) lengths, Bytes.sub_string bytes origin data)
  in
  let node_sizes bytes n origin =
    let rec zero k =
      if k = layout.null_bytes then
        let%map lengths, data = key_span bytes n origin in
        (layout.null_bytes + lengths, data + 4)
      else if u8 bytes (origin - 6 - k) = 0 then zero (k + 1)
      else fail (Records n)
    in
    zero 0
  in
  let records bytes n ~level ~leftmost =
    let count = u16 bytes 54 and top = u16 bytes 40 in
    let%bind () =
      if
        u16 bytes 42 = 0x8000 lor (2 + count)
        && top >= 120
        && top <= Page.size - 8 - (2 * u16 bytes 38)
        && Bytes.sub_string bytes 99 8 = "infimum\000"
        && Bytes.sub_string bytes 112 8 = "supremum"
        && u16 bytes 95 = 2
        && u16 bytes 110 = 0
        && u16 bytes 108 = (1 lsl 3) lor 3
      then Ok ()
      else fail (Records n)
    in
    let rec walk origin k stop acc =
      let next = (origin + u16 bytes (origin - 2)) land 0xFFFF in
      if next = 112 then if k = count && stop = top then Ok (List.rev acc) else fail (Records n)
      else if k >= count || next < 125 || next >= top then fail (Records n)
      else
        let status = u16 bytes (next - 4) in
        let info = u8 bytes (next - 5) lsr 4 in
        let expected = if level = 0 then 0 else 1 in
        let%bind () =
          if status lsr 3 = 2 + k && status land 7 = expected then Ok () else fail (Records n)
        in
        let%bind () =
          match (info, level > 0 && leftmost && k = 0) with
          | 0, false | 1, true -> Ok ()
          | _ -> fail (Leftmost n)
        in
        let%bind extra, data = if level = 0 then leaf_sizes bytes n next else node_sizes bytes n next in
        let start = next - 5 - extra and finish = next + data in
        if start < stop || finish > top then fail (Records n)
        else walk next (k + 1) finish (next :: acc)
    in
    walk 99 0 120 []
  in
  let directory bytes n origins =
    let slots = u16 bytes 38 in
    let slot s = u16 bytes (Page.size - 8 - (2 * (s + 1))) in
    let owned origin = u8 bytes (origin - 5) land 0x0F in
    let rec go list s pending =
      match list with
      | [] -> if s = slots then Ok () else fail (Directory n)
      | origin :: rest ->
          let pending = pending + 1 in
          if s < slots && slot s = origin then
            let low = if s = 0 then 1 else if s = slots - 1 then 1 else 4 in
            let high = if s = 0 then 1 else 8 in
            if owned origin = pending && pending >= low && pending <= high then go rest (s + 1) 0
            else fail (Directory n)
          else if owned origin = 0 then go rest s pending
          else fail (Directory n)
    in
    if slots < 2 then fail (Directory n) else go ((99 :: origins) @ [ 112 ]) 0 0
  in
  let references bytes origin =
    let rec go j lengths data acc =
      if j = Array.length fields then acc
      else
        let field = fields.(j) in
        let null =
          match field.nullability with
          | Not_null -> 0
          | Null_bit bit -> u8 bytes (origin - 6 - (bit / 8)) land (1 lsl (bit mod 8))
        in
        if null <> 0 then go (j + 1) lengths data acc
        else
          match Layout.length field.storage with
          | Varying { big } ->
              let first = u8 bytes lengths in
              if big && first land 0x80 <> 0 then
                let length = ((first land 0x3F) lsl 8) lor u8 bytes (lengths - 1) in
                let acc = if first land 0x40 <> 0 then (origin + data) :: acc else acc in
                go (j + 1) (lengths - 2) (data + length) acc
              else go (j + 1) (lengths - 1) (data + first) acc
          | Fixed size -> go (j + 1) lengths (data + size) acc
    in
    go 0 (origin - 6 - layout.null_bytes) 0 []
  in
  let lobs = Hashtbl.create 64 in
  let claim n =
    if Hashtbl.mem used n then fail (Lob n)
    else (
      Hashtbl.replace used n ();
      Hashtbl.replace lobs n ();
      Ok ())
  in
  let space = u32 root 34 in
  let lob bytes at n =
    let first_no = u32 bytes (at + 4) and length = u32 bytes (at + 16) in
    let%bind () =
      if u32 bytes at = space && u32 bytes (at + 8) = 1 && u32 bytes (at + 12) = 0 && length > 0 then Ok ()
      else fail (Lob n)
    in
    let%bind first = page first_no in
    let%bind () = if u16 first 24 = 24 && u8 first 38 = 0 && u32 first 40 = 1 then Ok () else fail (Lob first_no) in
    let%bind () = claim first_no in
    let rec chain number acc =
      if number = Page.null then Ok (List.rev acc)
      else
        let%bind bytes = page number in
        let%bind () = if u16 bytes 24 = 22 && u8 bytes 38 = 0 then Ok () else fail (Lob number) in
        let%bind () = claim number in
        chain (u32 bytes 12) ((number, bytes) :: acc)
    in
    let%bind index = chain (u32 first 12) [] in
    let node (number, offset) =
      if number = first_no && offset >= 96 && offset < 696 && (offset - 96) mod 60 = 0 then Ok first
      else
        match List.assoc_opt number index with
        | Some bytes when offset >= 39 && offset < 39 + (60 * 272) && (offset - 39) mod 60 = 0 -> Ok bytes
        | Some _ | None -> fail (Lob first_no)
    in
    let address bytes at = (u32 bytes at, u16 bytes (at + 4)) in
    let seen = Hashtbl.create 16 in
    let walk base ~entry =
      let rec go place prev k acc =
        if fst place = Page.null then
          if k = u32 first base && address first (base + 10) = prev then Ok acc else fail (Lob first_no)
        else if Hashtbl.mem seen place then fail (Lob first_no)
        else (
          Hashtbl.replace seen place ();
          let%bind bytes = node place in
          let offset = snd place in
          let%bind () = if address bytes offset = prev then Ok () else fail (Lob first_no) in
          let%bind acc = entry bytes offset k acc in
          go (address bytes (offset + 6)) place (k + 1) acc)
      in
      go (address first (base + 4)) (Page.null, 0) 0
    in
    let%bind total =
      walk 64 0 ~entry:(fun bytes offset k total ->
          let piece = u32 bytes (offset + 48) and size = u16 bytes (offset + 52) in
          let%bind () =
            if u32 bytes (offset + 12) = 0 && u32 bytes (offset + 56) = 1 && size > 0 then Ok () else fail (Lob first_no)
          in
          let%map () =
            if k = 0 then if piece = first_no && size = u32 first 54 && size <= 15680 then Ok () else fail (Lob first_no)
            else
              let%bind data = page piece in
              let%bind () =
                if u16 data 24 = 23 && u8 data 38 = 0 && u32 data 39 = size && size <= 16327 then Ok ()
                else fail (Lob piece)
              in
              claim piece
          in
          total + size)
    in
    let%bind (_ : int) = walk 80 0 ~entry:(fun _ _ _ total -> Ok total) in
    if total = length && Hashtbl.length seen = 10 + (272 * List.length index) then Ok () else fail (Lob first_no)
  in
  let tree = Hashtbl.create 1024 in
  let rec level height numbers expected last rows =
    let numbers = Array.of_list numbers in
    let%bind children, last, rows =
      Base.List.fold_result (List.init (Array.length numbers) Fun.id) ~init:([], last, rows)
        ~f:(fun (children, last, rows) j ->
          let n = numbers.(j) in
          let%bind bytes = if n = layout.root then Ok root else page n in
          let%bind () =
            if Hashtbl.mem tree n then fail (Links n)
            else (
              Hashtbl.replace tree n height;
              Hashtbl.replace used n ();
              Ok ())
          in
          let%bind () =
            if u16 bytes 24 = 17855 && Int64.to_int (Bytes.get_int64_be bytes 66) = layout.index then Ok ()
            else fail (Not_index n)
          in
          let%bind () = if u16 bytes 64 = height then Ok () else fail (Level n) in
          let prev = if j > 0 then numbers.(j - 1) else Page.null in
          let next = if j + 1 < Array.length numbers then numbers.(j + 1) else Page.null in
          let%bind () = if u32 bytes 8 = prev && u32 bytes 12 = next then Ok () else fail (Links n) in
          let%bind origins = records bytes n ~level:height ~leftmost:(j = 0) in
          let%bind () = directory bytes n origins in
          let%bind () =
            match (origins, height, Array.length numbers) with
            | [], 0, 1 when n = layout.root -> Ok ()
            | [], _, _ -> fail (Empty n)
            | _ -> Ok ()
          in
          let%bind keys = Base.Result.all (List.map (fun origin -> key_at bytes n origin) origins) in
          let%bind () =
            match (expected, keys) with
            | Some expected, first :: _ when expected.(j) <> first -> fail (Pointer n)
            | _ -> Ok ()
          in
          let%bind last =
            Base.List.fold_result keys ~init:last ~f:(fun last key ->
                match last with
                | Some (_, previous) when layout.order = Bytes_order && String.compare previous (snd key) >= 0 ->
                    fail (Order n)
                | _ -> Ok (Some key))
          in
          let children =
            if height = 0 then children
            else
              List.rev_append
                (List.map2 (fun origin ((_, data) as key) -> (u32 bytes (origin + String.length data), key)) origins keys)
                children
          in
          let rows = if height = 0 then rows + List.length origins else rows in
          let%bind () =
            if height > 0 then Ok ()
            else
              Base.List.fold_result origins ~init:() ~f:(fun () origin ->
                  Base.List.fold_result (references bytes origin) ~init:() ~f:(fun () at -> lob bytes at n))
          in
          Ok (children, last, rows))
    in
    if height = 0 then Ok rows
    else
      let children = List.rev children in
      let expected = Array.of_list (List.map snd children) in
      let%bind () =
        Base.List.fold_result children ~init:() ~f:(fun () (child, _) ->
            if child < pages then Ok () else fail (Pointer child))
      in
      ignore last;
      level (height - 1) (List.map fst children) (Some expected) None rows
  in
  let%bind rows = level (u16 root 64) [ layout.root ] None None 0 in
  let%bind inode = page 2 in
  let inode_of offset =
    if u32 root (offset + 4) = 2 then Ok (u16 root (offset + 8))
    else fail (Space "a segment's inode is not on page 2")
  in
  let%bind leaf_inode = inode_of 74 in
  let%bind top_inode = inode_of 84 in
  let segment at = Int64.to_int (Bytes.get_int64_be inode at) in
  let extents = (size + 63) / 64 in
  let%bind xdes =
    Base.Result.all (List.init ((size + group - 1) / group) (fun g -> if g = 0 then Ok header else page (g * group)))
  in
  let xdes = Array.of_list xdes in
  let entry k = (xdes.(k / 256), 150 + (40 * (k mod 256))) in
  let state k =
    let bytes, offset = entry k in
    u32 bytes (offset + 20)
  in
  let owner k =
    let bytes, offset = entry k in
    Int64.to_int (Bytes.get_int64_be bytes offset)
  in
  let page_used k i =
    let bytes, offset = entry k in
    u8 bytes (offset + 24 + (2 * i / 8)) land (1 lsl (2 * i mod 8)) = 0
  in
  let count k = Base.List.count (List.init 64 Fun.id) ~f:(page_used k) in
  let%bind () =
    Base.List.fold_result (List.init (extents * 64) Fun.id) ~init:() ~f:(fun () p ->
        if page_used (p / 64) (p mod 64) = Hashtbl.mem used p then Ok ()
        else
          let marked = if Hashtbl.mem used p then "free" else "used" in
          fail (Space (Printf.sprintf "page %d is %s by its extent" p marked)))
  in
  let frag_arr at = List.filter (fun n -> n <> Page.null) (List.init 32 (fun j -> u32 inode (at + 64 + (4 * j)))) in
  let leaf_frags = frag_arr leaf_inode and top_frags = frag_arr top_inode in
  let%bind () =
    Base.List.fold_result (List.of_seq (Hashtbl.to_seq tree)) ~init:() ~f:(fun () (n, height) ->
        let segment, frags =
          if height = 0 && n <> layout.root then (segment leaf_inode, leaf_frags) else (segment top_inode, top_frags)
        in
        match state (n / 64) with
        | 4 when owner (n / 64) = segment -> Ok ()
        | (2 | 3) when List.mem n frags -> Ok ()
        | _ -> fail (Space (Printf.sprintf "page %d is not in its segment" n)))
  in
  let%bind () =
    Base.List.fold_result (List.of_seq (Hashtbl.to_seq_keys lobs)) ~init:() ~f:(fun () n ->
        match state (n / 64) with
        | 4 when owner (n / 64) = segment leaf_inode -> Ok ()
        | (2 | 3) when List.mem n leaf_frags -> Ok ()
        | _ -> fail (Space (Printf.sprintf "page %d is not in its segment" n)))
  in
  let listed = Hashtbl.create 64 in
  let walk bytes at kind ~segment =
    let length = u32 bytes at in
    let address base = (u32 base 0, u16 base 4) in
    let read_address bytes at = address (Bytes.sub bytes at 6) in
    let rec go node prev k =
      match node with
      | page_no, _ when page_no = Page.null ->
          if k = length && read_address bytes (at + 10) = prev then Ok () else fail (Space "a list's length or end")
      | page_no, offset ->
          let k_extent = (page_no / group * 256) + ((offset - 8 - 150) / 40) in
          let%bind () =
            if page_no mod group <> 0 || (offset - 158) mod 40 <> 0 || k_extent >= extents || Hashtbl.mem listed k_extent
            then fail (Space "a list names a wrong or repeated extent")
            else Ok ()
          in
          Hashtbl.replace listed k_extent kind;
          let entry_bytes, entry_offset = entry k_extent in
          let%bind () =
            if read_address entry_bytes (entry_offset + 8) = prev then Ok () else fail (Space "a list's back link")
          in
          let used = count k_extent in
          let%bind () =
            match (kind, state k_extent, used) with
            | Free, 1, 0 -> Ok ()
            | Free_frag, 2, u when u < 64 -> Ok ()
            | Full_frag, 3, 64 -> Ok ()
            | Segment_free, 4, 0 | Full, 4, 64 -> if owner k_extent = segment then Ok () else fail (Space "an extent of another segment")
            | Not_full, 4, u when u > 0 && u < 64 ->
                if owner k_extent = segment then Ok () else fail (Space "an extent of another segment")
            | _ -> fail (Space (Printf.sprintf "extent %d in the wrong list" k_extent))
          in
          go (read_address entry_bytes (entry_offset + 14)) (page_no, offset) (k + 1)
    in
    go (read_address bytes (at + 4)) (Page.null, 0) 0
  in
  let%bind () = walk header 62 Free ~segment:0 in
  let%bind () = walk header 78 Free_frag ~segment:0 in
  let%bind () = walk header 94 Full_frag ~segment:0 in
  let%bind () =
    Base.List.fold_result [ leaf_inode; top_inode ] ~init:() ~f:(fun () at ->
        let segment = segment at in
        let%bind () = walk inode (at + 12) Segment_free ~segment in
        let%bind () = walk inode (at + 28) Not_full ~segment in
        let%bind () = walk inode (at + 44) Full ~segment in
        let not_full =
          Base.List.sum (module Base.Int) (List.of_seq (Hashtbl.to_seq listed)) ~f:(fun (k, kind) ->
              match kind with Not_full when owner k = segment -> count k | _ -> 0)
        in
        if u32 inode (at + 8) = not_full then Ok () else fail (Space "a segment's count of used pages"))
  in
  let%bind () =
    if Hashtbl.length listed = extents then Ok () else fail (Space "an extent in no list")
  in
  let frag_used =
    Base.List.sum (module Base.Int) (List.of_seq (Hashtbl.to_seq listed)) ~f:(fun (k, kind) ->
        match kind with Free_frag -> count k | _ -> 0)
  in
  let%bind () = if u32 header 58 = frag_used then Ok () else fail (Space "the space's count of fragment pages") in
  Ok { pages; rows }
