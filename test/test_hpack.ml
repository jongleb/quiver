open Sexplib0.Sexp_conv
open Quiver_tikv

let lines file = String.split_on_char '\n' (In_channel.with_open_bin file In_channel.input_all)

let static =
  Array.of_list
    (List.filter_map
       (fun line -> Option.map (fun i -> (String.sub line 0 i, String.sub line (i + 1) (String.length line - i - 1))) (String.index_opt line '\t'))
       (lines "http2/static.txt"))

let codes =
  Array.of_list
    (List.filter_map
       (fun line -> Option.map (fun i -> String.sub line (i + 1) (String.length line - i - 1)) (String.index_opt line ' '))
       (lines "http2/huffman.txt"))

let huffman text =
  let bits = String.concat "" (List.map (fun c -> codes.(Char.code c)) (List.of_seq (String.to_seq text))) in
  let bits = bits ^ String.make ((8 - (String.length bits mod 8)) mod 8) '1' in
  String.init (String.length bits / 8) (fun i ->
      Char.chr (Base.String.fold (String.sub bits (i * 8) 8) ~init:0 ~f:(fun acc bit -> (2 * acc) + Char.code bit - Char.code '0')))

let integer buffer ~flags ~prefix n =
  let mask = (1 lsl prefix) - 1 in
  if n < mask then Buffer.add_char buffer (Char.chr (flags lor n))
  else (
    Buffer.add_char buffer (Char.chr (flags lor mask));
    let rec rest n =
      if n < 128 then Buffer.add_char buffer (Char.chr n)
      else (
        Buffer.add_char buffer (Char.chr ((n mod 128) + 128));
        rest (n / 128))
    in
    rest (n - mask))

let header_size (name, value) = String.length name + String.length value + 32

(* The encoder's model of the decoder's table, newest first. *)
type table = { mutable max : int; mutable entries : (string * string) array }

let evict table room =
  let rec keep n total =
    if n = Array.length table.entries || total + header_size table.entries.(n) > room then n
    else keep (n + 1) (total + header_size table.entries.(n))
  in
  table.entries <- Array.sub table.entries 0 (keep 0 0)

type kind = Indexed | Incremental | Without | Never [@@deriving sexp_of]

type name_from = By_index | Literal_name [@@deriving sexp_of]
type coding = Huffman | Raw [@@deriving sexp_of]

type choice = { header : string * string; kind : kind; name_from : name_from; name_coding : coding; value_coding : coding }
[@@deriving sexp_of]

type block = { updates : int list; fields : choice list } [@@deriving sexp_of]

let encode table block =
  let buffer = Buffer.create 64 in
  List.iter
    (fun size ->
      integer buffer ~flags:0x20 ~prefix:5 size;
      table.max <- size;
      evict table size)
    block.updates;
  let lookup matches =
    match Base.Array.findi static ~f:(fun _ entry -> matches entry) with
    | Some (i, _) -> Some (i + 1)
    | None -> Option.map (fun (i, _) -> i + 62) (Base.Array.findi table.entries ~f:(fun _ entry -> matches entry))
  in
  let literal coding text =
    let bytes, flags = match coding with Huffman -> (huffman text, 0x80) | Raw -> (text, 0) in
    integer buffer ~flags ~prefix:7 (String.length bytes);
    Buffer.add_string buffer bytes
  in
  List.iter
    (fun choice ->
      let name, value = choice.header in
      match (choice.kind, lookup (fun entry -> entry = choice.header)) with
      | Indexed, Some index -> integer buffer ~flags:0x80 ~prefix:7 index
      | (Indexed | Incremental | Without | Never), _ ->
          let flags, prefix =
            match choice.kind with Indexed | Incremental -> (0x40, 6) | Without -> (0, 4) | Never -> (0x10, 4)
          in
          let () =
            match (choice.name_from, lookup (fun (other, _) -> other = name)) with
            | By_index, Some index -> integer buffer ~flags ~prefix index
            | (By_index | Literal_name), _ ->
                integer buffer ~flags ~prefix 0;
                literal choice.name_coding name
          in
          literal choice.value_coding value;
          let () =
            match choice.kind with
            | Indexed | Incremental ->
                if header_size choice.header > table.max then table.entries <- [||]
                else (
                  evict table (table.max - header_size choice.header);
                  table.entries <- Array.append [| choice.header |] table.entries)
            | Without | Never -> ()
          in
          ())
    block.fields;
  Buffer.contents buffer

let gen =
  QCheck2.Gen.(
    let text =
      oneof_weighted
        [
          (4, string_size ~gen:char (int_range 0 20));
          (1, string_size ~gen:char (int_range 120 300));
          (2, oneof_list [ ""; "GET"; "200"; "application/grpc"; "trailers"; "0" ]);
        ]
    in
    let name = oneof_weighted [ (3, oneof_list [ ":method"; ":status"; "content-type"; "grpc-status"; "x-a"; "te" ]); (1, text) ] in
    let choice =
      let* header = pair name text
      and* kind = oneof_list [ Indexed; Incremental; Without; Never ]
      and* name_from = oneof_list [ By_index; Literal_name ]
      and* name_coding = oneof_list [ Huffman; Raw ]
      and* value_coding = oneof_list [ Huffman; Raw ] in
      return { header; kind; name_from; name_coding; value_coding }
    in
    let* limit = oneof_list [ 0; 64; 200; 512; 4096 ] in
    let block = pair (list_size (int_range 0 2) (int_range 0 limit)) (list_size (int_range 0 12) choice) in
    let* blocks = list_size (int_range 1 6) (map (fun (updates, fields) -> { updates; fields }) block) in
    return (limit, blocks))

let decoded =
  QCheck2.Test.make ~count:2000
    ~name:
      "header blocks from an encoder making every choice HPACK has (indexes, indexing kinds, Huffman, \
       table size updates, evictions) decode to the encoded fields, block after block"
    ~print:(fun (limit, blocks) -> Printf.sprintf "limit=%d %s" limit (Base.Sexp.to_string ([%sexp_of: block list] blocks)))
    gen
    (fun (limit, blocks) ->
      let decoder = Hpack.decoder ~limit and table = { max = limit; entries = [||] } in
      List.for_all
        (fun block ->
          let indexed index =
            let buffer = Buffer.create 8 in
            integer buffer ~flags:0x80 ~prefix:7 index;
            Hpack.decode decoder (Buffer.contents buffer)
          in
          let encoded = encode table block in
          let size = Array.length table.entries in
          match Hpack.decode decoder encoded with
          | Ok fields ->
              fields = List.map (fun choice -> choice.header) block.fields
              && indexed (62 + size) = Error (`Hpack (Index (62 + size)))
              && (size = 0 || indexed (61 + size) = Ok [ table.entries.(size - 1) ])
          | Error (`Hpack error) -> QCheck2.Test.fail_reportf "%a" Base.Sexp.pp (Hpack.sexp_of_error error))
        blocks)

let ours =
  QCheck2.Test.make ~count:1000 ~name:"our own header blocks decode to the fields and leave the table empty"
    QCheck2.Gen.(list_size (int_range 0 10) (pair (string_size ~gen:char (int_range 0 200)) (string_size ~gen:char (int_range 0 300))))
    (fun fields ->
      let buffer = Buffer.create 64 in
      Hpack.encode buffer fields;
      let decoder = Hpack.decoder ~limit:4096 in
      Hpack.decode decoder (Buffer.contents buffer) = Ok fields && Hpack.decode decoder "\xbe" = Error (`Hpack (Index 62)))

let fuzz =
  QCheck2.Test.make ~count:5000 ~name:"any bytes decode or fail without an exception"
    QCheck2.Gen.(string_size ~gen:char (int_range 0 64))
    (fun text ->
      let decoder = Hpack.decoder ~limit:256 in
      ignore (Hpack.decode decoder text);
      ignore (Hpack.decode decoder text);
      true)

let examples () =
  let rec go decoder = function
    | [] -> ()
    | line :: rest when String.starts_with ~prefix:"example " line -> (
        match String.split_on_char ' ' line with
        | [ _; limit; order; hex ] ->
            let decoder = if order = "first" then Hpack.decoder ~limit:(int_of_string limit) else decoder in
            let rec fields acc = function
              | "end" :: rest -> (List.rev acc, rest)
              | field :: rest ->
                  let i = String.index field '\t' in
                  fields ((String.sub field 0 i, String.sub field (i + 1) (String.length field - i - 1)) :: acc) rest
              | [] -> (List.rev acc, [])
            in
            let expected, rest = fields [] rest in
            let bytes = String.init (String.length hex / 2) (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (2 * i) 2))) in
            Alcotest.(check (result (list (pair string string)) reject)) hex (Ok expected)
              (Result.map_error ignore (Hpack.decode decoder bytes));
            go decoder rest
        | _ -> Alcotest.fail line)
    | _ :: rest -> go decoder rest
  in
  go (Hpack.decoder ~limit:4096) (lines "http2/examples.txt")

let refused () =
  let check name expected text =
    Alcotest.(check bool) name true (Hpack.decode (Hpack.decoder ~limit:256) text = Error (`Hpack expected))
  in
  check "EOS inside a string" Huffman "\x00\x84\xff\xff\xff\xff";
  check "eight bits of padding" Huffman "\x00\x82\x1f\xff\x00";
  check "padding with a zero bit" Huffman "\x00\x81\x1e\x00";
  check "index past the table" (Index 62) "\xbe";
  check "index 0" (Index 0) "\x80";
  check "size update above the limit" (Table_size 257) "\x3f\xe2\x01";
  check "size update after a field" Late_size_update "\x82\x20";
  check "string past the block" Truncated "\x00\x05ab";
  check "integer past 2^28" Integer_overflow "\xff\xff\xff\xff\xff\xff\x01"

let tests =
  [
    Alcotest.test_case "RFC 7541 examples" `Quick examples;
    Alcotest.test_case "broken blocks refused" `Quick refused;
  ]
  @ List.map QCheck_alcotest.to_alcotest [ decoded; ours; fuzz ]
