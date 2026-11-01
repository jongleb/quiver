open Sexplib0.Sexp_conv
open Quiver_load

let gen =
  QCheck2.Gen.(
    let* dialect = oneof_list [ Quiver_core.Output_format.Rfc4180; Mysql_load_data ] in
    let value =
      map (String.concat "") (list_size (int_range 0 5) (oneof_list [ "a"; ","; "\""; "\n"; "\r\n"; "\\"; " " ]))
    in
    let field text =
      match dialect with
      | Rfc4180 -> "\"" ^ Base.String.substr_replace_all text ~pattern:"\"" ~with_:"\"\"" ^ "\""
      | Mysql_load_data ->
          "\""
          ^ String.concat ""
              (List.map
                 (function '\n' -> "\\n" | '\r' -> "\\r" | '\\' -> "\\\\" | '"' -> "\\\"" | c -> String.make 1 c)
                 (List.of_seq (String.to_seq text)))
          ^ "\""
    in
    let newline = match dialect with Rfc4180 -> "\r\n" | Mysql_load_data -> "\n" in
    let row = map (fun values -> String.concat "," (List.map field values) ^ newline) (list_size (int_range 1 3) value) in
    let* rows = list_size (int_range 0 30) row in
    let* bytes = int_range 1 120 and* skip = int_range 0 (List.length rows) and* reads = list_size (int_range 1 8) (int_range 1 40) in
    let header = match dialect with Rfc4180 -> "\"x\",\"y,\"\"z\"\"\"\r\n" | Mysql_load_data -> "" in
    return (dialect, header, rows, bytes, skip, reads))

let cut_whole =
  QCheck2.Test.make ~count:3000
    ~name:
      "a data file is cut into whole rows after its header, each piece at least the size asked \
       but the last, the rows after those skipped each once in order"
    ~print:(fun (_, header, rows, bytes, skip, reads) ->
      Printf.sprintf "header=%S rows=%s bytes=%d skip=%d reads=%s" header
        (String.concat "|" (List.map (Printf.sprintf "%S") rows))
        bytes skip
        (String.concat "," (List.map string_of_int reads)))
    gen
    (fun (dialect, header, rows, bytes, skip, reads) ->
      let text = header ^ String.concat "" rows in
      let pos = ref 0 and turn = ref 0 in
      let read buffer ~pos:at ~len =
        let size = List.nth reads (!turn mod List.length reads) in
        incr turn;
        let n = Int.min (Int.min len size) (String.length text - !pos) in
        Bytes.blit_string text !pos buffer at n;
        pos := !pos + n;
        Ok n
      in
      let pieces = ref [] in
      let () =
        match Pieces.cut dialect ~bytes ~skip read (fun piece -> Ok (pieces := piece :: !pieces)) with
        | Ok () -> ()
        | Error () -> ()
      in
      let pieces = List.rev !pieces in
      let rec matched rows pieces =
        match pieces with
        | [] -> rows = []
        | ({ data; rows = count } : Pieces.piece) :: rest ->
            count > 0
            && List.length rows >= count
            && String.equal data (header ^ String.concat "" (List.filteri (fun i _ -> i < count) rows))
            && (rest = []
               || String.length data - String.length header >= bytes
                  && String.length (String.concat "" (List.filteri (fun i _ -> i < count - 1) rows)) < bytes)
            && matched (List.filteri (fun i _ -> i >= count) rows) rest
      in
      matched (List.filteri (fun i _ -> i >= skip) rows) pieces)

(** Where a run of the load breaks. *)
type crash =
  | Running  (** A statement or a piece fails; the transaction rolls back. *)
  | Before_commit  (** The commit fails; nothing of the piece is kept. *)
  | After_commit  (** The commit is applied, its answer lost. *)
[@@deriving sexp_of]

(** The target: what is committed with the load's mark in the same transaction. *)
type server = {
  mutable kept : string list;
  mutable open_ : string list;
  mutable mark : int;
  mutable marks : int list;  (** Each a primary key: recorded twice, a commit fails. *)
  mutable steps : int;
}

let resumed ~name ~load ~expected gen print =
  QCheck2.Test.make ~count:2000 ~name ~print gen (fun ((crashes : (int * crash) list), case) ->
      let server = { kept = []; open_ = []; mark = 0; marks = []; steps = 0 } in
      let crashed_at kinds =
        server.steps <- server.steps + 1;
        List.find_map (fun (step, crash) -> if step = server.steps && List.mem crash kinds then Some crash else None) crashes
      in
      let run text =
        match crashed_at [ Running ] with
        | Some _ -> Error ()
        | None ->
            server.open_ <- text :: server.open_;
            Ok ()
      and commit mark =
        let apply () =
          server.kept <- server.kept @ List.rev server.open_;
          server.open_ <- [];
          server.mark <- mark;
          server.marks <- mark :: server.marks
        in
        match crashed_at [ Before_commit; After_commit ] with
        | _ when List.mem mark server.marks -> Error ()
        | Some Before_commit -> Error ()
        | Some After_commit ->
            apply ();
            Error ()
        | Some Running | None ->
            apply ();
            Ok ()
      in
      let rec attempt left =
        server.open_ <- [];
        match load case ~skip:server.mark ~run ~commit with
        | Ok () -> left >= 0
        | Error () -> left > 0 && attempt (left - 1)
      in
      attempt (List.length crashes) && String.equal (String.concat "" server.kept) (String.concat "" (expected case)))

let crashes =
  QCheck2.Gen.(list_size (int_range 0 4) (pair (int_range 1 40) (oneof_list [ Running; Before_commit; After_commit ])))

let statements_resumed =
  resumed
    ~name:
      "statements loaded in pieces, broken anywhere and run again from the target's mark, are each \
       committed once, in order"
    ~load:(fun (statements, bytes) ~skip ~run ~commit ->
      Pieces.statements ~bytes ~skip
        (fun each -> Base.List.fold_result statements ~init:() ~f:(fun () statement -> each statement))
        ~run ~commit)
    ~expected:fst
    QCheck2.Gen.(
      pair crashes
        (pair
           (map (List.mapi (fun i n -> Printf.sprintf "[%d:%s]" i (String.make n 's'))) (list_size (int_range 0 25) (int_range 0 12)))
           (int_range 1 40)))
    (fun (crashes, (statements, bytes)) ->
      Sexplib0.Sexp.to_string_hum
        ([%sexp_of: (int * crash) list * string list * int] (crashes, statements, bytes)))

let rows_resumed =
  resumed
    ~name:
      "a CSV file loaded in pieces, broken anywhere and run again from the rows the target marked, \
       has each row committed once, in order"
    ~load:(fun (dialect, header, rows, bytes, _, reads) ~skip ~run ~commit ->
      let text = header ^ String.concat "" rows in
      let pos = ref 0 and turn = ref 0 and done_ = ref skip in
      let read buffer ~pos:at ~len =
        let size = List.nth reads (!turn mod List.length reads) in
        incr turn;
        let n = Int.min (Int.min len size) (String.length text - !pos) in
        Bytes.blit_string text !pos buffer at n;
        pos := !pos + n;
        Ok n
      in
      Pieces.cut dialect ~bytes ~skip read (fun { data; rows } ->
          let body = String.sub data (String.length header) (String.length data - String.length header) in
          match run body with
          | Error () -> Error ()
          | Ok () ->
              done_ := !done_ + rows;
              commit !done_))
    ~expected:(fun (_, _, rows, _, _, _) -> rows)
    QCheck2.Gen.(pair crashes gen)
    (fun (crashes, (_, header, rows, bytes, _, _)) ->
      Sexplib0.Sexp.to_string_hum
        ([%sexp_of: (int * crash) list * string * string list * int] (crashes, header, rows, bytes)))

let tests =
  [
    QCheck_alcotest.to_alcotest cut_whole;
    QCheck_alcotest.to_alcotest statements_resumed;
    QCheck_alcotest.to_alcotest rows_resumed;
  ]
