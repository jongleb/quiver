(** Converters of library types for the hand-written converters of types that
    hold them, and decoding that turns a failed conversion into an error. *)

val yojson_of_file : File_path.Part.t -> Yojson.Safe.t
val file_of_yojson : Yojson.Safe.t -> File_path.Part.t
val yojson_of_span : Core.Time_ns.Span.t -> Yojson.Safe.t
val span_of_yojson : Yojson.Safe.t -> Core.Time_ns.Span.t

val checked_of_sexp :
  ('e -> Sexplib0.Sexp.t) -> ('a, 'e) result -> Sexplib0.Sexp.t -> 'a @@ portable

val checked_of_yojson : ('e -> Sexplib0.Sexp.t) -> ('a, 'e) result -> Yojson.Safe.t -> 'a

val field : string -> (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a
(** A field of a JSON object, converted; raises as derived converters do. *)

val decode : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> ('a, string) result
(** A derived [of_yojson], its failure as text. *)
