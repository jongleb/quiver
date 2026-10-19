(** MySQL option files ([my.cnf]) as the mysql client reads them. *)

(** A line that means something; comments and blank lines mean nothing. *)
type line =
  | Group of string  (** [[name]], lowercased. *)
  | Option of { name : string; value : string option }
      (** [name = value] or a bare [name]; the name lowercased, without
          [loose-] and with [-] for [_]. *)
  | Include of string  (** [!include file]. *)
  | Include_dir of string  (** [!includedir directory]. *)
[@@deriving sexp_of, equal]

val lines : string -> line list
