(** A server release line quiver is tested against, with its patch number. Every
    decision that depends on the server matches on all of them, so a new line is
    added here and then reviewed at each match the compiler points to. *)
type t = private
  | Mysql_8_0 of int  (** MySQL 8.0, patch 11 (first GA) or later. *)
  | Mysql_8_4 of int  (** MySQL 8.4 LTS. *)
  | Mariadb_10_11 of int  (** MariaDB 10.11 LTS. *)
  | Mariadb_11_4 of int  (** MariaDB 11.4 LTS. *)
  | Mariadb_11_8 of int  (** MariaDB 11.8 LTS. *)
[@@deriving sexp_of ~portable]

val of_version_string : string -> (t, [> `Unsupported of string ]) result @@ portable
