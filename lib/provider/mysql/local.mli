(** The Unix socket of a server on this machine, found as the mysql client
    finds it for [localhost]: [socket] in the [[client]] groups of the option
    files, then [MYSQL_UNIX_PORT], then where packages put it. *)

type system = {
  getenv : string -> string option;
  read : string -> string option;  (** A file's text, if it can be read. *)
  entries : string -> string list;
      (** Names in a directory; none if it cannot be read. *)
  exists : string -> bool;
}
(** What finding the socket reads of a machine. *)

val system : system

type error = No_socket of string list  (** The places tried. *)
[@@deriving sexp_of]

val socket : system -> (File_path.t, [> `Local of error ]) result

val send_space : unit -> int option
(** Bytes a server's Unix socket holds unread before its writes block, where
    the system sets it for every socket (macOS: [net.local.stream.sendspace],
    8 KiB unless raised, which makes a socket several times slower than TCP). *)
