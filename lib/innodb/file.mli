(** A tablespace file written and read by whole pages from any thread. *)

type error = { call : string; code : int  (** [errno]. *) } [@@deriving sexp_of ~portable]

type t : immediate

val create : string -> (t, [> `File of error ]) result @@ portable
(** Created anew, or emptied. *)

val write : t -> int -> Bytes.t -> (unit, [> `File of error ]) result @@ portable
(** Page [n], a whole page of bytes. *)

val read : t -> int -> Bytes.t -> (unit, [> `File of error ]) result @@ portable
(** Page [n] into a page of bytes: zeros where the file has none. *)

val resize : t -> pages:int -> (unit, [> `File of error ]) result @@ portable
val close : t -> unit @@ portable

val size : string -> (int, [> `File of error ]) result @@ portable
(** Of a file or a directory, which shows it is there. *)

val contents : string -> (string, [> `File of error ]) result @@ portable
(** A small file whole. *)

val put : string -> string -> (unit, [> `File of error ]) result @@ portable
(** Writes a small file whole, created anew or emptied. *)

val rename : string -> string -> (unit, [> `File of error ]) result @@ portable
val remove : string -> (unit, [> `File of error ]) result @@ portable

val adopt : string -> (unit, [> `File of error ]) result @@ portable
(** Gives the file the owner and group of its directory. *)
