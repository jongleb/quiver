type error =
  | Fsp_out_of_range of int  (** Fractional seconds precision outside 0..6. *)
  | Precision_out_of_range of int  (** Decimal precision below 1. *)
  | Scale_out_of_range of { precision : int; scale : int }
      (** Decimal scale outside 0..precision. *)
[@@deriving sexp_of ~portable]

(** Fractional seconds precision, 0..6: values are stored in microseconds. *)
module Fsp : sig
  type t = private int [@@deriving sexp_of ~portable, equal ~portable]

  val yojson_of_t : t -> Yojson.Safe.t
  val of_int : int -> (t, [> `Ltype of error ]) result @@ portable
  val t_of_yojson : Yojson.Safe.t -> t
  val t_of_sexp : Sexplib0.Sexp.t -> t @@ portable
  val microseconds : t @@ portable
end

(** Exact decimal shape: [precision >= 1], [0 <= scale <= precision]. *)
module Decimal : sig
  type t = private { precision : int; scale : int } [@@deriving sexp_of ~portable, equal ~portable]

  val yojson_of_t : t -> Yojson.Safe.t
  val make : precision:int -> scale:int -> (t, [> `Ltype of error ]) result @@ portable
  val t_of_yojson : Yojson.Safe.t -> t
  val t_of_sexp : Sexplib0.Sexp.t -> t @@ portable
end

(** Logical column type shared by every provider. *)
type t =
  | Bool
  | Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Float32
  | Float64
  | Decimal of Decimal.t
  | String  (** UTF-8 text. *)
  | Bytes  (** Arbitrary binary data. *)
  | Date
  | Year  (** Calendar year; [0] is MySQL's zero year [0000]. *)
  | Unchecked_date
      (** Year, month and day as stored, never checked against the calendar:
          MySQL's [0000-00-00], [2020-00-15], [2020-02-30]. *)
  | Unchecked_datetime of Fsp.t
      (** An [Unchecked_date] with a wall-clock time. *)
  | Time of Fsp.t  (** Signed duration, may exceed one day. *)
  | Datetime of Fsp.t  (** Wall-clock date and time without a zone. *)
  | Timestamp of Fsp.t  (** Instant in UTC. *)
  | Json
  | Geometry  (** SRID (4 bytes, little-endian) followed by WKB. *)
  | Enum of string list
  | Set of string list
[@@deriving sexp ~portable, equal ~portable]

val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
