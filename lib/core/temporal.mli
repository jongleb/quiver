@@ portable

(** ISO text forms of dates and times as MySQL prints and parses them:
    [YYYY-MM-DD], [YYYY-MM-DD HH:MM:SS[.f]] and signed [H+:MM:SS[.f]] with
    exactly [fsp] fractional digits. Days and microseconds count from 1970-01-01
    UTC. *)

val[@zero_alloc] days_of_date : year:int -> month:int -> day:int -> int or_null
(** Days since 1970-01-01 of a calendar-valid date of years 0000 to 9999;
    [or_null] so that reading a date allocates nothing. *)

val[@zero_alloc] date :
  Byte_buffer.t @ local -> days:int -> (unit, [> `Unrepresentable_date ]) result
(** [YYYY-MM-DD]; days outside 0000-01-01 to 9999-12-31 are unrepresentable. *)

val[@zero_alloc] datetime :
  Byte_buffer.t @ local ->
  Ltype.Fsp.t ->
  micros:int64_u ->
  (unit, [> `Unrepresentable_date ]) result

val[@zero_alloc] time : Byte_buffer.t @ local -> Ltype.Fsp.t -> micros:int64_u -> unit
val max_packed_date : int
val[@zero_alloc] pack_date : year:int -> month:int -> day:int -> int
val[@zero_alloc] pack_datetime :
  year:int -> month:int -> day:int -> micros:int -> int64_u
val[@zero_alloc] unchecked_date : Byte_buffer.t @ local -> int -> unit
val[@zero_alloc] unchecked_datetime : Byte_buffer.t @ local -> Ltype.Fsp.t -> int64_u -> unit
val day : Byte_buffer.t @ local -> Core.Date.t -> unit
val moment : Byte_buffer.t @ local -> Moment.t -> unit
val parse_date : string -> Core.Date.t option
val parse_datetime : string -> Moment.t option
val parse_time : string -> int64 option
