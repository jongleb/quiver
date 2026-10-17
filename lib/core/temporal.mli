@@ portable

(** ISO text forms of dates and times as MySQL prints and parses them:
    [YYYY-MM-DD], [YYYY-MM-DD HH:MM:SS[.f]] and signed [H+:MM:SS[.f]] with
    exactly [fsp] fractional digits. Days and microseconds count from 1970-01-01
    UTC. *)

val[@zero_alloc] days_of_date : year:int -> month:int -> day:int -> int or_null @@ portable
(** Days since 1970-01-01 of a calendar-valid date of years 0000 to 9999;
    [or_null] so that reading a date allocates nothing. *)

val[@zero_alloc] date :
  Byte_buffer.t @ local -> days:int -> (unit, [> `Unrepresentable_date ]) result @@ portable
(** [YYYY-MM-DD]; days outside 0000-01-01 to 9999-12-31 are unrepresentable. *)

val[@zero_alloc] datetime :
  Byte_buffer.t @ local ->
  Ltype.Fsp.t ->
  micros:int64# ->
  (unit, [> `Unrepresentable_date ]) result @@ portable

val[@zero_alloc] time : Byte_buffer.t @ local -> Ltype.Fsp.t -> micros:int64# -> unit @@ portable
val micros_per_second : int @@ portable
val micros_per_day : int @@ portable
val max_year : int @@ portable
val max_packed_date : int @@ portable
val[@zero_alloc] pack_date : year:int -> month:int -> day:int -> int @@ portable
val[@zero_alloc] pack_datetime :
  year:int -> month:int -> day:int -> micros:int -> int64# @@ portable
val[@zero_alloc] unchecked_date : Byte_buffer.t @ local -> int -> unit @@ portable
val[@zero_alloc] unchecked_datetime : Byte_buffer.t @ local -> Ltype.Fsp.t -> int64# -> unit @@ portable
val day : Byte_buffer.t @ local -> Core.Date.t -> unit @@ portable
val moment : Byte_buffer.t @ local -> Moment.t -> unit @@ portable
val parse_date : string -> Core.Date.t option @@ portable
val parse_datetime : string -> Moment.t option @@ portable
val parse_time : string -> int64 option @@ portable
