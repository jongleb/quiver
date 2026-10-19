external tune : Unix.file_descr -> unit @@ portable = "quiver_tcp_tune" [@@noalloc]

type path = { rtt : Core.Time_ns.Span.t; send_buffer : int }

external rtt_us : Unix.file_descr -> int @@ portable = "quiver_tcp_rtt"
[@@noalloc]

external send_buffer_limit : unit -> int @@ portable
  = "quiver_tcp_send_buffer_limit"
[@@noalloc]

let path fd =
  match (rtt_us fd, send_buffer_limit ()) with
  | rtt, send_buffer when rtt >= 0 && send_buffer > 0 ->
      Some { rtt = Core.Time_ns.Span.of_int_us rtt; send_buffer }
  | _ -> None

let ceiling { rtt; send_buffer } =
  match Core.Time_ns.Span.(rtt > zero) with
  | true ->
      Some (Float.of_int send_buffer /. Core.Time_ns.Span.to_sec rtt)
  | false -> None

module Class = struct
  type t = int

  let of_int dscp = match dscp >= 0 && dscp < 64 with true -> Some dscp | false -> None
end

external mark : Unix.file_descr -> Class.t -> unit @@ portable = "quiver_tcp_mark"
[@@noalloc]
