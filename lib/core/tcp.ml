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

type held =
  | Timed of {
      busy : Core.Time_ns.Span.t;
      receive_window : Core.Time_ns.Span.t;
      send_buffer : Core.Time_ns.Span.t;
    }
  | Peer_window of int

type strain = { sent : int; resent : int; held : held }

external counted : Unix.file_descr -> int array @@ portable = "quiver_tcp_strain"

let strain fd =
  let counts = counted fd in
  let held =
    match counts.(0) with
    | 1 ->
        Some
          (Timed
             {
               busy = Core.Time_ns.Span.of_int_us counts.(3);
               receive_window = Core.Time_ns.Span.of_int_us counts.(4);
               send_buffer = Core.Time_ns.Span.of_int_us counts.(5);
             })
    | 2 -> Some (Peer_window counts.(3))
    | _ -> None
  in
  Option.map (fun held -> { sent = counts.(1); resent = counts.(2); held }) held

let ceiling { rtt; send_buffer } =
  Base.Option.some_if (Core.Time_ns.Span.is_positive rtt)
    (Float.of_int send_buffer /. Core.Time_ns.Span.to_sec rtt)

module Class = struct
  type t = int

  let of_int dscp = Base.Option.some_if (Base.Int.between dscp ~low:0 ~high:63) dscp
end

external mark : Unix.file_descr -> Class.t -> unit @@ portable = "quiver_tcp_mark"
[@@noalloc]
