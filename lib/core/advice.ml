type evidence = Share of float | Near_ceiling of float [@@deriving sexp_of]

type t =
  | Receive_buffer of evidence
  | Send_buffer of evidence
  | Lossy_path of float
  | Durable_commits
  | Synced_binlog
  | Logged_load
[@@deriving sexp_of]

type counts = { sent : int; resent : int; held : held; connections : int }

and held =
  | Timed of { busy_us : int; window_full_us : int; buffer_full_us : int }
  | Peer_windows of int

let of_counts { sent; resent; held; connections } (path : Tcp.path option)
    ~seconds ~streams =
  let share part whole = Float.of_int part /. Float.of_int (Int.max 1 whole) in
  let lossy =
    let resent = share resent sent in
    Base.Option.some_if (sent > 0 && resent >= 0.001) (Lossy_path resent)
  in
  let limited =
    match (held, path) with
    | Timed { busy_us; window_full_us; buffer_full_us }, _ when busy_us > 0 ->
        let window = share window_full_us busy_us
        and buffer = share buffer_full_us busy_us in
        List.filter_map Fun.id
          [
            Base.Option.some_if (window >= 0.1) (Receive_buffer (Share window));
            Base.Option.some_if (buffer >= 0.1) (Send_buffer (Share buffer));
          ]
    | Peer_windows windows, Some { rtt; send_buffer }
      when connections > 0 && Core.Time_ns.Span.is_positive rtt ->
        let rate =
          Float.of_int sent /. Float.max 1e-3 seconds
          /. Float.of_int (Int.max 1 streams)
        and window = windows / connections in
        let reach limit = rate *. Core.Time_ns.Span.to_sec rtt /. Float.of_int (Int.max 1 limit) in
        List.filter_map Fun.id
          [
            Base.Option.some_if
              (window < send_buffer && reach window >= 0.8)
              (Receive_buffer (Near_ceiling (reach window)));
            Base.Option.some_if (reach send_buffer >= 0.8)
              (Send_buffer (Near_ceiling (reach send_buffer)));
          ]
    | (Timed _ | Peer_windows _), _ -> []
  in
  Option.to_list lossy @ limited
