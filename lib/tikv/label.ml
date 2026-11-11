type t = { name : string; ranges : (string * string) list }

let deny ~name first rest = { name; ranges = first :: rest }

let json t span =
  let hex key = Hex_encode.to_hex (Codec.encode_bytes key) in
  let ranges =
    String.concat ","
      (List.map (fun (start, stop) -> [%string {|{"start_key":"%{hex start}","end_key":"%{hex stop}"}|}]) t.ranges)
  in
  let ttl = Core.Time_ns.Span.to_int_ms span in
  [%string
    {|{"id":"%{t.name}","labels":[{"key":"schedule","value":"deny","ttl":"%{ttl#Int}ms"}],"rule_type":"key-range","data":[%{ranges}]}|}]

let keep dial ~host ~port t (hold : Pd.hold) =
  let request : Http1.request =
    match hold with
    | Hold_for span -> Post { path = "/pd/api/v1/config/region-label/rule"; json = json t span }
    | Let_go -> Delete [%string "/pd/api/v1/config/region-label/rule/%{t.name}"]
  in
  Result.map ignore (Http1.request dial ~host ~port request)
