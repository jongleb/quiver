type server = Same_machine | Other_machine

external performance : unit -> int = "quiver_cores_performance" [@@noalloc]

let cpus text =
  let open Base.Option.Let_syntax in
  let%map ranges =
    Base.Option.all
      (List.map
         (fun range ->
           match String.split_on_char '-' (String.trim range) with
           | [ cpu ] ->
               let%map cpu = int_of_string_opt cpu in
               [ cpu ]
           | [ low; high ] ->
               let%bind low = int_of_string_opt low
               and high = int_of_string_opt high in
               if 0 <= low && low <= high then
                 Some (List.init (high - low + 1) (fun i -> low + i))
               else None
           | _ -> None)
         (String.split_on_char ',' (String.trim text)))
  in
  List.concat ranges

let quota text =
  match String.split_on_char ' ' (String.trim text) with
  | [ limit; period ] -> (
      match (int_of_string_opt limit, int_of_string_opt period) with
      | Some limit, Some period when limit > 0 && period > 0 ->
          Some ((limit + period - 1) / period)
      | _ -> None)
  | _ -> None

let budget server =
  let read path =
    Base.Option.try_with (fun () -> Core.In_channel.read_all path)
  in
  let field text ~prefix =
    List.find_map
      (fun line -> Base.String.chop_prefix line ~prefix)
      (String.split_on_char '\n' text)
  in
  let linux () =
    let open Base.Option.Let_syntax in
    let%map allowed =
      let%bind status = read "/proc/self/status" in
      let%bind listed = field status ~prefix:"Cpus_allowed_list:" in
      cpus listed
    in
    let fast =
      Option.fold ~none:allowed
        ~some:(fun performance ->
          match List.filter (fun cpu -> List.mem cpu performance) allowed with
          | [] -> allowed
          | fast -> fast)
        (Option.bind (read "/sys/devices/cpu_core/cpus") cpus)
    in
    let topology cpu name =
      Option.bind
        (read (Printf.sprintf "/sys/devices/system/cpu/cpu%d/topology/%s" cpu name))
        (fun text -> int_of_string_opt (String.trim text))
    in
    let physical =
      Option.fold ~none:(List.length fast)
        ~some:(fun cores ->
          List.length (List.sort_uniq [%compare: int * int] cores))
        (Base.Option.all
           (List.map
              (fun cpu ->
                let%map package = topology cpu "physical_package_id"
                and core = topology cpu "core_id" in
                (package, core))
              fast))
    in
    let root = "/sys/fs/cgroup" in
    let rec levels dir =
      if String.equal dir root then [ dir ]
      else if Base.String.is_prefix dir ~prefix:(root ^ "/") then
        dir :: levels (Filename.dirname dir)
      else []
    in
    let limit =
      let%bind cgroup = read "/proc/self/cgroup" in
      let%bind path = field cgroup ~prefix:"0::" in
      Base.List.min_elt ~compare:Int.compare
        (List.filter_map
           (fun dir -> Option.bind (read (dir ^ "/cpu.max")) quota)
           (levels (root ^ Base.String.rstrip ~drop:(Char.equal '/') path)))
    in
    Option.fold ~none:physical ~some:(Int.min physical) limit
  in
  let fast =
    Option.value ~default:(Domain.recommended_domain_count ())
      (if performance () > 0 then Some (performance ()) else linux ())
  in
  let usable = Int.min fast (Domain.recommended_domain_count ()) in
  match server with
  | Other_machine -> Int.max 1 usable
  | Same_machine -> Int.max 1 (usable / 2)
