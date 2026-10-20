let listed =
  QCheck2.Test.make ~count:500
    ~name:"a Linux CPU list, however its runs are written, gives its CPUs"
    ~print:QCheck2.Print.(list (pair int int))
    QCheck2.Gen.(list_size (int_range 1 8) (pair (int_range 0 4) (int_range 0 3)))
    (fun runs ->
      let ranges, _ =
        List.fold_left
          (fun (ranges, next) (gap, length) ->
            ((next + gap, next + gap + length) :: ranges, next + gap + length + 2))
          ([], 0) runs
      in
      let ranges = List.rev ranges in
      let text =
        String.concat ","
          (List.map
             (fun (low, high) ->
               if low = high then string_of_int low
               else Printf.sprintf "%d-%d" low high)
             ranges)
      in
      Option.equal (List.equal Int.equal)
        (Quiver_core.Cores.cpus (text ^ "\n"))
        (Some
           (List.concat_map
              (fun (low, high) -> List.init (high - low + 1) (fun i -> low + i))
              ranges)))

let tests =
  [
    QCheck_alcotest.to_alcotest listed;
    Alcotest.test_case "a malformed CPU list gives nothing" `Quick (fun () ->
        List.iter
          (fun text ->
            Alcotest.(check (option (list int)))
              text None
              (Quiver_core.Cores.cpus text))
          [ ""; "3-1"; "1,,2"; "a"; "1-"; "-1"; "1-2-3" ]);
    Alcotest.test_case "cpu.max allows whole cores, rounded up" `Quick (fun () ->
        List.iter
          (fun (text, cores) ->
            Alcotest.(check (option int)) text cores (Quiver_core.Cores.quota text))
          [
            ("max 100000\n", None);
            ("200000 100000\n", Some 2);
            ("150000 100000", Some 2);
            ("50000 100000", Some 1);
            ("100000", None);
            ("0 100000", None);
            ("x y", None);
          ]);
    Alcotest.test_case
      "the budget is at least one core, within the runtime's domains, and \
       halves when the server shares the machine"
      `Quick (fun () ->
        let other = Quiver_core.Cores.budget Other_machine
        and same = Quiver_core.Cores.budget Same_machine in
        Alcotest.(check bool)
          "1 <= other <= domains" true
          (1 <= other && other <= Domain.recommended_domain_count ());
        Alcotest.(check int) "same" (Int.max 1 (other / 2)) same);
  ]
