open Quiver_core

let tests =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:2000 ~name:"text probes are valid UTF-8"
         ~print:(fun (lo, hi, count) -> Printf.sprintf "%S %S %d" lo hi count)
         QCheck2.Gen.(
           let text =
             map
               (fun points ->
                 let out = Buffer.create 16 in
                 List.iter
                   (fun point ->
                     Buffer.add_utf_8_uchar out
                       (Uchar.of_int
                          (if point >= 0xD800 && point <= 0xDFFF then 0x41
                           else point)))
                   points;
                 Buffer.contents out)
               (list_size (int_range 0 6)
                  (oneof [ int_range 0x20 0x7E; int_range 0x80 0x10FFFF ]))
           in
           triple text text (int_range 1 40))
         (fun (lo, hi, count) ->
           let probes = Probe.between (Key.Text lo) (Key.Text hi) ~count in
           List.length probes <= count - 1
           && List.for_all
                (function
                  | Key.Text probe -> String.is_valid_utf_8 probe | _ -> false)
                probes));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:2000
         ~name:"integer probes stay within the domain"
         ~print:(fun (lo, hi, count) -> Printf.sprintf "%Ld %Ld %d" lo hi count)
         QCheck2.Gen.(
           map3
             (fun a b count -> (Int64.min a b, Int64.max a b, count))
             int64 int64 (int_range 1 40))
         (fun (lo, hi, count) ->
           let probes = Probe.between (Key.Signed lo) (Key.Signed hi) ~count in
           List.length probes = count - 1
           && List.for_all
                (function
                  | Key.Signed v ->
                      Int64.compare lo v <= 0 && Int64.compare v hi <= 0
                  | _ -> false)
                probes));
  ]
