let aes ~key text =
  match Quiver_openssl.Ff1.aes ~key text with Some out -> out | None -> Alcotest.fail "AES refused its block"

let reference ~key ~tweak ~radix numerals =
  let n = Array.length numerals in
  let u = n / 2 in
  let v = n - u in
  let z = Z.of_int radix in
  let power k = Z.pow z k in
  let number digits = Array.fold_left (fun acc digit -> Z.add (Z.mul acc z) (Z.of_int digit)) Z.zero digits in
  let spelled m value =
    let out = Array.make m 0 in
    let rec go i value =
      if i >= 0 then (
        out.(i) <- Z.to_int (Z.rem value z);
        go (i - 1) (Z.div value z))
    in
    go (m - 1) value;
    out
  in
  let b = (Z.numbits (Z.pred (power v)) + 7) / 8 in
  let d = (4 * ((b + 3) / 4)) + 4 in
  let t = String.length tweak in
  let bytes_of width value =
    String.init width (fun i -> Char.chr (Z.to_int (Z.logand (Z.shift_right value (8 * (width - 1 - i))) (Z.of_int 0xff))))
  in
  let p =
    String.concat ""
      [
        "\001\002\001"; bytes_of 3 (Z.of_int radix); "\010"; bytes_of 1 (Z.of_int (u land 0xff)); bytes_of 4 (Z.of_int n);
        bytes_of 4 (Z.of_int t);
      ]
  in
  let xor a b = String.init 16 (fun i -> Char.chr (Char.code a.[i] lxor Char.code b.[i])) in
  let prf text =
    let rec go acc pos = if pos >= String.length text then acc else go (aes ~key (xor acc (String.sub text pos 16))) (pos + 16) in
    go (String.make 16 '\000') 0
  in
  let rec rounds i a b_ =
    if i = 10 then Array.append a b_
    else
      let pad = (((-(t + b + 1)) mod 16) + 16) mod 16 in
      let q = String.concat "" [ tweak; String.make pad '\000'; bytes_of 1 (Z.of_int i); bytes_of b (number b_) ] in
      let r = prf (p ^ q) in
      let extra = List.init (((d + 15) / 16) - 1) (fun j -> aes ~key (xor r (bytes_of 16 (Z.of_int (j + 1))))) in
      let s = String.sub (String.concat "" (r :: extra)) 0 d in
      let m = if i mod 2 = 0 then u else v in
      let y = String.fold_left (fun acc c -> Z.add (Z.shift_left acc 8) (Z.of_int (Char.code c))) Z.zero s in
      let c = Z.erem (Z.add (number a) y) (power m) in
      rounds (i + 1) b_ (spelled m c)
  in
  rounds 0 (Array.sub numerals 0 u) (Array.sub numerals u v)

let alphabet = "0123456789abcdefghijklmnopqrstuvwxyz"

let nist () =
  let k128 = Hex_encode.from_hex "2B7E151628AED2A6ABF7158809CF4F3C"
  and k192 = Hex_encode.from_hex "2B7E151628AED2A6ABF7158809CF4F3CEF4359D8D580AA4F"
  and k256 = Hex_encode.from_hex "2B7E151628AED2A6ABF7158809CF4F3CEF4359D8D580AA4F7F036D6F04FC6A94" in
  let ten = Hex_encode.from_hex "39383736353433323130" and eleven = Hex_encode.from_hex "3737373770717273373737" in
  List.iteri
    (fun i (key, radix, tweak, plain, expected) ->
      let numerals = Array.init (String.length plain) (fun j -> String.index alphabet plain.[j]) in
      let out = reference ~key ~tweak ~radix numerals in
      Alcotest.(check string)
        (Printf.sprintf "sample %d" (i + 1))
        expected
        (String.init (Array.length out) (fun j -> alphabet.[out.(j)])))
    [
      (k128, 10, "", "0123456789", "2433477484");
      (k128, 10, ten, "0123456789", "6124200773");
      (k128, 36, eleven, "0123456789abcdefghi", "a9tv40mll9kdu509eum");
      (k192, 10, "", "0123456789", "2830668132");
      (k192, 10, ten, "0123456789", "2496655549");
      (k192, 36, eleven, "0123456789abcdefghi", "xbj3kv35jrawxv32ysr");
      (k256, 10, "", "0123456789", "6657667009");
      (k256, 10, ten, "0123456789", "1001623463");
      (k256, 36, eleven, "0123456789abcdefghi", "xs8a0azh2avyalyzuwd");
    ]

let binary =
  QCheck2.Test.make ~count:500 ~name:"the C permutation of n bits is FF1 of radix 2"
    ~print:(fun (key, n, value) -> Printf.sprintf "key %S, %d bits, %d" key n value)
    QCheck2.Gen.(
      let* key = string_size ~gen:char (return 32) in
      let* n = int_range 2 62 in
      let* value = int_bound ((1 lsl n) - 1) in
      return (key, n, value))
    (fun (key, n, value) ->
      let numerals = Array.init n (fun i -> (value lsr (n - 1 - i)) land 1) in
      let expected = Array.fold_left (fun acc bit -> (acc lsl 1) lor bit) 0 (reference ~key ~tweak:"" ~radix:2 numerals) in
      let limit = Int64.sub (Int64.shift_left 1L n) 1L in
      Int64.equal (Int64.of_int expected) (Quiver_openssl.Ff1.below ~key ~limit (Int64.of_int value)))

let distinct values =
  let sorted = List.sort_uniq compare values in
  List.length sorted = List.length values

let key_gen = QCheck2.Gen.(string_size ~gen:char (return 32))

let below_is_bijection =
  QCheck2.Test.make ~count:200 ~name:"a permutation below any limit is a bijection of 0 .. limit"
    ~print:(fun (key, limit) -> Printf.sprintf "key %S, limit %d" key limit)
    QCheck2.Gen.(pair key_gen (int_bound 3000))
    (fun (key, limit) ->
      let image =
        List.init (limit + 1) (fun x -> Int64.to_int (Quiver_openssl.Ff1.below ~key ~limit:(Int64.of_int limit) (Int64.of_int x)))
      in
      List.for_all (fun y -> y >= 0 && y <= limit) image && distinct image)

let digits_are_bijection =
  QCheck2.Test.make ~count:200 ~name:"a permutation of mixed digits is a bijection keeping each radix"
    ~print:(fun (key, radixes) -> Printf.sprintf "key %S, radixes %s" key (String.concat " " (List.map string_of_int radixes)))
    QCheck2.Gen.(
      pair key_gen
        (let* count = int_range 0 4 in
         list_size (return count) (oneof [ int_range 2 12; return 256; int_range 2 40 ])))
    (fun (key, radixes) ->
      let product = List.fold_left ( * ) 1 radixes in
      QCheck2.assume (product <= 5_000);
      let spelled = String.concat "" (List.map (fun r -> String.make 1 (Char.chr (r land 0xff))) radixes) in
      let all =
        List.init product (fun x ->
            let rec go acc x = function
              | [] -> acc
              | r :: rest -> go (Char.chr (x mod r) :: acc) (x / r) rest
            in
            String.of_seq (List.to_seq (go [] x (List.rev radixes))))
      in
      let image =
        List.map
          (fun digits ->
            match Quiver_openssl.Ff1.digits ~key ~tweak:"t" ~radixes:spelled digits with
            | Some out -> out
            | None -> Alcotest.fail "refused")
          all
      in
      List.for_all
        (fun out -> List.for_all2 (fun r c -> Char.code c < r) radixes (List.of_seq (String.to_seq out)))
        image
      && distinct image)

let long_strings =
  QCheck2.Test.make ~count:60 ~name:"long strings of bits are FF1 of radix 2, every block of the round output used"
    ~print:(fun (key, bits) -> Printf.sprintf "key %S, %d bits" key (String.length bits))
    QCheck2.Gen.(pair key_gen (string_size ~gen:(oneof_list [ '\000'; '\001' ]) (int_range 63 700)))
    (fun (key, bits) ->
      let numerals = Array.init (String.length bits) (fun i -> Char.code bits.[i]) in
      let expected = reference ~key ~tweak:"" ~radix:2 numerals in
      match Quiver_openssl.Ff1.digits ~key ~tweak:"" ~radixes:(String.make (String.length bits) '\002') bits with
      | None -> false
      | Some out -> Array.for_all2 (fun e c -> e = Char.code c) expected (Array.init (String.length out) (String.get out)))

let digits_of_two_are_bits =
  QCheck2.Test.make ~count:300 ~name:"mixed digits of radix 2 are the bits of an integer"
    ~print:(fun (key, n, value) -> Printf.sprintf "key %S, %d bits, %d" key n value)
    QCheck2.Gen.(
      let* key = key_gen in
      let* n = int_range 1 62 in
      let* value = int_bound ((1 lsl n) - 1) in
      return (key, n, value))
    (fun (key, n, value) ->
      let bits = String.init n (fun i -> Char.chr ((value lsr (n - 1 - i)) land 1)) in
      let limit = Int64.sub (Int64.shift_left 1L n) 1L in
      let through_integer = Int64.to_int (Quiver_openssl.Ff1.below ~key ~limit (Int64.of_int value)) in
      match Quiver_openssl.Ff1.digits ~key ~tweak:"" ~radixes:(String.make n '\002') bits with
      | None -> false
      | Some out -> String.fold_left (fun acc c -> (acc lsl 1) lor Char.code c) 0 out = through_integer)

let tweak_matters () =
  let key = String.make 32 'k' in
  let at tweak = Quiver_openssl.Ff1.digits ~key ~tweak ~radixes:(String.make 8 '\010') "\001\002\003\004\005\006\007\008" in
  Alcotest.(check bool) "another tweak, another permutation" false (Option.equal String.equal (at "a") (at "b"));
  Alcotest.(check (option string)) "too long" None
    (Quiver_openssl.Ff1.digits ~key ~tweak:"" ~radixes:(String.make 1025 '\000') (String.make 1025 'x'));
  Alcotest.(check (option string)) "digit above its radix" None
    (Quiver_openssl.Ff1.digits ~key ~tweak:"" ~radixes:"\010" "\010");
  Alcotest.(check (option string)) "radix one" None (Quiver_openssl.Ff1.digits ~key ~tweak:"" ~radixes:"\001" "\000")

let labels () =
  let secret = Quiver_core.Keyed.of_secret "s" in
  let at label =
    List.init 50 (fun i ->
        Quiver_core.Keyed.permute_integer (Quiver_core.Keyed.permutation secret ~label) ~low:0L ~high:1_000_000L (Int64.of_int i))
  in
  Alcotest.(check bool) "another label, another permutation" false (at "a" = at "b");
  Alcotest.(check bool) "one label, one permutation" true (at "a" = at "a");
  Alcotest.(check bool)
    "another secret, another permutation" false
    (at "a"
    = List.init 50 (fun i ->
          Quiver_core.Keyed.permute_integer
            (Quiver_core.Keyed.permutation (Quiver_core.Keyed.of_secret "t") ~label:"a")
            ~low:0L ~high:1_000_000L (Int64.of_int i)));
  let permutation = Quiver_core.Keyed.permutation secret ~label:"a" in
  Alcotest.(check (option string)) "text with too many letters" None
    (Quiver_core.Keyed.permute_text permutation (String.make 2000 'a'));
  Alcotest.(check bool) "text just short enough" true
    (Option.is_some (Quiver_core.Keyed.permute_text permutation (String.make 1700 'a')));
  Alcotest.(check (option string)) "bytes beyond a kilobyte" None (Quiver_core.Keyed.permute_bytes permutation (String.make 1025 'a'));
  Alcotest.(check (option string)) "nothing to permute" (Some "-- .") (Quiver_core.Keyed.permute_text permutation "-- .")

let permutation = Quiver_core.Keyed.permutation (Quiver_core.Keyed.of_secret "s") ~label:"test"

let integers_in_range =
  QCheck2.Test.make ~count:200 ~name:"an integer permutation stays in its range and is a bijection there"
    ~print:(fun (low, span) -> Printf.sprintf "from %d, %d values" low (span + 1))
    QCheck2.Gen.(pair (int_range (-5000) 5000) (int_bound 2000))
    (fun (low, span) ->
      let image =
        List.init (span + 1) (fun i ->
            Quiver_core.Keyed.permute_integer permutation ~low:(Int64.of_int low) ~high:(Int64.of_int (low + span))
              (Int64.of_int (low + i)))
      in
      List.for_all (fun y -> Int64.compare y (Int64.of_int low) >= 0 && Int64.compare y (Int64.of_int (low + span)) <= 0) image
      && distinct image)

let alphabet_gen =
  QCheck2.Gen.oneof_list
    [ "0"; "7"; "a"; "b"; "z"; "B"; "Z"; "ж"; "Ж"; "е"; "Е"; "Ё"; "ё"; "я"; "@"; "."; "ü"; " "; "\000"; "\255" ]
let text_gen = QCheck2.Gen.(map (String.concat "") (list_size (int_bound 7) alphabet_gen))

let kind code =
  if code >= Char.code '0' && code <= Char.code '9' then `Digit
  else if (code >= Char.code 'a' && code <= Char.code 'z') || (code >= 0x430 && code <= 0x44F) || code = 0x451 then `Lower
  else if (code >= Char.code 'A' && code <= Char.code 'Z') || (code >= 0x410 && code <= 0x42F) || code = 0x401 then `Upper
  else `Fixed code

let script code = if code < 0x80 then `Latin else `Cyrillic

let points text =
  let rec go pos acc = if pos >= String.length text then List.rev acc else
      let width, code = Quiver_core.Utf8.decode text pos in
      go (pos + width) ((String.sub text pos width, code) :: acc)
  in
  go 0 []

let text_keeps_form =
  QCheck2.Test.make ~count:1000 ~name:"a permuted text keeps every character's kind, case and place"
    ~print:(Printf.sprintf "%S") text_gen
    (fun text ->
      match Quiver_core.Keyed.permute_text permutation text with
      | None -> false
      | Some out ->
          String.length out = String.length text
          && List.for_all2
               (fun (raw, code) (raw', code') ->
                 match (kind code, kind code') with
                 | `Fixed _, _ -> String.equal raw raw'
                 | (`Lower | `Upper), _ -> kind code = kind code' && script code = script code'
                 | `Digit, _ -> kind code' = `Digit)
               (points text) (points out))

let fold_case text =
  String.concat ""
    (List.map
       (fun (raw, code) ->
         match kind code with
         | `Upper when code < 0x80 -> String.lowercase_ascii raw
         | `Upper ->
             let lower = if code = 0x401 then 0x451 else code + 0x20 in
             let out = Buffer.create 2 in
             Buffer.add_utf_8_uchar out (Uchar.of_int lower);
             Buffer.contents out
         | `Lower | `Digit | `Fixed _ -> raw)
       (points text))

let text_injective =
  QCheck2.Test.make ~count:300 ~name:"distinct texts give distinct masks, texts equal but for case or trailing spaces give masks equal so"
    ~print:QCheck2.Print.(list (Printf.sprintf "%S"))
    QCheck2.Gen.(list_size (int_range 2 60) text_gen)
    (fun texts ->
      let texts = List.sort_uniq compare texts in
      let masked = List.map (fun text -> Option.get (Quiver_core.Keyed.permute_text permutation text)) texts in
      let trim text =
        let rec ends pos = if pos > 0 && Char.equal text.[pos - 1] ' ' then ends (pos - 1) else pos in
        String.sub text 0 (ends (String.length text))
      in
      let loose text = trim (fold_case text) in
      distinct masked
      && List.for_all2
           (fun a ma ->
             List.for_all2 (fun b mb -> String.equal (loose a) (loose b) = String.equal (loose ma) (loose mb)) texts masked)
           texts masked)

let flip text =
  String.concat ""
    (List.map
       (fun (raw, code) ->
         let swapped =
           if code >= Char.code 'a' && code <= Char.code 'z' then code - 32
           else if code >= Char.code 'A' && code <= Char.code 'Z' then code + 32
           else if code >= 0x430 && code <= 0x44F then code - 0x20
           else if code >= 0x410 && code <= 0x42F then code + 0x20
           else if code = 0x451 then 0x401
           else if code = 0x401 then 0x451
           else code
         in
         if swapped = code then raw
         else
           let out = Buffer.create 2 in
           Buffer.add_utf_8_uchar out (Uchar.of_int swapped);
           Buffer.contents out)
       (points text))

let case_commutes =
  QCheck2.Test.make ~count:1000 ~name:"flipping the case of a text flips the case of its mask"
    ~print:(Printf.sprintf "%S") text_gen
    (fun text ->
      Option.equal String.equal
        (Quiver_core.Keyed.permute_text permutation (flip text))
        (Option.map flip (Quiver_core.Keyed.permute_text permutation text)))

let bytes_injective =
  QCheck2.Test.make ~count:200 ~name:"permuted bytes keep their length and never meet"
    ~print:QCheck2.Print.(list (Printf.sprintf "%S"))
    QCheck2.Gen.(list_size (int_range 2 80) (string_size ~gen:(oneof_list [ '\000'; 'a'; '\255' ]) (int_bound 3)))
    (fun values ->
      let values = List.sort_uniq compare values in
      let masked = List.map (fun value -> Option.get (Quiver_core.Keyed.permute_bytes permutation value)) values in
      List.for_all2 (fun a b -> String.length a = String.length b) values masked && distinct masked)

let tests =
  [
    Alcotest.test_case "FF1 reference against NIST samples" `Quick nist;
    QCheck_alcotest.to_alcotest binary;
    QCheck_alcotest.to_alcotest below_is_bijection;
    QCheck_alcotest.to_alcotest digits_are_bijection;
    QCheck_alcotest.to_alcotest digits_of_two_are_bits;
    QCheck_alcotest.to_alcotest long_strings;
    Alcotest.test_case "tweaks and refusals" `Quick tweak_matters;
    Alcotest.test_case "labels, secrets and lengths" `Quick labels;
    QCheck_alcotest.to_alcotest integers_in_range;
    QCheck_alcotest.to_alcotest text_keeps_form;
    QCheck_alcotest.to_alcotest text_injective;
    QCheck_alcotest.to_alcotest case_commutes;
    QCheck_alcotest.to_alcotest bytes_injective;
  ]
