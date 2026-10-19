type t =
  | Mysql_8_0 of int
  | Mysql_8_4 of int
  | Mariadb_10_11 of int
  | Mariadb_11_4 of int
  | Mariadb_11_8 of int
[@@deriving sexp_of ~portable]

let of_version_string raw =
  let digits part =
    int_of_string_opt
      (Core.String.take_while part ~f:(function
        | '0' .. '9' -> true
        | _ -> false))
  in
  let flavour =
    if Base.String.is_substring raw ~substring:"TiDB" then `Tidb
    else if Base.String.is_substring raw ~substring:"MariaDB" then `Mariadb
    else `Mysql
  in
  let line =
    match String.split_on_char '.' raw with
    | major :: minor :: patch :: _ -> (
        match (flavour, digits major, digits minor, digits patch) with
        | `Mysql, Some 8, Some 0, Some patch when patch >= 11 -> Some (Mysql_8_0 patch)
        | `Mysql, Some 8, Some 4, Some patch -> Some (Mysql_8_4 patch)
        | `Mariadb, Some 10, Some 11, Some patch -> Some (Mariadb_10_11 patch)
        | `Mariadb, Some 11, Some 4, Some patch -> Some (Mariadb_11_4 patch)
        | `Mariadb, Some 11, Some 8, Some patch -> Some (Mariadb_11_8 patch)
        | (`Mysql | `Mariadb | `Tidb), _, _, _ -> None)
    | _ -> None
  in
  Option.to_result line ~none:(`Unsupported raw)
