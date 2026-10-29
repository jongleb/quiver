open Base.Result.Let_syntax

type domain = { low : int64; high : int64 }

type action =
  | Permute of domain option
  | Null
  | Text of string
  | Shape
  | Email
  | Hash
  | Source of string
  | Expression of Mask_script.expression
  | Keep
  | Custom of { name : string; text : string -> string @@ portable }

type error =
  | Malformed of string @@ aliased
  | Unknown_action of string @@ aliased
  | Unused of string @@ aliased
  | Generated_column of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
  | Generated_values of Schema.Name.t @@ aliased
  | Not_nullable of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
  | Unfit of {
      table : Schema.Name.t @@ aliased;
      column : Ident.t @@ aliased;
      action : string @@ aliased;
      ltype : Ltype.t @@ aliased;
    }
  | Not_member of {
      table : Schema.Name.t @@ aliased;
      column : Ident.t @@ aliased;
      value : string @@ aliased;
    }
  | Scripted of {
      table : Schema.Name.t @@ aliased;
      column : Ident.t @@ aliased;
      error : Mask_script.error @@ aliased;
    }
  | Null_from_source of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
  | Null_from_expression of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
  | Mismatched_column of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
  | Rewritten of Column.error @@ aliased
  | Unhandled of { table : Schema.Name.t @@ aliased; columns : Ident.t list @@ aliased }
  | Finer_than_column of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
  | Not_injective of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased; action : string @@ aliased }
  | Split_reference of {
      table : Schema.Name.t @@ aliased;
      column : Ident.t @@ aliased;
      other : Schema.Name.t @@ aliased;
      other_column : Ident.t @@ aliased;
    }
  | Small_domain of {
      table : Schema.Name.t @@ aliased;
      column : Ident.t @@ aliased;
      low : int64 @@ aliased;
      high : int64 @@ aliased;
    }
  | Outside_domain of {
      table : Schema.Name.t @@ aliased;
      column : Ident.t @@ aliased;
      low : int64 @@ aliased;
      high : int64 @@ aliased;
    }
  | Too_long of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
[@@deriving sexp_of]

let action_name = function
  | Permute None -> "permute"
  | Permute (Some { low; high }) -> Printf.sprintf "permute:%Ld..%Ld" low high
  | Null -> "null"
  | Text _ -> "text"
  | Shape -> "shape"
  | Email -> "email"
  | Hash -> "hash"
  | Source _ -> "sql"
  | Expression _ -> "expr"
  | Keep -> "keep"
  | Custom { name; _ } -> name

type rule = { pattern : Pattern.column; types : string list; written : string; action : action }

let sexp_of_rule { written; _ } = Sexplib0.Sexp.Atom written

let rule written =
  match Pattern.assignment written with
  | None -> Error (`Mask (Malformed written))
  | Some (named, spelled) ->
      let prefixed prefix = Base.String.chop_prefix spelled ~prefix in
      let action =
        let domain text =
          match String.split_on_char '.' text with
          | [ low; ""; high ] -> (
              match (Int64.of_string_opt low, Int64.of_string_opt high) with
              | Some low, Some high when Int64.compare low high <= 0 -> Ok (Permute (Some { low; high }))
              | _ -> Error (`Mask (Malformed written)))
          | _ -> Error (`Mask (Malformed written))
        in
        match spelled with
        | "permute" -> Ok (Permute None)
        | "null" -> Ok Null
        | "shape" -> Ok Shape
        | "email" -> Ok Email
        | "hash" -> Ok Hash
        | "keep" -> Ok Keep
        | _ -> (
            match (prefixed "text:", prefixed "sql:", prefixed "expr:", prefixed "permute:") with
            | Some value, _, _, _ -> Ok (Text value)
            | None, Some "", _, _ -> Error (`Mask (Malformed written))
            | None, Some expression, _, _ -> Ok (Source expression)
            | None, None, Some text, _ ->
                Result.map (fun expression -> Expression expression) (Mask_script.expression ~source:written text)
            | None, None, None, Some range -> domain range
            | None, None, None, None -> Error (`Mask (Unknown_action spelled)))
      in
      let%bind action = action in
      Result.map (fun pattern -> { pattern; types = []; written; action }) (Pattern.column named)

let scripted script =
  Base.Result.all
    (List.map
       (fun ({ at = { source; line; _ }; selector; target } : Mask_script.rule) ->
         let named, types =
           match selector with
           | Named named -> (named, [])
           | Where { table; column; types } -> (table ^ "." ^ column, types)
         and action =
           match target with
           | Masked body -> Expression body
           | Kept -> Keep
           | Permuted None -> Permute None
           | Permuted (Some (low, high)) -> Permute (Some { low; high })
         in
         Result.map
           (fun pattern ->
             { pattern; types; written = Printf.sprintf "%s:%d (%s %s)" source line (action_name action) named; action })
           (Pattern.column named))
       (Mask_script.rules script))

let custom named ~name text =
  Result.map
    (fun pattern -> { pattern; types = []; written = named ^ "=" ^ name; action = Custom { name; text } })
    (Pattern.column named)

type t = { key : Keyed.t; script : Mask_script.t; rules : rule list; strict : (string * Pattern.table) list }

let make key ~script ~strict rules = { key; script; rules; strict }
let none = { key = Keyed.of_secret ""; script = Mask_script.empty; rules = []; strict = [] }

type step =
  | Nulls of int
  | Texts of { index : int; field : Ident.t; f : string -> string @@ portable }
  | Ints of { index : int; field : Ident.t; f : int -> int @@ portable }
  | Int64s of { index : int; field : Ident.t; f : int64 -> int64 @@ portable }
  | Bounded_ints of { index : int; field : Ident.t; f : int -> int option @@ portable; domain : domain }
  | Bounded_int64s of { index : int; field : Ident.t; f : int64 -> int64 option @@ portable; domain : domain }
  | Limited_texts of { index : int; field : Ident.t; f : string -> string option @@ portable }
  | Present of { index : int; field : Ident.t }
  | Computed of {
      index : int;
      field : Ident.t;
      nullability : Schema.Field.nullability;
      compiled : Mask_script.compiled;
      reads : int list;  (** The batch columns the expression reads, in its order. *)
    }

(** What a step writes back into its column, once every step has read the batch. *)
type write =
  | Null_column of int
  | Text_column of int * string option list
  | Int_column of int * int option list
  | Int64_column of int * int64 option list

module Table = struct
  type masked = {
    table : Schema.Name.t;
    key : Keyed.t;
    steps : step list;
    sources : (Ident.t * string) list;
    masked : (Ident.t * string) list;
  }

  type t = Unmasked | Masked of masked

  let none = Unmasked
  let sources = function Unmasked -> [] | Masked { sources; _ } -> sources
  let masked = function Unmasked -> [] | Masked { masked; _ } -> masked

  let apply t batch =
    match t with
    | Unmasked -> Ok ()
    | Masked t ->
        let columns = Batch.sealed_columns batch and rows = Batch.sealed_rows batch in
        let mismatched field = Error (`Mask (Mismatched_column { table = t.table; column = field })) in
        let values (type a) (column : a Column.t @ local) (f : a -> a) =
          let rec read i acc =
            if i < 0 then acc
            else
              match Column.presence column i with
              | Null -> read (i - 1) (None :: acc)
              | Present -> read (i - 1) (Some (f (Column.get column i)) :: acc)
          in
          read (rows - 1) [] [@nontail]
        in
        let attempted (type a) (column : a Column.t @ local) (f : a -> a option) =
          let rec read i acc =
            if i < 0 then Some acc
            else
              match Column.presence column i with
              | Null -> read (i - 1) (None :: acc)
              | Present -> (
                  match f (Column.get column i) with
                  | None -> None
                  | Some value -> read (i - 1) (Some value :: acc))
          in
          read (rows - 1) [] [@nontail]
        in
        let cells index =
          let (Column.Any column) = Iarray.get columns index in
          let dated : [ `Date | `Datetime | `Plain ] =
            match Column.ltype column with
            | Date -> `Date
            | Datetime _ | Timestamp _ -> `Datetime
            | _ -> `Plain
          in
          let rec read i acc =
            if i < 0 then acc
            else
              let cell : Mask_script.value =
                match (Column.presence column i, Column.repr column, dated) with
                | Null, _, _ -> Null
                | Present, Text, _ -> Text (Column.get column i)
                | Present, Int, `Date -> Date (Column.get column i)
                | Present, Int, (`Datetime | `Plain) -> Int (Int64.of_int (Column.get column i))
                | Present, Int64, `Datetime -> Datetime (Column.get column i)
                | Present, Int64, (`Date | `Plain) -> Int (Column.get column i)
                | Present, (Bool | Float), _ -> Null
              in
              read (i - 1) (cell :: acc)
          in
          Array.of_list (read (rows - 1) [])
        in
        let computed ~index ~field ~nullability ~compiled ~reads =
          let rec all = function [] -> [] | index :: rest -> cells index :: all rest in
          let own = cells index and read = all reads in
          let results =
            List.init rows (fun i ->
                Mask_script.run compiled t.key ~value:own.(i)
                  ~row:(Array.of_list (List.map (fun column -> column.(i)) read)))
          in
          let null_in_not_null =
            match nullability with
            | Schema.Field.Nullable -> None
            | Not_null ->
                List.find_opt (function Mask_script.Null -> true | Text _ | Int _ | Bool _ | Date _ | Datetime _ -> false) results
          in
          let (Column.Any column) = Iarray.get columns index in
          match null_in_not_null with
          | Some _ -> Error (`Mask (Null_from_expression { table = t.table; column = field }))
          | None -> (
              let typed f = Base.Option.all (List.map f results) in
              match Column.repr column with
              | Text ->
                  Option.fold ~none:(mismatched field)
                    ~some:(fun values -> Ok (Text_column (index, values)))
                    (typed (function
                      | Mask_script.Null -> Some None
                      | Text x -> Some (Some x)
                      | Int _ | Bool _ | Date _ | Datetime _ -> None))
              | Int ->
                  Option.fold ~none:(mismatched field)
                    ~some:(fun values -> Ok (Int_column (index, values)))
                    (typed (function
                      | Mask_script.Null -> Some None
                      | Int x -> Some (Some (Int64.to_int x))
                      | Date days -> Some (Some days)
                      | Text _ | Bool _ | Datetime _ -> None))
              | Int64 -> (
                  let unit =
                    match Column.ltype column with
                    | Datetime fsp | Timestamp fsp ->
                        Int64.of_float (10. ** float_of_int (6 - (fsp : Ltype.Fsp.t :> int)))
                    | _ -> 1L
                  in
                  let coarse =
                    List.exists
                      (function Mask_script.Datetime x -> not (Int64.equal (Int64.rem x unit) 0L) | _ -> false)
                      results
                  in
                  if coarse then Error (`Mask (Finer_than_column { table = t.table; column = field }))
                  else
                      Option.fold ~none:(mismatched field)
                        ~some:(fun values -> Ok (Int64_column (index, values)))
                        (typed (function
                          | Mask_script.Null -> Some None
                          | Int x | Datetime x -> Some (Some x)
                          | Text _ | Bool _ | Date _ -> None)))
              | Bool | Float -> mismatched field)
        in
        let read step =
          match step with
          | Nulls index -> Ok (Some (Null_column index))
          | Texts { index; field; f } -> (
              let (Column.Any column) = Iarray.get columns index in
              match Column.repr column with
              | Text -> Ok (Some (Text_column (index, values column f)))
              | Bool | Int | Int64 | Float -> mismatched field)
          | Ints { index; field; f } -> (
              let (Column.Any column) = Iarray.get columns index in
              match Column.repr column with
              | Int -> Ok (Some (Int_column (index, values column f)))
              | Bool | Text | Int64 | Float -> mismatched field)
          | Int64s { index; field; f } -> (
              let (Column.Any column) = Iarray.get columns index in
              match Column.repr column with
              | Int64 -> Ok (Some (Int64_column (index, values column f)))
              | Bool | Text | Int | Float -> mismatched field)
          | Bounded_ints { index; field; f; domain = { low; high } } -> (
              let (Column.Any column) = Iarray.get columns index in
              match Column.repr column with
              | Int ->
                  Option.fold (attempted column f)
                    ~none:(Error (`Mask (Outside_domain { table = t.table; column = field; low; high })))
                    ~some:(fun values -> Ok (Some (Int_column (index, values))))
              | Bool | Text | Int64 | Float -> mismatched field)
          | Bounded_int64s { index; field; f; domain = { low; high } } -> (
              let (Column.Any column) = Iarray.get columns index in
              match Column.repr column with
              | Int64 ->
                  Option.fold (attempted column f)
                    ~none:(Error (`Mask (Outside_domain { table = t.table; column = field; low; high })))
                    ~some:(fun values -> Ok (Some (Int64_column (index, values))))
              | Bool | Text | Int | Float -> mismatched field)
          | Limited_texts { index; field; f } -> (
              let (Column.Any column) = Iarray.get columns index in
              match Column.repr column with
              | Text ->
                  Option.fold (attempted column f)
                    ~none:(Error (`Mask (Too_long { table = t.table; column = field })))
                    ~some:(fun values -> Ok (Some (Text_column (index, values))))
              | Bool | Int | Int64 | Float -> mismatched field)
          | Present { index; field } ->
              let (Column.Any column) = Iarray.get columns index in
              let rec check i =
                if i >= rows then Ok None
                else
                  match Column.presence column i with
                  | Present -> check (i + 1)
                  | Null -> Error (`Mask (Null_from_source { table = t.table; column = field }))
              in
              check 0 [@nontail]
          | Computed { index; field; nullability; compiled; reads } ->
              Result.map Option.some (computed ~index ~field ~nullability ~compiled ~reads)
        in
        let refill (type a) (column : a Column.t @ local) (values : a option list) =
          Column.clear column;
          let rec add = function
            | [] -> Ok ()
            | None :: rest ->
                Column.add_null column;
                add rest
            | Some value :: rest -> (
                match Column.add column value with
                | Ok () -> add rest
                | Error (`Column error) -> Error (`Mask (Rewritten error)))
          in
          add values [@nontail]
        in
        let write = function
          | Null_column index ->
              let (Column.Any column) = Iarray.get columns index in
              Column.clear column;
              Base.Fn.apply_n_times ~n:rows (fun () -> Column.add_null column) ();
              Ok ()
          | Text_column (index, values) -> (
              let (Column.Any column) = Iarray.get columns index in
              match Column.repr column with Text -> refill column values [@nontail] | Bool | Int | Int64 | Float -> Ok ())
          | Int_column (index, values) -> (
              let (Column.Any column) = Iarray.get columns index in
              match Column.repr column with Int -> refill column values [@nontail] | Bool | Text | Int64 | Float -> Ok ())
          | Int64_column (index, values) -> (
              let (Column.Any column) = Iarray.get columns index in
              match Column.repr column with Int64 -> refill column values [@nontail] | Bool | Text | Int | Float -> Ok ())
        in
        let rec planned acc = function
          | [] -> Ok (List.rev acc)
          | step :: rest -> (
              match read step with
              | Ok None -> planned acc rest
              | Ok (Some next) -> planned (next :: acc) rest
              | Error _ as error -> error)
        in
        let rec written = function
          | [] -> Ok ()
          | next :: rest -> ( match write next with Ok () -> written rest | Error _ as error -> error)
        in
        let fresh (error : error) : error @ unique =
          match error with
          | Malformed text -> Malformed text
          | Unknown_action text -> Unknown_action text
          | Unused text -> Unused text
          | Generated_column { table; column } -> Generated_column { table; column }
          | Generated_values table -> Generated_values table
          | Not_nullable { table; column } -> Not_nullable { table; column }
          | Unfit { table; column; action; ltype } -> Unfit { table; column; action; ltype }
          | Not_member { table; column; value } -> Not_member { table; column; value }
          | Scripted { table; column; error } -> Scripted { table; column; error }
          | Null_from_source { table; column } -> Null_from_source { table; column }
          | Null_from_expression { table; column } -> Null_from_expression { table; column }
          | Mismatched_column { table; column } -> Mismatched_column { table; column }
          | Rewritten error -> Rewritten error
          | Unhandled { table; columns } -> Unhandled { table; columns }
          | Finer_than_column { table; column } -> Finer_than_column { table; column }
          | Not_injective { table; column; action } -> Not_injective { table; column; action }
          | Split_reference { table; column; other; other_column } ->
              Split_reference { table; column; other; other_column }
          | Small_domain { table; column; low; high } -> Small_domain { table; column; low; high }
          | Outside_domain { table; column; low; high } -> Outside_domain { table; column; low; high }
          | Too_long { table; column } -> Too_long { table; column }
        in
        let outcome = match planned [] t.steps with Ok writes -> written writes [@nontail] | Error _ as error -> error in
        match outcome with Ok () -> Ok () | Error (`Mask error) -> Error (`Mask (fresh error))
end

let resolve { key; script; rules; strict } ~relations tables =
  let kind_of (ltype : Ltype.t) : Mask_script.kind option =
    match ltype with
    | String | Bytes | Json | Enum _ | Set _ | Decimal _ -> Some Text
    | Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32 | Uint64 -> Some Int
    | Date -> Some Date
    | Datetime _ | Timestamp _ -> Some Datetime
    | Bool | Float32 | Float64 | Year | Unchecked_date | Unchecked_datetime _ | Time _ | Geometry -> None
  in
  let category (ltype : Ltype.t) =
    match ltype with
    | String -> "text"
    | Bytes -> "binary"
    | Json -> "json"
    | Bool | Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32 | Uint64 -> "integer"
    | Decimal _ -> "decimal"
    | Float32 | Float64 -> "float"
    | Date | Unchecked_date -> "date"
    | Datetime _ | Unchecked_datetime _ -> "datetime"
    | Timestamp _ -> "timestamp"
    | Time _ -> "time"
    | Year -> "year"
    | Enum _ -> "enum"
    | Set _ -> "set"
    | Geometry -> "geometry"
  in
  let%bind relations = match rules with [] -> Ok Relations.none | _ :: _ -> relations () in
  let%bind declared =
    Base.Result.all
      (List.map
         (fun ((database, table, column), (parent_database, parent_table, parent_column)) ->
           let malformed = Error (`Mask (Malformed (String.concat "." [ database; table; column ]))) in
           match
             ( Ident.of_string database,
               Ident.of_string table,
               Ident.of_string column,
               Ident.of_string parent_database,
               Ident.of_string parent_table,
               Ident.of_string parent_column )
           with
           | Ok database, Ok table, Ok column, Ok parent_database, Ok parent_table, Ok parent_column ->
               Ok
                 {
                   Relations.child = { database; table };
                   parent = { database = parent_database; table = parent_table };
                   columns = ((column, parent_column), []);
                 }
           | _ -> malformed)
         (Mask_script.references script))
  in
  let relations = { relations with foreign = relations.foreign @ declared } in
  let domain_of name (field : Schema.Field.t) spec =
    let column = field.name in
    let range =
      match
        List.find_opt
          (fun ({ table; column = narrow; _ } : Relations.range) ->
            Schema.Name.equal table name && String.equal (String.lowercase_ascii (narrow :> string)) (String.lowercase_ascii (column :> string)))
          relations.ranges
      with
      | Some { low; high; _ } -> Some (low, high, `Narrow)
      | None -> (
          match field.ltype with
          | Int8 -> Some (-128L, 127L, `Narrow)
          | Int16 -> Some (-32768L, 32767L, `Narrow)
          | Int32 -> Some (-0x8000_0000L, 0x7FFF_FFFFL, `Narrow)
          | Uint8 -> Some (0L, 255L, `Narrow)
          | Uint16 -> Some (0L, 65535L, `Narrow)
          | Uint32 -> Some (0L, 0xFFFF_FFFFL, `Narrow)
          | Int64 | Uint64 -> Some (Int64.min_int, Int64.max_int, `Wide)
          | Bool | Float32 | Float64 | Decimal _ | String | Bytes | Json | Date | Year | Unchecked_date
          | Unchecked_datetime _ | Time _ | Datetime _ | Timestamp _ | Geometry | Enum _ | Set _ ->
              None)
    in
    let unfit () = Error (`Mask (Unfit { table = name; column; action = action_name (Permute spec); ltype = field.ltype })) in
    match range with
    | None -> unfit ()
    | Some (least, most, width) -> (
        let least = match field.ltype with Uint64 -> 0L | _ -> least in
        let low, high =
          match (spec, width) with
          | Some { low; high }, _ -> (low, high)
          | None, `Wide -> (1L, 9007199254740991L)
          | None, `Narrow -> (1L, Int64.div most 2L)
        in
        if Int64.compare low least < 0 || Int64.compare high most > 0 then unfit ()
        else if Int64.unsigned_compare (Int64.sub high low) 999_999L < 0 then
          Error (`Mask (Small_domain { table = name; column; low; high }))
        else Ok { low; high })
  in
  let step (table : Schema.Table.t) (field : Schema.Field.t) index action =
    let name = table.name and column = field.name in
    let unfit () =
      Error (`Mask (Unfit { table = name; column; action = action_name action; ltype = field.ltype }))
    in
    let texts f = Ok (Some (Texts { index; field = column; f })) in
    match action with
    | Permute spec -> (
        match (field.ltype, spec) with
        | String, None ->
            let permutation = Keyed.permutation key ~label:"text" in
            Ok (Some (Limited_texts { index; field = column; f = (fun text -> Keyed.permute_text permutation text) }))
        | Bytes, None ->
            let permutation = Keyed.permutation key ~label:"bytes" in
            Ok (Some (Limited_texts { index; field = column; f = (fun value -> Keyed.permute_bytes permutation value) }))
        | _ -> (
            match domain_of name field spec with
            | Error _ as error -> error
            | Ok ({ low; high } as domain) -> (
                let permutation = Keyed.permutation key ~label:(Printf.sprintf "int:%Ld:%Ld" low high) in
                let (permute @ portable) value =
                  if Int64.compare value low < 0 || Int64.compare value high > 0 then None
                  else Some (Keyed.permute_integer permutation ~low ~high value)
                in
                match field.ltype with
                | Int64 | Uint64 -> Ok (Some (Bounded_int64s { index; field = column; f = permute; domain }))
                | _ ->
                    let (f @ portable) value = Option.map Int64.to_int (permute (Int64.of_int value)) in
                    Ok (Some (Bounded_ints { index; field = column; f; domain })))))
    | Null -> (
        match field.nullability with
        | Nullable -> Ok (Some (Nulls index))
        | Not_null -> Error (`Mask (Not_nullable { table = name; column })))
    | Text value -> (
        let (constant @ portable) _ = value in
        match field.ltype with
        | String | Bytes | Json -> texts constant
        | Enum members ->
            if List.mem value members then texts constant
            else Error (`Mask (Not_member { table = name; column; value }))
        | Set members ->
            if
              String.equal value ""
              || List.for_all (fun member -> List.mem member members) (String.split_on_char ',' value)
            then texts constant
            else Error (`Mask (Not_member { table = name; column; value }))
        | Bool | Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32 | Uint64 | Float32 | Float64
        | Decimal _ | Date | Year | Unchecked_date | Unchecked_datetime _ | Time _ | Datetime _
        | Timestamp _ | Geometry ->
            unfit ())
    | Shape -> ( match field.ltype with String -> texts (fun text -> Keyed.shape key text) | _ -> unfit ())
    | Email -> ( match field.ltype with String -> texts (fun text -> Keyed.email key text) | _ -> unfit ())
    | Hash -> (
        let ints ~low ~high =
          let (f @ portable) value =
            Int64.to_int
              (Keyed.within key (Int.to_string value) ~low:(Int64.of_int low) ~high:(Int64.of_int high))
          in
          Ok (Some (Ints { index; field = column; f }))
        in
        match field.ltype with
        | String -> texts (fun text -> Keyed.hash_text key text)
        | Bytes -> texts (fun text -> Keyed.hash_bytes key text)
        | Int8 -> ints ~low:(-128) ~high:127
        | Int16 -> ints ~low:(-32768) ~high:32767
        | Int32 -> ints ~low:(-0x8000_0000) ~high:0x7FFF_FFFF
        | Uint8 -> ints ~low:0 ~high:255
        | Uint16 -> ints ~low:0 ~high:65535
        | Uint32 -> ints ~low:0 ~high:0xFFFF_FFFF
        | Int64 | Uint64 -> Ok (Some (Int64s { index; field = column; f = (fun value -> Keyed.hash_int64 key value) }))
        | Bool | Float32 | Float64 | Decimal _ | Json | Date | Year | Unchecked_date | Unchecked_datetime _
        | Time _ | Datetime _ | Timestamp _ | Geometry | Enum _ | Set _ ->
            unfit ())
    | Source _ -> (
        match field.nullability with
        | Nullable -> Ok None
        | Not_null -> Ok (Some (Present { index; field = column })))
    | Expression expression -> (
        let batch = Schema.Table.columns table in
        let place wanted =
          Base.List.findi batch ~f:(fun _ ({ name; _ } : Schema.Field.t) ->
              String.equal (String.lowercase_ascii (name :> string)) (String.lowercase_ascii wanted))
        in
        match kind_of field.ltype with
        | None -> unfit ()
        | Some kind ->
            let%map compiled =
              Result.map_error
                (fun (`Mask_script error) -> `Mask (Scripted { table = name; column; error }))
                (Mask_script.compile script expression ~value:kind
                   ~columns:(fun wanted ->
                     Option.bind (place wanted) (fun (_, (field : Schema.Field.t)) -> kind_of field.ltype))
                   ~result:kind)
            in
            Some
              (Computed
                 {
                   index;
                   field = column;
                   nullability = field.nullability;
                   compiled;
                   reads = List.filter_map (fun wanted -> Option.map fst (place wanted)) (Mask_script.reads compiled);
                 }))
    | Keep -> Ok None
    | Custom { text; _ } -> ( match field.ltype with String | Bytes -> texts text | _ -> unfit ())
  in
  let lower (name : Ident.t) = String.lowercase_ascii (name :> string) in
  let same_column (table : Schema.Name.t) (column : Ident.t) (table' : Schema.Name.t) (column' : Ident.t) =
    Schema.Name.equal table table' && String.equal (lower column) (lower column')
  in
  let rules = Array.of_list rules in
  let uses = Array.make (Array.length rules) 0 in
  let first name ({ name = column; ltype; _ } : Schema.Field.t) =
    let rec find i =
      if i >= Array.length rules then None
      else
        match Pattern.fits_column rules.(i).pattern name column with
        | Fits when List.is_empty rules.(i).types || List.mem (category ltype) rules.(i).types -> Some i
        | Fits | Misses -> find (i + 1)
    in
    find 0
  in
  let chosen (table : Schema.Table.t) =
    let name = table.name in
    let fields = fst table.fields :: snd table.fields in
    let%map () =
      match
        List.find_map
          (fun (field : Schema.Field.t) ->
            Option.map
              (fun i ->
                uses.(i) <- uses.(i) + 1;
                field)
              (first name field))
          table.generated
      with
      | Some field -> Error (`Mask (Generated_column { table = name; column = field.name }))
      | None -> Ok ()
    in
    ( table,
      List.mapi
        (fun index (field : Schema.Field.t) ->
          ( index,
            field,
            Option.map
              (fun i ->
                uses.(i) <- uses.(i) + 1;
                rules.(i).action)
              (first name field) ))
        fields )
  in
  let%bind chosen = Base.Result.all (List.map chosen tables) in
  let referenced =
    List.concat_map
      (fun ({ parent; columns = first, rest; _ } : Relations.foreign) ->
        List.map (fun (_, column) -> (parent, column)) (first :: rest))
      relations.foreign
  in
  let classes = Hashtbl.create 64 in
  let node (table : Schema.Name.t) column =
    String.concat "\000" [ (table.database :> string); (table.table :> string); lower column ]
  in
  let rec root node =
    match Hashtbl.find_opt classes node with
    | None -> node
    | Some parent when String.equal parent node -> node
    | Some parent -> root parent
  in
  List.iter
    (fun ({ child; parent; columns = first, rest } : Relations.foreign) ->
      List.iter
        (fun (column, parent_column) ->
          let a = root (node child column) and b = root (node parent parent_column) in
          if not (String.equal a b) then Hashtbl.replace classes a b)
        (first :: rest))
    relations.foreign;
  let members =
    List.concat_map
      (fun ((table : Schema.Table.t), fields) ->
        List.map (fun (index, (field : Schema.Field.t), action) -> (root (node table.name field.name), (table, index, field, action))) fields)
      chosen
  in
  let linked =
    List.filter
      (fun (root, _) -> List.length (List.filter (fun (other, _) -> String.equal other root) members) > 1)
      members
  in
  let roots = List.sort_uniq String.compare (List.map fst linked) in
  let%bind propagated =
    Base.Result.all
      (List.map
         (fun root ->
           let group = List.filter_map (fun (other, member) -> if String.equal other root then Some member else None) linked in
           let split ((a : Schema.Table.t), _, (field : Schema.Field.t), _) ((b : Schema.Table.t), _, (other : Schema.Field.t), _) =
             Error (`Mask (Split_reference { table = a.name; column = field.name; other = b.name; other_column = other.name }))
           in
           let parent_side ((table : Schema.Table.t), _, (field : Schema.Field.t), _) =
             List.exists (fun (parent, column) -> same_column table.name field.name parent column) referenced
           in
           let shape (_, _, _, action) =
             match action with
             | None | Some Keep -> `Identity
             | Some (Permute spec) -> `Permuted spec
             | Some Null -> `Nulled
             | Some (Text _ | Shape | Email | Hash | Source _ | Expression _ | Custom _) -> `Other
           in
           let permuted = List.filter (fun member -> match shape member with `Permuted _ -> true | _ -> false) group in
           match permuted with
           | [] -> (
               let others = List.filter (fun member -> match shape member with `Other -> true | _ -> false) group in
               match others with
               | ((table, _, (field : Schema.Field.t), _) as changed) :: _ ->
                   Option.fold ~none:(Ok [])
                     ~some:(fun other -> split changed other)
                     (List.find_opt
                        (fun ((other : Schema.Table.t), _, (column : Schema.Field.t), _) ->
                          not (same_column table.Schema.Table.name field.name other.name column.name))
                        group)
               | [] ->
                   let unnulled_child (((table : Schema.Table.t), _, (field : Schema.Field.t), _) as parent) =
                     match shape parent with
                     | `Nulled when parent_side parent ->
                         List.find_map
                           (fun ({ child; parent = referred; columns = first, rest } : Relations.foreign) ->
                             List.find_map
                               (fun (child_column, parent_column) ->
                                 if same_column referred parent_column table.name field.name then
                                   List.find_opt
                                     (fun (((other : Schema.Table.t), _, (column : Schema.Field.t), _) as member) ->
                                       same_column other.name column.name child child_column
                                       && match shape member with `Nulled -> false | _ -> true)
                                     group
                                   |> Option.map (fun child -> (parent, child))
                                 else None)
                               (first :: rest))
                           relations.foreign
                     | _ -> None
                   in
                   Option.fold (List.find_map unnulled_child group) ~none:(Ok []) ~some:(fun (parent, child) ->
                       split parent child))
           | source :: _ ->
               let spec = match shape source with `Permuted spec -> spec | _ -> None in
               let resolved ((table : Schema.Table.t), _, (field : Schema.Field.t), _) =
                 match (field.ltype, spec) with
                 | String, None -> Ok `Text
                 | Bytes, None -> Ok `Bytes
                 | _ -> Result.map (fun domain -> `Integers domain) (domain_of table.name field spec)
               in
               let%bind wanted = resolved source in
               let%bind () =
                 Base.Result.all_unit
                   (List.map
                      (fun member ->
                        match shape member with
                        | `Identity | `Permuted _ ->
                            let%bind found = resolved member in
                            if found = wanted then Ok () else split source member
                        | `Nulled | `Other -> Ok ())
                      group)
               in
               let conflict =
                 List.find_opt
                   (fun member ->
                     match (member, shape member) with
                     | (_, _, _, Some Keep), _ -> true
                     | _, `Identity -> false
                     | _, `Permuted spec' -> spec' <> spec
                     | _, `Nulled -> parent_side member
                     | _, `Other -> true)
                   group
               in
               Option.fold conflict
                 ~some:(fun member -> split source member)
                 ~none:
                   (Ok
                      (List.filter_map
                         (fun ((table : Schema.Table.t), _, (field : Schema.Field.t), action) ->
                           match action with
                           | None -> Some ((table.name, field.name), Permute spec)
                           | Some _ -> None)
                         group)))
         roots)
  in
  let propagated = List.concat propagated in
  let final =
    List.map
      (fun ((table : Schema.Table.t), fields) ->
        ( table,
          List.filter_map
            (fun (index, (field : Schema.Field.t), action) ->
              match action with
              | Some action -> Some (index, field, action, `Ruled)
              | None ->
                  Option.map
                    (fun action -> (index, field, action, `Propagated))
                    (List.find_map
                       (fun ((table', column), action) ->
                         if same_column table.name field.name table' column then Some action else None)
                       propagated))
            fields ))
      chosen
  in
  let injective (field : Schema.Field.t) = function
    | Keep | Permute _ -> `Injective
    | Null -> ( match field.nullability with Nullable -> `Injective | Not_null -> `Merging)
    | Text _ | Shape | Email | Hash | Source _ | Expression _ | Custom _ -> `Merging
  in
  let%bind () =
    Base.Result.all_unit
      (List.map
         (fun ((table : Schema.Table.t), assigned) ->
           let keys =
             let read =
               match table.key with
               | Primary (first, rest) | Unique { fields = first, rest; _ } ->
                   [ List.map (fun (field : Schema.Field.t) -> field.name) (first :: rest) ]
               | Indexed _ | No_key -> []
             in
             read
             @ List.filter_map
                 (fun ({ table = owner; columns = first, rest } : Relations.unique) ->
                   if Schema.Name.equal owner table.name then Some (first :: rest) else None)
                 relations.unique
           in
           match
             List.find_opt
               (fun (_, (field : Schema.Field.t), action, _) ->
                 let keyed = List.exists (List.exists (fun column -> String.equal (lower column) (lower field.name))) keys in
                 match injective field action with `Merging -> keyed | `Injective -> false)
               assigned
           with
           | Some (_, field, action, _) ->
               Error (`Mask (Not_injective { table = table.name; column = field.name; action = action_name action }))
           | None -> Ok ())
         final)
  in
  let strict_uses = Array.make (List.length strict) 0 in
  let table_masking ((table : Schema.Table.t), assigned) =
    let name = table.name in
    let fields = fst table.fields :: snd table.fields in
    let%bind () =
      let watched =
        List.filteri
          (fun i (_, pattern) ->
            match Pattern.fits_table pattern name with
            | Fits ->
                strict_uses.(i) <- strict_uses.(i) + 1;
                true
            | Misses -> false)
          strict
      in
      let unhandled =
        List.filter
          (fun (field : Schema.Field.t) ->
            not (List.exists (fun (_, (taken : Schema.Field.t), _, _) -> Ident.equal taken.name field.name) assigned))
          fields
      in
      match (watched, unhandled) with
      | _ :: _, _ :: _ ->
          Error
            (`Mask (Unhandled { table = name; columns = List.map (fun (field : Schema.Field.t) -> field.name) unhandled }))
      | [], _ | _, [] -> Ok ()
    in
    let masked = List.filter (fun (_, _, action, _) -> match action with Keep -> false | _ -> true) assigned in
    let%bind () =
      match (masked, table.generated) with
      | _ :: _, _ :: _ -> Error (`Mask (Generated_values name))
      | [], _ | _, [] -> Ok ()
    in
    let%map steps =
      Base.Result.all (List.map (fun (index, field, action, _) -> step table field index action) masked)
    in
    match masked with
    | [] -> Table.Unmasked
    | _ :: _ ->
        Table.Masked
          {
            table = name;
            key;
            steps = List.filter_map Fun.id steps;
            sources =
              List.filter_map
                (fun (_, (field : Schema.Field.t), action, _) ->
                  match action with
                  | Source expression -> Some (field.name, expression)
                  | Permute _ | Null | Text _ | Shape | Email | Hash | Expression _ | Keep | Custom _ -> None)
                masked;
            masked =
              List.map
                (fun (_, (field : Schema.Field.t), action, origin) ->
                  match origin with
                  | `Ruled -> (field.name, action_name action)
                  | `Propagated -> (field.name, action_name action ^ " (a reference)"))
                masked;
          }
  in
  let%bind masking = Base.Result.all (List.map table_masking final) in
  match
    ( Base.Array.findi uses ~f:(fun _ uses -> uses = 0),
      Base.List.findi strict ~f:(fun i _ -> strict_uses.(i) = 0) )
  with
  | Some (i, _), _ -> Error (`Mask (Unused rules.(i).written))
  | None, Some (_, (written, _)) -> Error (`Mask (Unused written))
  | None, None -> Ok masking
