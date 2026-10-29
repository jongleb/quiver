open Base.Result.Let_syntax

type action =
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
  | Key_column of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
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
[@@deriving sexp_of]

let action_name = function
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
        match spelled with
        | "null" -> Ok Null
        | "shape" -> Ok Shape
        | "email" -> Ok Email
        | "hash" -> Ok Hash
        | "keep" -> Ok Keep
        | _ -> (
            match (prefixed "text:", prefixed "sql:", prefixed "expr:") with
            | Some value, _, _ -> Ok (Text value)
            | None, Some "", _ -> Error (`Mask (Malformed written))
            | None, Some expression, _ -> Ok (Source expression)
            | None, None, Some text ->
                Result.map (fun expression -> Expression expression) (Mask_script.expression ~source:written text)
            | None, None, None -> Error (`Mask (Unknown_action spelled)))
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
         and action = match target with Masked body -> Expression body | Kept -> Keep in
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
          | Key_column { table; column } -> Key_column { table; column }
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
        in
        let outcome = match planned [] t.steps with Ok writes -> written writes [@nontail] | Error _ as error -> error in
        match outcome with Ok () -> Ok () | Error (`Mask error) -> Error (`Mask (fresh error))
end

let resolve { key; script; rules; strict } tables =
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
  let step (table : Schema.Table.t) (field : Schema.Field.t) index action =
    let name = table.name and column = field.name in
    let unfit () =
      Error (`Mask (Unfit { table = name; column; action = action_name action; ltype = field.ltype }))
    in
    let texts f = Ok (Some (Texts { index; field = column; f })) in
    match action with
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
  let strict_uses = Array.make (List.length strict) 0 in
  let table_masking (table : Schema.Table.t) =
    let name = table.name in
    let fields = fst table.fields :: snd table.fields in
    let keys =
      match table.key with
      | Primary (first, rest) | Unique { fields = first, rest; _ } -> first :: rest
      | Indexed _ | No_key -> []
    in
    let%bind () =
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
    let masked =
      List.filter_map Fun.id
        (List.mapi
           (fun index (field : Schema.Field.t) ->
             Option.map
               (fun i ->
                 uses.(i) <- uses.(i) + 1;
                 (index, field, rules.(i).action))
               (first name field))
           fields)
    in
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
            not (List.exists (fun (_, (taken : Schema.Field.t), _) -> Ident.equal taken.name field.name) masked))
          fields
      in
      match (watched, unhandled) with
      | _ :: _, _ :: _ ->
          Error
            (`Mask (Unhandled { table = name; columns = List.map (fun (field : Schema.Field.t) -> field.name) unhandled }))
      | [], _ | _, [] -> Ok ()
    in
    let masked =
      List.filter (fun (_, _, action) -> match action with Keep -> false | _ -> true) masked
    in
    let%bind () =
      match
        List.find_opt
          (fun (_, (field : Schema.Field.t), _) ->
            List.exists (fun (key : Schema.Field.t) -> Ident.equal key.name field.name) keys)
          masked
      with
      | Some (_, field, _) -> Error (`Mask (Key_column { table = name; column = field.name }))
      | None -> Ok ()
    in
    let%bind () =
      match (masked, table.generated) with
      | _ :: _, _ :: _ -> Error (`Mask (Generated_values name))
      | [], _ | _, [] -> Ok ()
    in
    let%map steps = Base.Result.all (List.map (fun (index, field, action) -> step table field index action) masked) in
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
                (fun (_, (field : Schema.Field.t), action) ->
                  match action with
                  | Source expression -> Some (field.name, expression)
                  | Null | Text _ | Shape | Email | Hash | Expression _ | Keep | Custom _ -> None)
                masked;
            masked = List.map (fun (_, (field : Schema.Field.t), action) -> (field.name, action_name action)) masked;
          }
  in
  let%bind masking = Base.Result.all (List.map table_masking tables) in
  match
    ( Base.Array.findi uses ~f:(fun _ uses -> uses = 0),
      Base.List.findi strict ~f:(fun i _ -> strict_uses.(i) = 0) )
  with
  | Some (i, _), _ -> Error (`Mask (Unused rules.(i).written))
  | None, Some (_, (written, _)) -> Error (`Mask (Unused written))
  | None, None -> Ok masking
