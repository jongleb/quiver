type byte_order = Little_endian | Big_endian [@@deriving sexp ~portable, enumerate]

let byte_order =
  match Sys.big_endian with true -> Big_endian | false -> Little_endian

type control =
  | Hello of {
      session : string;
      byte_order : byte_order;
      streams : int;
      chunks : int;
    }
  | Database of { name : Quiver_core.Ident.t; ddl : string }
  | Table of { table : Quiver_core.Schema.Table.t; ddl : string; parts : int }
  | Objects of {
      database : Quiver_core.Ident.t;
      definitions : Quiver_core.Definition.t list;
    }
  | Begin of { chunk : int; table : Quiver_core.Schema.Name.t; index : int }
  | End of { chunk : int; frames : int }
  | Abort of { chunk : int }
  | Finish of {
      source : string;
      tables : (Quiver_core.Schema.Name.t * int) list;
      manifest : string;
    }
  | Heartbeat of { replies : int }
  | Leave
[@@deriving sexp ~portable]

type attach =
  | Data of { session : string }
  | Control of { session : string; replies : int }
[@@deriving sexp ~portable]

type reply =
  | Ready of Quiver_core.Output_format.t
  | Done
  | Refused of string
  | Skipped
  | Committed of { chunk : int; part : Quiver_core.Manifest.part }
  | Rejected of { chunk : int; reason : string }
  | Heard of { messages : int }
  | Failed of string
[@@deriving sexp ~portable]
