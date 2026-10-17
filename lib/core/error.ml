type t =
  [ `Consistency of Consistency.error
  | `Ident of Ident.error
  | `Ltype of Ltype.error
  | `Schema of Schema.error
  | `Chunk of Chunk.error
  | `Key of Key.error
  | `Column of Column.error
  | `Batch of Batch.error
  | `Plan_opts of Plan_opts.error
  | `Dump_opts of Dump_opts.error
  | `Concurrent of Concurrent.error ]
[@@deriving sexp_of]
