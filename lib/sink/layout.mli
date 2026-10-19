type t : value mod contended portable
(** mydumper-style output directory shared by every format: schema files, one
    data file per chunk, [metadata] and the [quiver.json] manifest. *)

val create :
  dir:File_path.t -> Compression.t -> (t, [> `Output of Output.error ]) result @@ portable

val compression : t -> Compression.t @@ portable
val buffers : t -> Quiver_core.Buffer_pool.t @@ portable
val file_name : Quiver_core.Ident.t -> string @@ portable
val header : string @@ portable

val database :
  t ->
  Quiver_core.Ident.t ->
  ddl:string ->
  (unit, [> `Output of Output.error ]) result @@ portable

val table :
  t ->
  Quiver_core.Schema.Table.t ->
  ddl:string ->
  (unit, [> `Output of Output.error ]) result @@ portable

val part :
  t ->
  Quiver_core.Schema.Table.t ->
  index:int ->
  extension:string ->
  (Output.t, [> `Output of Output.error ]) result @@ portable

type script = {
  name : string;  (** The file's name without extension. *)
  statements : string list;
      (** Without their terminator; the file ends each with [";\n"], which
          appears nowhere inside one. *)
}
(** One schema file of a database's views, triggers or routines. *)

val scripts :
  Quiver_core.Ident.t ->
  Quiver_core.Definition.t list ->
  (script list, [> `Unsplittable of Quiver_core.Schema.Name.t ]) result
  @@ portable
(** In the order they are restored: a stand-in table for every view, the views,
    then triggers, then procedures, functions and events. *)

val objects :
  t ->
  Quiver_core.Ident.t ->
  Quiver_core.Definition.t list ->
  ( unit,
    [> `Output of Output.error | `Unsplittable of Quiver_core.Schema.Name.t ]
  )
  result @@ portable

val finish :
  t ->
  source:string ->
  tables:(Quiver_core.Schema.Name.t * int) list ->
  manifest:Yojson.Safe.t ->
  (unit, [> `Output of Output.error ]) result
