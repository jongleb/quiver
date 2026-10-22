open Base.Result.Let_syntax

module type Format = sig
  type settings : value mod contended portable
  type lines : value mod contended portable

  val extension : string @@ portable
  val format : settings -> Quiver_core.Output_format.t @@ portable
  val lines : settings -> Quiver_core.Schema.Table.t -> lines @@ portable

  val preamble :
    settings -> Quiver_core.Schema.Table.t -> (Quiver_core.Byte_buffer.t @ local -> unit) option @@ portable

  val write :
    lines ->
    Quiver_core.Batch.sealed @ local ->
    Quiver_core.Byte_buffer.t @ local ->
    ( unit,
      [> `Unrepresentable_date of Quiver_core.Schema.Name.t * Quiver_core.Ident.t
      | `Misread_json of Quiver_core.Schema.Name.t * Quiver_core.Ident.t ] )
    result
    @@ portable
end

module Make (F : Format) = struct
  type t = { layout : Layout.t; settings : F.settings }

  type renderer = {
    compression : Compression.t;
    buffers : Quiver_core.Buffer_pool.t;
    lines : F.lines;
  }

  type part = { output : Output.t; renderer : renderer; mutable rows : int }
  type rendered = { frame : Output.frame; rows : int }

  type error =
    | Output of Output.error
    | Unrepresentable_date of {
        table : Quiver_core.Schema.Name.t @@ aliased;
        column : Quiver_core.Ident.t @@ aliased;
      }
    | Misread_json of {
        table : Quiver_core.Schema.Name.t @@ aliased;
        column : Quiver_core.Ident.t @@ aliased;
      }
    | Unsplittable_definition of Quiver_core.Schema.Name.t
  [@@deriving sexp_of ~portable]

  let sink result = Result.map_error (fun (`Output error) -> `Sink (Output error)) result
  let create layout settings = { layout; settings }
  let format t = F.format t.settings
  let database t database ~ddl = sink (Layout.database t.layout database ~ddl)
  let table t table ~ddl ~parts:_ = sink (Layout.table t.layout table ~ddl)
  let add_part _ _ = Ok ()

  let objects t database definitions =
    Result.map_error
      (function
        | `Output error -> `Sink (Output error)
        | `Unsplittable name -> `Sink (Unsplittable_definition name))
      (Layout.objects t.layout database definitions)

  let part t table ~index =
    let buffers = Layout.buffers t.layout in
    let%bind output = sink (Layout.part t.layout table ~index ~extension:F.extension) in
    let%map () =
      match F.preamble t.settings table with
      | None -> Ok ()
      | Some write ->
          let out = Quiver_core.Buffer_pool.take buffers in
          write (borrow_ out);
          sink (Output.emit output buffers out)
    in
    {
      output;
      rows = 0;
      renderer =
        { compression = Layout.compression t.layout; buffers; lines = F.lines t.settings table };
    }

  let renderer part = part.renderer

  let render renderer (batch : Quiver_core.Batch.sealed @ local) =
    let out = Quiver_core.Buffer_pool.take renderer.buffers in
    match F.write renderer.lines batch (borrow_ out) with
    | Error (`Unrepresentable_date (table, column)) ->
        Error (`Sink (Unrepresentable_date { table; column }))
    | Error (`Misread_json (table, column)) -> Error (`Sink (Misread_json { table; column }))
    | Ok () -> (
        match Output.seal renderer.buffers renderer.compression out with
        | Ok frame -> Ok { frame; rows = Quiver_core.Batch.sealed_rows batch }
        | Error (`Output error) -> Error (`Sink (Output error)))

  let append part { frame; rows } =
    match Output.append part.output frame with
    | Ok () ->
        part.rows <- part.rows + rows;
        Ok ()
    | Error (`Output error) -> Error (`Sink (Output error))

  let close part answer =
    answer (Result.map_error (fun (`Output error) -> Output error) (Output.close part.output ~rows:part.rows))

  let discard part = Output.discard part.output

  let finish t ~source ~tables ~manifest =
    sink (Layout.finish t.layout ~source ~tables ~manifest)
end
