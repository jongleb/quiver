@@ portable

(** What an output format implements. A part is the output of one chunk, the
    [index]-th of its table. [render] turns one batch into a self-contained
    piece of output and may run on any thread, concurrently for the same part;
    it must not keep the batch. [append] receives a part's rendered batches in
    batch order, one at a time; a rendered batch has one owner, so its buffers
    go back to their pool only after [append]. A part ends with [close] once its chunk is read
    whole, or with [discard] when reading it failed. *)
module type S = sig
  type t : value mod contended portable
  (** The output, shared by every thread of a dump. *)

  type part
  (** Output of one chunk, open for appending; one thread uses it at a time. *)

  type renderer : value mod contended portable
  (** What rendering a part's batches needs, shared by the render threads. *)

  type rendered : value mod portable
  (** One batch rendered on its own, handed to the thread that appends it. *)

  type error : value mod contended portable
  (** Sink-specific failure. *)

  val sexp_of_error : error -> Sexplib0.Sexp.t @@ portable
  val format : t -> Output_format.t @@ portable

  val database :
    t -> Ident.t -> ddl:string -> (unit, [> `Sink of error ]) result
    @@ portable

  val table :
    t ->
    Schema.Table.t ->
    ddl:string ->
    parts:int ->
    order:Schema.order ->
    (unit, [> `Sink of error ]) result
    @@ portable

  val add_part :
    t -> Schema.Table.t -> (unit, [> `Sink of error ]) result @@ portable
  (** The table gets one part more than [table] announced: called while
      another of its parts is open, so before its last part closes. *)

  val objects :
    t -> Ident.t -> Definition.t list -> (unit, [> `Sink of error ]) result
    @@ portable

  val part :
    t -> Schema.Table.t -> index:int -> (part, [> `Sink of error ]) result
    @@ portable

  val renderer : part -> renderer @@ portable

  val render :
    renderer ->
    Batch.sealed @ local ->
    (rendered, [> `Sink of error ]) result @ unique
    @@ portable

  val append :
    part -> rendered @ once unique -> (unit, [> `Sink of error ]) result
    @@ portable

  val close :
    part -> ((Manifest.part, error) result -> unit) @ portable -> unit
    @@ portable
  (** The outcome reaches the callback once the part is durable, on any thread
      and possibly before [close] returns; the caller moves on meanwhile. *)

  val discard : part -> unit @@ portable

  val finish :
    t ->
    source:string ->
    tables:(Schema.Name.t * int) list ->
    manifest:Yojson.Safe.t ->
    (unit, [> `Sink of error ]) result
  (** On the thread that started the dump: the manifest is JSON. *)
end
