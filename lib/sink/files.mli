@@ portable

(** A sink that writes a format's data files into a {!Layout}: one file per
    chunk, encoded and hashed by {!Output}, schema files and the manifest beside
    them. A format only says how a table's rows become text. *)

(** How rows become text in one format. *)
module type Format = sig
  type settings : value mod contended portable

  type lines : value mod contended portable
  (** What writing one table's rows needs. *)

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

module Make (F : Format) : sig
  type t : value mod contended portable

  type error =
    | Output of Output.error
    | Unrepresentable_date of {
        table : Quiver_core.Schema.Name.t @@ aliased;
        column : Quiver_core.Ident.t @@ aliased;
      }  (** A date or time value outside the proleptic Gregorian years 0–9999. *)
    | Misread_json of {
        table : Quiver_core.Schema.Name.t @@ aliased;
        column : Quiver_core.Ident.t @@ aliased;
      }
        (** A JSON value with a number the server would read as another double
            ({!Json_numbers}): CSV has no room to set it apart, SQL and [copy] do. *)
    | Unsplittable_definition of Quiver_core.Schema.Name.t
        (** A routine, trigger or view with [";\n"] inside a string literal. *)
  [@@deriving sexp_of ~portable]

  val create : Layout.t -> F.settings -> t @@ portable

  include Quiver_core.Sink_intf.S with type t := t and type error := error
end
