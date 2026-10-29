@@ portable

(** A parallel dump from provider [P] into sink [S] at consistency ['lvl]. *)
module Make (P : Provider_intf.S) (S : Sink_intf.S) : sig
  type error = [ Error.t | `Provider of P.error | `Sink of S.error ]
  [@@deriving sexp_of]

  type 'lvl request = {
    conn : P.conn_config;
    mode : P.snapshot_mode;
    level : 'lvl Consistency.t;
    filter : Filter.t;
    rows : Filter.rows;
    objects : Filter.objects;
    masking : Mask.t;  (** Checked against the dump's tables before the snapshot. *)
    opts : Dump_opts.t;
  }
  (** What to dump and how. *)

  val run :
    'lvl request ->
    S.t ->
    (Manifest.t, error) result
end
