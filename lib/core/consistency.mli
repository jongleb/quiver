@@ portable

(** Phantom index of a level where all sessions see one point in time. *)
type snapshot = |

(** Phantom index of a level where each chunk is consistent only by itself. *)
type best_effort = |

type _ t =
  | Snapshot : snapshot t  (** All sessions see one point in time. *)
  | Best_effort : best_effort t  (** Each chunk is consistent only by itself. *)

(** A level whose index is not statically known. *)
type any = Any : _ t -> any

val yojson_of_any : any -> Yojson.Safe.t

type error =
  | Unsupported of any  (** The provider cannot guarantee this level. *)
[@@deriving sexp_of]
