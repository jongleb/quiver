type snapshot = |
type best_effort = |
type _ t = Snapshot : snapshot t | Best_effort : best_effort t
type any = Any : _ t -> any

let name (Any level) =
  match level with Snapshot -> "snapshot" | Best_effort -> "best-effort"

let sexp_of_any any = Sexplib0.Sexp.Atom (name any)
let yojson_of_any any = `String (name any)

type error = Unsupported of any [@@deriving sexp_of]
