type error = { call : string; code : int } [@@deriving sexp_of ~portable]
type t = int

external open_ : string -> int @@ portable = "quiver_innodb_open"
external write_ : int -> Bytes.t -> int -> int @@ portable = "quiver_innodb_write"
external read_ : int -> Bytes.t -> int -> int @@ portable = "quiver_innodb_read"
external resize_ : int -> int -> int @@ portable = "quiver_innodb_resize"
external close : int -> unit @@ portable = "quiver_innodb_close"

let outcome call result = if result < 0 then Error (`File { call; code = -result }) else Ok ()

let create path =
  let fd = open_ path in
  Result.map (fun () -> fd) (outcome "open" fd)

let whole bytes = Bytes.length bytes = Page.size

let write t n bytes = if whole bytes then outcome "pwrite" (write_ t bytes n) else Error (`File { call = "pwrite"; code = 22 })
let read t n bytes = if whole bytes then outcome "pread" (read_ t bytes n) else Error (`File { call = "pread"; code = 22 })
let resize t ~pages = outcome "ftruncate" (resize_ t pages)

external size_ : string -> int @@ portable = "quiver_innodb_size"
external load_ : string -> Bytes.t -> int @@ portable = "quiver_innodb_load"
external put_ : string -> string -> int @@ portable = "quiver_innodb_put"
external rename_ : string -> string -> int @@ portable = "quiver_innodb_rename"
external remove_ : string -> int @@ portable = "quiver_innodb_remove"
external adopt_ : string -> int @@ portable = "quiver_innodb_adopt"

let size path =
  let size = size_ path in
  if size < 0 then Error (`File { call = "stat"; code = -size }) else Ok size

let contents path =
  let size = size_ path in
  if size < 0 then Error (`File { call = "stat"; code = -size })
  else
    let bytes = Bytes.create size in
    Result.map (fun () -> Bytes.unsafe_to_string bytes) (outcome "read" (load_ path bytes))

let put path text = outcome "write" (put_ path text)
let rename from into = outcome "rename" (rename_ from into)
let remove path = outcome "unlink" (remove_ path)
let adopt path = outcome "chown" (adopt_ path)
