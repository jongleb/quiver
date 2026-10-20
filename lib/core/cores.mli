(** The cores of this machine a dump may keep busy. *)

(** Where the database server runs, against this process. *)
type server = Same_machine | Other_machine

val budget : server -> int
(** Fast physical cores this process may run on — performance cores on macOS;
    on Linux the affinity mask without SMT siblings and hybrid efficiency
    cores, within the cgroup v2 CPU quota — and never more than the runtime's
    domains; half of them when the server shares the machine; at least one. *)

val cpus : string -> int list option
(** The CPUs of a Linux CPU list such as [0-3,8,10-11], in order. *)

val quota : string -> int option
(** Whole cores a cgroup v2 [cpu.max] allows, rounded up; [None] when it sets
    no limit or cannot be read. *)
