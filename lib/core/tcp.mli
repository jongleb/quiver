@@ portable

val tune : Unix.file_descr -> unit
(** Sets what every TCP connection of quiver needs: no Nagle delay, a bound on
    bytes waiting unsent, keepalive that finds a silent peer in about a minute
    and a half, and an end to data unacknowledged for two minutes. *)
