open Core

type t

val create: unit -> t

val add_rel1: t -> String.t -> Int.t -> unit
val add_rel2: t -> String.t -> (Int.t * Int.t) -> unit
val add_rel3: t -> String.t -> (Int.t * Int.t * Int.t) -> unit

val query1: t -> String.t -> Int.t List.t
val query2: t -> String.t -> (Int.t * Int.t) List.t
val query3: t -> String.t -> (Int.t * Int.t * Int.t) List.t
