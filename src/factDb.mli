open Core

module CI = Datalog_caml_interface

type t = CI.Logic.DB.t

val create: unit -> t

val ask: ?oc:bool -> ?with_rules:CI.Parse.clause list ->
         ?with_facts:CI.Parse.term list ->
         t -> CI.Parse.term -> CI.Parse.term list

val add_rel1: t -> String.t -> Int.t -> unit
val add_rel2: t -> String.t -> (Int.t * Int.t) -> unit
val add_rel3: t -> String.t -> (Int.t * Int.t * Int.t) -> unit

val query1: t -> String.t -> Int.t List.t
val query2: t -> String.t -> (Int.t * Int.t) List.t
val query3: t -> String.t -> (Int.t * Int.t * Int.t) List.t

val get_int: t -> Int.t -> String.t -> Int.t Option.t
val get_bool: t -> String.t -> Bool.t
