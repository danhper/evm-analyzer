open Core

module CI = Datalog_caml_interface

type t = CI.Logic.DB.t

module T: sig
  val bigint_key: BigInt.t CI.Univ.key
  val int: Int.t CI.Univ.key
  val string: String.t CI.Univ.key
end

val create: unit -> t

val ask: ?oc:bool -> ?with_rules:CI.Parse.clause list ->
         ?with_facts:CI.Parse.term list ->
         t -> CI.Parse.term -> CI.Parse.term list


val get_rel1: k:'a CI.Univ.key -> String.t -> 'a CI.Rel1.t
val get_rel2: k1:'a CI.Univ.key -> k2:'b CI.Univ.key -> String.t -> ('a, 'b) CI.Rel2.t
val get_rel3: k1:'a CI.Univ.key -> k2:'b CI.Univ.key -> k3:'c CI.Univ.key -> String.t -> ('a, 'b, 'c) CI.Rel3.t

val add_rel1: t -> 'a CI.Rel1.t -> 'a -> unit
val add_rel2: t -> ('a, 'b) CI.Rel2.t -> ('a * 'b) -> unit
val add_rel3: t -> ('a, 'b, 'c) CI.Rel3.t -> ('a * 'b * 'c) -> unit

val add_int_rel1: t -> String.t -> Int.t -> unit
val add_int_rel2: t -> String.t -> (Int.t * Int.t) -> unit
val add_int_rel3: t -> String.t -> (Int.t * Int.t * Int.t) -> unit

val query1: t -> 'a CI.Rel1.t -> 'a List.t
val query2: t -> ('a, 'b) CI.Rel2.t -> ('a * 'b) List.t
val query3: t -> ('a, 'b, 'c) CI.Rel3.t -> ('a * 'b * 'c) List.t

val get_int: t -> Int.t -> String.t -> Int.t Option.t
val get_bool: t -> String.t -> Bool.t
