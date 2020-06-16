open Core

module CI = Datalog_caml_interface


type t


module Rel4: sig
  type ('a,'b,'c,'d) t = CI.const * 'a CI.Univ.key * 'b CI.Univ.key * 'c CI.Univ.key * 'd CI.Univ.key
  val create: ?k1:'a CI.Univ.key -> ?k2:'b CI.Univ.key -> ?k3:'c CI.Univ.key
           -> ?k4:'d CI.Univ.key -> String.t -> ('a,'b,'c,'d) t
  val add_list: CI.Logic.DB.t -> ('a,'b,'c,'d) t -> ('a * 'b * 'c * 'd) List.t -> unit
end

module Rel5: sig
  type ('a,'b,'c,'d,'e) t = CI.const * 'a CI.Univ.key * 'b CI.Univ.key * 'c CI.Univ.key * 'd CI.Univ.key * 'e CI.Univ.key
  val create: ?k1:'a CI.Univ.key -> ?k2:'b CI.Univ.key -> ?k3:'c CI.Univ.key
           -> ?k4:'d CI.Univ.key -> ?k5:'e CI.Univ.key -> String.t -> ('a,'b,'c,'d,'e) t
  val add_list: CI.Logic.DB.t -> ('a,'b,'c,'d,'e) t -> ('a * 'b * 'c * 'd * 'e) List.t -> unit
end

module Types: sig
  val bigint_key: BigInt.t CI.Univ.key
  val int: Int.t CI.Univ.key
  val string: String.t CI.Univ.key
end

val num_clauses: t -> int
val num_facts: t -> int

val create: unit -> t

val ask: ?oc:bool -> ?with_rules:CI.Parse.clause list ->
         ?with_facts:CI.Parse.term list ->
         t -> CI.Parse.term -> CI.Parse.term list


val get_rel1: k:'a CI.Univ.key -> String.t -> 'a CI.Rel1.t
val get_rel2: k1:'a CI.Univ.key -> k2:'b CI.Univ.key -> String.t -> ('a, 'b) CI.Rel2.t
val get_rel3: k1:'a CI.Univ.key -> k2:'b CI.Univ.key -> k3:'c CI.Univ.key -> String.t -> ('a, 'b, 'c) CI.Rel3.t
val get_rel4: k1:'a CI.Univ.key -> k2:'b CI.Univ.key -> k3:'c CI.Univ.key ->
              k4:'d CI.Univ.key -> String.t -> ('a, 'b, 'c, 'd) Rel4.t
val get_rel5: k1:'a CI.Univ.key -> k2:'b CI.Univ.key -> k3:'c CI.Univ.key ->
              k4:'d CI.Univ.key -> k5:'e CI.Univ.key -> String.t -> ('a, 'b, 'c, 'd, 'e) Rel5.t

val add_rel1: t -> 'a CI.Rel1.t -> 'a -> unit
val add_rel2: t -> ('a, 'b) CI.Rel2.t -> ('a * 'b) -> unit
val add_rel3: t -> ('a, 'b, 'c) CI.Rel3.t -> ('a * 'b * 'c) -> unit
val add_rel4: t -> ('a, 'b, 'c, 'd) Rel4.t -> ('a * 'b * 'c * 'd) -> unit
val add_rel5: t -> ('a, 'b, 'c, 'd, 'e) Rel5.t -> ('a * 'b * 'c * 'd * 'e) -> unit

val add_int_rel1: t -> String.t -> Int.t -> unit
val add_int_rel2: t -> String.t -> (Int.t * Int.t) -> unit
val add_int_rel3: t -> String.t -> (Int.t * Int.t * Int.t) -> unit
(* val add_int_rel4: t -> String.t -> (Int.t * Int.t * Int.t * Int.t) -> unit *)

val add_bigint_rel1: t -> String.t -> BigInt.t -> unit

val query1: t -> 'a CI.Rel1.t -> 'a List.t
val query2: t -> ('a, 'b) CI.Rel2.t -> ('a * 'b) List.t
val query3: t -> ('a, 'b, 'c) CI.Rel3.t -> ('a * 'b * 'c) List.t
val query4: t -> ('a, 'b, 'c, 'd) Rel4.t -> ('a * 'b * 'c * 'd) List.t
val query5: t -> ('a, 'b, 'c, 'd, 'e) Rel5.t -> ('a * 'b * 'c * 'd * 'e) List.t

val get_int: t -> Int.t -> String.t -> Int.t Option.t
val get_bool: t -> String.t -> Bool.t


module Relations: sig
  val reentrant_call: (Int.t, BigInt.t, BigInt.t, BigInt.t, BigInt.t) Rel5.t
  val direct_call: (Int.t, BigInt.t, BigInt.t, BigInt.t) Rel4.t
  val call: (Int.t, BigInt.t, BigInt.t, BigInt.t) Rel4.t
  val failed_call: (Int.t, BigInt.t, BigInt.t) CI.Rel3.t
  val overflow: (Int.t, Bool.t, Int.t, BigInt.t, BigInt.t) Rel5.t
  val tx_sstore: (Int.t, String.t, Int.t, BigInt.t) Rel4.t
  val tx_sload: (Int.t, String.t, Int.t, BigInt.t) Rel4.t
  val tod: (Int.t, String.t, String.t, BigInt.t) Rel4.t
  val caller: (Int.t, BigInt.t) CI.Rel2.t
  val selfdestruct: (Int.t, BigInt.t) CI.Rel2.t
  val unsafe_selfdestruct: (Int.t, BigInt.t) CI.Rel2.t
  val unsafe_sstore: (Int.t, BigInt.t) CI.Rel2.t
  val mdepends_w: (Int.t, BigInt.t, BigInt.t) CI.Rel3.t
  val mdepends_r: (Int.t, BigInt.t, BigInt.t) CI.Rel3.t
end
