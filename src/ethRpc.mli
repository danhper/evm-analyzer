open Core

type t

val new_client: String.t -> t

module Eth: sig
  val get_code: ?tag:String.t -> t -> String.t -> String.t Lwt.t
  val get_balance: ?tag:String.t -> t -> String.t -> BigInt.t Lwt.t
end
