open Core

type t
type tag = [`Block of Int.t | `Latest | `Earliest | `Pending]

val new_client: String.t -> t

module Eth: sig
  val get_code: ?tag:tag -> t -> String.t -> String.t Lwt.t
  val get_balance: ?tag:tag -> t -> String.t -> BigInt.t Lwt.t
  val get_transaction: t -> String.t -> EthTypes.Transaction.t Option.t Lwt.t
end
