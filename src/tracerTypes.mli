open Core

module StackValue: sig
  type t = {
    value: BigInt.t;
    id: Int.t;
  } [@@deriving show]

  val create: id:Int.t -> BigInt.t -> t

  val copy: t -> t
end

module EVMStorage: sig
  type t

  val empty: unit -> t
  val exists: t -> BigInt.t -> bool
  val set: t -> BigInt.t -> BigInt.t -> unit
  val get: t -> BigInt.t -> BigInt.t
end

module EVMStack: sig
  include module type of EStack.Funcs

  type t = StackValue.t EStack.t
end

module Env: sig
  type t = {
    stack: EVMStack.t;
    block_number: Int.t;
    tx_hash: String.t;
    address: BigInt.t;
  }

  val create: block_number:Int.t -> tx_hash:String.t -> BigInt.t -> t
end

module FullTrace: sig
  type t = {
    trace: Trace.t;
    args: StackValue.t List.t;
    result: StackValue.t Option.t;
    env: Env.t;
  }
end

module Tagger: sig
  type t = FactDb.t -> FullTrace.t -> unit
end
