open Core

module StackValue: sig
  type t = {
    value: BigInt.t;
    id: Int.t;
  } [@@deriving show]

  val create: id:Int.t -> BigInt.t -> t

  val copy: t -> t
end

module FullTrace: sig
  type t = {
    trace: Trace.t;
    args: StackValue.t List.t;
    result: StackValue.t Option.t;
  }
end

module Tagger: sig
  type t = Db.t -> FullTrace.t -> unit
end

module Env: sig
  type t = {
    stack: StackValue.t EStack.t;
  }

  val create: unit -> t
end
