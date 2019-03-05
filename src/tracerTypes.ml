open Core

module StackValue = struct
  type t = {
    value: BigInt.t;
    id: Int.t;
  } [@@deriving show { with_path = false }]

  let create ~id value = { value; id; }

  let copy t = t
end

module Env = struct
  type t = {
    stack: StackValue.t EStack.t;
    block_number: Int.t;
    tx_hash: String.t;
    address: BigInt.t;
  }

  let create ~block_number ~tx_hash address = {
    stack = EStack.create ~copy:StackValue.copy ();
    block_number;
    tx_hash;
    address;
  }
end

module FullTrace = struct
  type t = {
    trace: Trace.t;
    args: StackValue.t List.t;
    result: StackValue.t Option.t;
    env: Env.t;
  }
end

module Tagger = struct
  type t = FactDb.t -> FullTrace.t -> unit
end
