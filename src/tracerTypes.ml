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
  type t = [%import: TracerTypes.Env.t]

  let create address = {
    stack = EStack.create ~copy:StackValue.copy ();
    address;
  }
end

module FullTrace = struct
  type t = [%import: TracerTypes.FullTrace.t]
end

module Tagger = struct
  type t = [%import: TracerTypes.Tagger.t]
end
