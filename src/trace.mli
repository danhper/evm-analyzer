open Core

type t = {
  gas: Int.t;
  gas_cost: Int.t;
  op: Op.t;
  depth: Int.t;
  pc: Int.t;
  result: BigInt.t Option.t;
}
