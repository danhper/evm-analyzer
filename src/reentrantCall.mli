open Core

type t = {
  alice: String.t;
  bob: String.t;
  alice_amount: BigInt.t;
  bob_amount: BigInt.t;
  alice_calls: Int.t;
  bob_calls: Int.t;
}

val aggregate_calls: FactDb.t -> t List.t
val to_json: t -> Yojson.Safe.t
val of_json: Yojson.Safe.t -> t
