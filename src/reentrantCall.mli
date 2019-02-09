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


module ContractResult: sig
  type t = {
    address: String.t;
    reentrant_calls: (String.t * Int.t) List.t;
  }

  val to_string: t -> String.t
  val analyze_file: ?min_value:Int.t -> String.t -> t List.t
  val analyze: ?min_value:Int.t -> Yojson.Safe.t -> t
end
