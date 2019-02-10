open Core

module Reentrancy: sig
  type t

  val to_string: t -> String.t
  val to_json: t -> Yojson.Safe.t
  val analyze_file: ?min_value:Float.t -> String.t -> t List.t
  val analyze: ?min_value:Float.t -> Yojson.Safe.t -> t
end

module UnhandledException: sig
  type t

  val to_string: t -> String.t
  val to_json: t -> Yojson.Safe.t
  val analyze_file: ?historical_balance:Bool.t -> ?min_balance:Float.t -> ?min_value:Float.t -> String.t -> t List.t Lwt.t
  val analyze: ?historical_balance:Bool.t -> ?min_value:Float.t -> rpc:EthRpc.t -> Yojson.Safe.t -> t Lwt.t
end
