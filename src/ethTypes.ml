module Transaction = struct
  type t = {
    block_hash: string [@key "blockHash"];
    block_number: int [@key "blockNumber"];
    from: string;
    gas: int64;
    gasPrice: int64;
    hash: string;
    input: string;
    nonce: int;
    r: string;
    s: string;
    to_: string [@key "to"];
    transaction_index: string [@key "transactionIndex"];
    v: string;
    value: BigInt.t;
  } [@@deriving rpcty]
end

module ExecutionResult = struct
  type t = {
    gas: int;
    failed: bool;
    truncated: bool;
    traces: Trace.t list;
  }

  let from_json json =
    let open Yojson.Safe.Util in
    {
      gas = json |> member "gas" |> to_int;
      failed = json |> member "failed" |> to_bool;
      truncated = json |> member "truncated" |> to_bool;
      traces = TraceParser.parse_json (member "structLogs" json)
    }
end

module SimpleTransaction = struct
  type t = {
    block_hash: string;
    block_number: int;
    to_: string;
    gas: int;
    gas_price: string;
    result: ExecutionResult.t;
  }

  let from_json json =
    let open Yojson.Safe.Util in
    {
      block_hash = json |> member "hash" |> to_string;
      block_number = json |> member "blockNumber" |> to_int;
      to_ = json |> member "to" |> to_string;
      gas = json |> member "gas" |> to_int;
      gas_price = json |> member "gasPrice" |> to_string;
      result = ExecutionResult.from_json (member "executionResult" json)
    }

  let from_string raw_string =
    let json = Yojson.Safe.from_string raw_string in
    from_json json
end
