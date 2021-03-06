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
    struct_logs: Yojson.Safe.t;
  }

  let from_json json =
    let open Yojson.Safe.Util in
    let struct_logs = member "structLogs" json in
    {
      gas = json |> member "gas" |> to_int;
      failed = json |> member "failed" |> to_bool;
      truncated = json |> member "truncated" |> to_bool;
      traces = TraceParser.parse_json struct_logs;
      struct_logs;
    }
end

module SimpleTransaction = struct
  type t = {
    hash: string;
    block_number: int;
    to_: string;
    to_balance: string;
    value: string;
    gas: int;
    gas_price: string;
    result: ExecutionResult.t;
  }

  let from_json json =
    let open Yojson.Safe.Util in
    {
      hash = json |> member "hash" |> to_string;
      block_number = json |> member "blockNumber" |> to_int;
      to_ = json |> member "to" |> to_string;
      to_balance = json |> member "toBalance" |> to_string;
      value = json |> member "value" |> to_string;
      gas = json |> member "gas" |> to_int;
      gas_price = json |> member "gasPrice" |> to_string;
      result = ExecutionResult.from_json (member "executionResult" json)
    }

  let to_json t = `Assoc [
      ("hash", `String t.hash);
      ("blockNumber", `Int t.block_number);
      ("to", `String t.to_);
      ("toBalance", `String t.to_balance);
      ("value", `String t.value);
      ("gas", `Int t.gas);
      ("gasPrice", `String t.gas_price);
    ]

  let from_string raw_string =
    let json = Yojson.Safe.from_string raw_string in
    from_json json
end
