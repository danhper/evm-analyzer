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
