open Core

type t = {
  bytecode: string;
  mutable index: int;
}

let length t = String.length t.bytecode
let is_empty t = t.index >= length t

let consume t ~bytes =
  let len = bytes * 2 in
  if t.index + len > length t then
    let left_bytecode = String.sub t.bytecode ~pos:t.index ~len:(length t - t.index) in
    failwith (Printf.sprintf "cound not get %d bytes from %s" len left_bytecode)
  else
    let bytecode = String.sub t.bytecode ~pos:t.index ~len in
    t.index <- t.index + len;
    "0x" ^ bytecode

let consume_opcode t = Int.Hex.of_string (consume ~bytes:1 t)

let consume_full_opcode t =
  let open Opcode in
  let opcode = consume_opcode t in
  match opcode with
  | 0x00 -> Stop
  | 0x01 -> Add
  | 0x02 -> Mul
  | 0x03 -> Sub
  | 0x04 -> Div
  | 0x05 -> Sdiv
  | 0x06 -> Mod
  | 0x07 -> Smod
  | 0x08 -> Addmod
  | 0x09 -> Mulmod
  | 0x0a -> Exp
  | 0x0b -> Signextend

  | 0x10 -> Lt
  | 0x11 -> Gt
  | 0x12 -> Slt
  | 0x13 -> Sgt
  | 0x14 -> Eq
  | 0x15 -> Iszero
  | 0x16 -> And
  | 0x17 -> Or
  | 0x18 -> Xor
  | 0x19 -> Not
  | 0x1a -> Byte
  | 0x1b -> Shl
  | 0x1c -> Shr
  | 0x1d -> Sar

  | 0x20 -> Keccak256

  | 0x30 -> Address
  | 0x31 -> Balance
  | 0x32 -> Origin
  | 0x33 -> Caller
  | 0x34 -> Callvalue
  | 0x35 -> Calldataload
  | 0x36 -> Calldatasize
  | 0x37 -> Calldatacopy
  | 0x38 -> Codesize
  | 0x39 -> Codecopy
  | 0x3a -> Gasprice
  | 0x3b -> Extcodesize
  | 0x3c -> Extcodecopy
  | 0x3d -> Returndatasize
  | 0x3e -> Returndatacopy
  | 0x3f -> Extcodehash

  | 0x40 -> Blockhash
  | 0x41 -> Coinbase
  | 0x42 -> Timestamp
  | 0x43 -> Number
  | 0x44 -> Difficulty
  | 0x45 -> Gaslimit

  | 0x50 -> Pop
  | 0x51 -> Mload
  | 0x52 -> Mstore
  | 0x53 -> Mstore8
  | 0x54 -> Sload
  | 0x55 -> Sstore
  | 0x56 -> Jump
  | 0x57 -> Jumpi
  | 0x58 -> Pc
  | 0x59 -> Msize
  | 0x5a -> Gas
  | 0x5b -> Jumpdest

  | n when n >= 0x60 && n <= 0x7f ->
    let bytes = (n - 0x60 + 1) in
    Push (bytes, (consume t ~bytes))

  | n when n >= 0x80 && n <= 0x8f -> Dup (n - 0x80 + 1)

  | n when n >= 0x90 && n <= 0x9f -> Swap (n - 0x90 + 1)

  | 0xa0 -> Log0
  | 0xa1 -> Log1
  | 0xa2 -> Log2
  | 0xa3 -> Log3
  | 0xa4 -> Log4

  | 0xb0 -> Jumpto
  | 0xb1 -> Jumpif
  | 0xb2 -> Jumpv
  | 0xb3 -> Jumpsub
  | 0xb4 -> Jumpsubv
  | 0xb5 -> Beginsub
  | 0xb6 -> Begindata
  | 0xb7 -> Returnsub
  | 0xb8 -> Putlocal
  | 0xb9 -> Getlocal

  | 0xf0 -> Create
  | 0xf1 -> Call
  | 0xf2 -> Callcode
  | 0xf3 -> Return
  | 0xf4 -> Delegatecall
  | 0xf5 -> Create2
  | 0xfa -> Staticcall

  | 0xfd -> Revert
  | 0xfe -> Invalid
  | 0xff -> Selfdestruct

  | v -> Unknown (Printf.sprintf "0x%x" v)


let normalize_bytecode bytecode =
  let drop_0x_prefix bytecode =
    if String.is_prefix ~prefix:"0x" bytecode
      then String.drop_prefix bytecode 2
      else bytecode
  in
  (* solidity swarm hash: 0xa1 0x65 'b' 'z' 'z' 'r' '0' 0x58 0x20 <32 bytes swarm hash> 0x00 0x29 *)
  let drop_swarm_hash bytecode =
    let c = Fn.compose (Printf.sprintf "%x") Char.to_int in
    let hash_start = String.concat ["a1"; "65"; c 'b'; c 'z'; c 'z'; c 'r'; c '0'; "58"; "20"] in
    let hash_start_length = String.length hash_start in
    let hash_length = hash_start_length + 32 * 2 + 4 in
    if String.is_suffix ~suffix:"0029" bytecode &&
      String.sub ~pos:(String.length bytecode - hash_length)
                 ~len:hash_start_length bytecode = hash_start
      then String.sub ~pos:0 ~len:(String.length bytecode - hash_length) bytecode
      else bytecode
  in
  bytecode |> String.strip |> drop_0x_prefix |> drop_swarm_hash

let parse_bytecode raw_bytecode =
  let bytecode = normalize_bytecode raw_bytecode in
  let parser = { bytecode; index = 0 } in
  let rec loop acc =
    if is_empty parser
      then List.rev acc
      else loop (consume_full_opcode parser :: acc)
  in
  loop []
