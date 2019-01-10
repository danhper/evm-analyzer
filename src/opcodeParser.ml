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
    Int.Hex.of_string ("0x" ^ bytecode)

let consume_opcode t = consume ~bytes:1 t

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

  | 0x60 -> Push1 (consume t ~bytes:1)
  | 0x61 -> Push2 (consume t ~bytes:2)
  | 0x62 -> Push3 (consume t ~bytes:3)
  | 0x63 -> Push4 (consume t ~bytes:4)
  | 0x64 -> Push5 (consume t ~bytes:5)
  | 0x65 -> Push6 (consume t ~bytes:6)
  | 0x66 -> Push7 (consume t ~bytes:7)
  | 0x67 -> Push8 (consume t ~bytes:8)
  | 0x68 -> Push9 (consume t ~bytes:9)
  | 0x69 -> Push10 (consume t ~bytes:10)
  | 0x6a -> Push11 (consume t ~bytes:11)
  | 0x6b -> Push12 (consume t ~bytes:12)
  | 0x6c -> Push13 (consume t ~bytes:13)
  | 0x6d -> Push14 (consume t ~bytes:14)
  | 0x6e -> Push15 (consume t ~bytes:15)
  | 0x6f -> Push16 (consume t ~bytes:16)
  | 0x70 -> Push17 (consume t ~bytes:17)
  | 0x71 -> Push18 (consume t ~bytes:18)
  | 0x72 -> Push19 (consume t ~bytes:19)
  | 0x73 -> Push20 (consume t ~bytes:20)
  | 0x74 -> Push21 (consume t ~bytes:21)
  | 0x75 -> Push22 (consume t ~bytes:22)
  | 0x76 -> Push23 (consume t ~bytes:23)
  | 0x77 -> Push24 (consume t ~bytes:24)
  | 0x78 -> Push25 (consume t ~bytes:25)
  | 0x79 -> Push26 (consume t ~bytes:26)
  | 0x7a -> Push27 (consume t ~bytes:27)
  | 0x7b -> Push28 (consume t ~bytes:28)
  | 0x7c -> Push29 (consume t ~bytes:29)
  | 0x7d -> Push30 (consume t ~bytes:30)
  | 0x7e -> Push31 (consume t ~bytes:31)
  | 0x7f -> Push32 (consume t ~bytes:32)

  | 0x80 -> Dup1
  | 0x81 -> Dup2
  | 0x82 -> Dup3
  | 0x83 -> Dup4
  | 0x84 -> Dup5
  | 0x85 -> Dup6
  | 0x86 -> Dup7
  | 0x87 -> Dup8
  | 0x88 -> Dup9
  | 0x89 -> Dup10
  | 0x8a -> Dup11
  | 0x8b -> Dup12
  | 0x8c -> Dup13
  | 0x8d -> Dup14
  | 0x8e -> Dup15
  | 0x8f -> Dup16

  | 0x90 -> Swap1
  | 0x91 -> Swap2
  | 0x92 -> Swap3
  | 0x93 -> Swap4
  | 0x94 -> Swap5
  | 0x95 -> Swap6
  | 0x96 -> Swap7
  | 0x97 -> Swap8
  | 0x98 -> Swap9
  | 0x99 -> Swap10
  | 0x9a -> Swap11
  | 0x9b -> Swap12
  | 0x9c -> Swap13
  | 0x9d -> Swap14
  | 0x9e -> Swap15
  | 0x9f -> Swap16

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
