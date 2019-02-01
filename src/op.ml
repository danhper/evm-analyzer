open Core

type t =
  | Stop       (* halts execution *)
  | Add        (* addition operation *)
  | Mul        (* multiplication operation *)
  | Sub        (* subtraction operation *)
  | Div        (* integer division operation *)
  | Sdiv       (* signed integer division operation *)
  | Mod        (* modulo remainder operation *)
  | Smod       (* signed modulo remainder operation *)
  | Addmod     (* unsigned modular addition *)
  | Mulmod     (* unsigned modular multiplication *)
  | Exp        (* exponential operation *)
  | Signextend (* extend length of signed integer *)

  | Lt      (* less-than comparison *)
  | Gt      (* greater-than comparison *)
  | Slt     (* signed less-than comparison *)
  | Sgt     (* signed greater-than comparison *)
  | Eq      (* equality comparison *)
  | Iszero  (* simple not operator *)
  | And     (* bitwise AND operation *)
  | Or      (* bitwise OR operation *)
  | Xor     (* bitwise XOR operation *)
  | Not     (* bitwise NOT operation *)
  | Byte    (* retrieve single byte from word *)
  | Shl     (* bitwise SHL operation *)
  | Shr     (* bitwise SHR operation *)
  | Sar     (* bitwise SAR operation *)

  | Keccak256    (* compute KECCAK-256 hash *)

  | Address         (* get address of currently executing account *)
  | Balance         (* get balance of the given account *)
  | Origin          (* get execution origination address *)
  | Caller          (* get caller address *)
  | Callvalue       (* get deposited value by the instruction/transaction responsible for this execution *)
  | Calldataload    (* get input data of current environment *)
  | Calldatasize    (* get size of input data in current environment *)
  | Calldatacopy    (* copy input data in current environment to memory *)
  | Codesize        (* get size of code running in current environment *)
  | Codecopy        (* copy code running in current environment to memory *)
  | Gasprice        (* get price of gas in current environment *)
  | Extcodesize     (* get external code size (from another contract) *)
  | Extcodecopy     (* copy external code (from another contract) *)
  | Returndatasize  (* get size of return data buffer *)
  | Returndatacopy  (* copy return data in current environment to memory *)
  | Extcodehash     (* get external code hash (from another contract) *)

  | Blockhash     (* get hash of most recent complete block *)
  | Coinbase      (* get the block's coinbase address *)
  | Timestamp     (* get the block's timestamp *)
  | Number        (* get the block's number *)
  | Difficulty    (* get the block's difficulty *)
  | Gaslimit      (* get the block's gas limit *)

  | Pop          (* remove item from stack *)
  | Mload        (* load word from memory *)
  | Mstore       (* save word to memory *)
  | Mstore8      (* save byte to memory *)
  | Sload        (* load word from storage *)
  | Sstore       (* save word to storage *)
  | Jump         (* alter the program counter *)
  | Jumpi        (* conditionally alter the program counter *)
  | Pc           (* the program counter *)
  | Msize        (* get the size of active memory *)
  | Gas          (* get the amount of available gas *)
  | Jumpdest     (* set a potential jump destination *)

  | Push of Int.t * BigInt.t (* plance n bytes item on the stacks *)

  | Dup of Int.t   (* copies the nth highest item in the stack to the top of the stack *)

  | Swap of Int.t  (* swaps the highest and second highest value on the stack *)

  | Log0        (* Makes a log entry; no topics. *)
  | Log1        (* Makes a log entry; 1 topic. *)
  | Log2        (* Makes a log entry; 2 topics. *)
  | Log3        (* Makes a log entry; 3 topics. *)
  | Log4        (* Makes a log entry; 4 topics. *)

  | Create        (* create a new account with associated code *)
  | Call          (* message-call into an account *)
  | Callcode      (* message-call with another account's code only *)
  | Return        (* halt execution returning output data *)
  | Delegatecall  (* like CALLCODE but keeps caller's value and sender *)
  | Create2       (* create new account with associated code at address `sha3(0xff + sender + salt + init code) % 2**160` *)
  | Staticcall    (* like CALL but disallow state modifications *)

  | Revert        (* halt execution, revert state and return output data *)
  | Invalid       (* invalid instruction for expressing runtime errors (e.g., division-by-zero) *)
  | Selfdestruct  (* halt execution and register account for later deletion *)

  | Unknown of String.t      (* unknown opcodes *)
  [@@deriving show { with_path = false }]

let of_string string =
  let get_length ~opcode string =
    let length = String.drop_prefix string (String.length opcode) in
    Int.of_string length
  in
  match string with
  | "STOP" -> Stop
  | "ADD" -> Add
  | "MUL" -> Mul
  | "SUB" -> Sub
  | "DIV" -> Div
  | "SDIV" -> Sdiv
  | "MOD" -> Mod
  | "SMOD" -> Smod
  | "ADDMOD" -> Addmod
  | "MULMOD" -> Mulmod
  | "EXP" -> Exp
  | "SIGNEXTEND" -> Signextend
  | "LT" -> Lt
  | "GT" -> Gt
  | "SLT" -> Slt
  | "SGT" -> Sgt
  | "EQ" -> Eq
  | "ISZERO" -> Iszero
  | "AND" -> And
  | "OR" -> Or
  | "XOR" -> Xor
  | "NOT" -> Not
  | "BYTE" -> Byte
  | "SHL" -> Shl
  | "SHR" -> Shr
  | "SAR" -> Sar
  | "KECCAK256" -> Keccak256
  | "ADDRESS" -> Address
  | "BALANCE" -> Balance
  | "ORIGIN" -> Origin
  | "CALLER" -> Caller
  | "CALLVALUE" -> Callvalue
  | "CALLDATALOAD" -> Calldataload
  | "CALLDATASIZE" -> Calldatasize
  | "CALLDATACOPY" -> Calldatacopy
  | "CODESIZE" -> Codesize
  | "CODECOPY" -> Codecopy
  | "GASPRICE" -> Gasprice
  | "EXTCODESIZE" -> Extcodesize
  | "EXTCODECOPY" -> Extcodecopy
  | "RETURNDATASIZE" -> Returndatasize
  | "RETURNDATACOPY" -> Returndatacopy
  | "EXTCODEHASH" -> Extcodehash
  | "BLOCKHASH" -> Blockhash
  | "COINBASE" -> Coinbase
  | "TIMESTAMP" -> Timestamp
  | "NUMBER" -> Number
  | "DIFFICULTY" -> Difficulty
  | "GASLIMIT" -> Gaslimit
  | "POP" -> Pop
  | "MLOAD" -> Mload
  | "MSTORE" -> Mstore
  | "MSTORE8" -> Mstore8
  | "SLOAD" -> Sload
  | "SSTORE" -> Sstore
  | "JUMP" -> Jump
  | "JUMPI" -> Jumpi
  | "PC" -> Pc
  | "MSIZE" -> Msize
  | "GAS" -> Gas
  | "JUMPDEST" -> Jumpdest
  | "LOG0" -> Log0
  | "LOG1" -> Log1
  | "LOG2" -> Log2
  | "LOG3" -> Log3
  | "LOG4" -> Log4
  | "CREATE" -> Create
  | "CALL" -> Call
  | "CALLCODE" -> Callcode
  | "RETURN" -> Return
  | "DELEGATECALL" -> Delegatecall
  | "CREATE2" -> Create2
  | "STATICCALL" -> Staticcall
  | "REVERT" -> Revert
  | "INVALID" -> Invalid
  | "SELFDESTRUCT" -> Selfdestruct
  | v when String.is_prefix ~prefix:"PUSH" v ->
    begin match String.split ~on:' ' v with
    | [push; raw_value] ->
      let value = if String.is_prefix ~prefix:"0x" raw_value
        then String.drop_prefix raw_value 2 else raw_value in
      let len = get_length ~opcode:"PUSH" push in
      let pos = 64 - (len * 2) in
      let final_value = BigInt.of_string_base 16 (String.sub value ~pos ~len:(len * 2)) in
      Push (len, final_value)
    | _ -> failwithf "invalid push format: %s" v ()
    end
  | v when String.is_prefix ~prefix:"DUP" v ->
    Dup (get_length ~opcode:"DUP" v)
  | v when String.is_prefix ~prefix:"SWAP" v ->
    Swap (get_length ~opcode:"SWAP" v)
  | v -> Unknown v

let size op = match op with
  | Push (n, _) -> n + 1
  | _ -> 1

let to_string t = match t with
  | Push (n, value) ->
    Printf.sprintf "PUSH%d %s" n (BigInt.format "#0x%x" value)
  | Dup n -> Printf.sprintf "DUP%d" n
  | Swap n -> Printf.sprintf "SWAP%d" n
  | _ -> String.uppercase (show t)


let input_count t = match t with
  | Stop | Address | Origin | Caller | Callvalue | Calldatasize | Codesize | Gasprice
  | Returndatasize | Coinbase | Timestamp | Number | Difficulty | Gaslimit
  | Pc | Msize | Gas | Jumpdest | Push _ | Dup _ | Swap _ | Invalid | Unknown _ -> 0

  | Iszero | Not | Balance | Calldataload | Extcodesize | Blockhash
  | Pop | Mload | Sload | Jump | Selfdestruct | Extcodehash -> 1

  | Add | Mul | Sub | Div | Sdiv | Mod | Smod | Exp | Signextend
  | Lt | Gt | Slt | Sgt | Eq | And | Or | Xor | Byte | Shl | Shr | Sar
  | Keccak256 | Mstore | Mstore8 | Sstore | Jumpi | Log0
  | Return | Revert -> 2

  | Addmod | Mulmod | Calldatacopy | Returndatacopy | Codecopy | Log1 | Create -> 3

  | Extcodecopy | Log2 | Create2 -> 4
  | Log3 -> 5
  | Log4 | Delegatecall | Staticcall-> 6
  | Call | Callcode -> 7


let has_result t = match t with
  | Dup _ | Swap _ | Stop | Unknown _ | Invalid
  | Calldatacopy | Codecopy | Extcodecopy | Returndatacopy
  | Pop | Mstore | Mstore8 | Sstore
  | Jump | Jumpi | Jumpdest
  | Log0 | Log1 | Log2 | Log3 | Log4 | Return | Revert | Selfdestruct -> false
  | _ -> true

let has_children t = match t with
  | Call | Staticcall | Delegatecall | Create | Create2 -> true
  | _ -> false

let execute_binary_op op a b =
  let open BigInt in
  match op with
  | Add -> a + b
  | Sub -> a - b
  | Mul -> a * b
  | Div -> a / b
  | Sdiv -> a / b
  | Exp -> pow a (to_int b)
  | _ -> failwith "op not supported"
