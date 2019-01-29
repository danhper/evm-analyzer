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

  | Push of Int.t * String.t (* plance n bytes item on the stacks *)

  | Dup of Int.t   (* copies the nth highest item in the stack to the top of the stack *)

  | Swap of Int.t  (* swaps the highest and second highest value on the stack *)

  | Log0        (* Makes a log entry; no topics. *)
  | Log1        (* Makes a log entry; 1 topic. *)
  | Log2        (* Makes a log entry; 2 topics. *)
  | Log3        (* Makes a log entry; 3 topics. *)
  | Log4        (* Makes a log entry; 4 topics. *)

  | Jumpto             (* alter the program counter to a jumpdest -- not part of Instructions.cpp *)
  | Jumpif             (* conditionally alter the program counter -- not part of Instructions.cpp *)
  | Jumpv              (* alter the program counter to a jumpdest -- not part of Instructions.cpp *)
  | Jumpsub            (* alter the program counter to a beginsub -- not part of Instructions.cpp *)
  | Jumpsubv           (* alter the program counter to a beginsub -- not part of Instructions.cpp *)
  | Beginsub           (* set a potential jumpsub destination -- not part of Instructions.cpp *)
  | Begindata          (* begin the data section -- not part of Instructions.cpp *)
  | Returnsub          (* return to subroutine jumped from -- not part of Instructions.cpp *)
  | Putlocal           (* pop top of stack to local variable -- not part of Instructions.cpp *)
  | Getlocal           (* push local variable to top of stack -- not part of Instructions.cpp *)

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
  | "JUMPTO" -> Jumpto
  | "JUMPIF" -> Jumpif
  | "JUMPV" -> Jumpv
  | "JUMPSUB " -> Jumpsub
  | "JUMPSUBV" -> Jumpsubv
  | "BEGINSUB" -> Beginsub
  | "BEGINDATA" -> Begindata
  | "RETURNSUB" -> Returnsub
  | "PUTLOCAL" -> Putlocal
  | "GETLOCAL" -> Getlocal
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
      let final_value = "0x" ^ (String.sub value ~pos:0 ~len) in
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
    Printf.sprintf "PUSH%d %s" n value
  | Dup n -> Printf.sprintf "DUP%d" n
  | Swap n -> Printf.sprintf "SWAP%d" n
  | _ -> String.uppercase (show t)
