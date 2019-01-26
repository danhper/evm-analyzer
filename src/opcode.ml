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

let size opcode = match opcode with
  | Push (n, _) -> n + 1
  | _ -> 1

let to_string t = match t with
  | Push (n, value) ->
    Printf.sprintf "PUSH%d %s" n value
  | Dup n -> Printf.sprintf "DUP%d" n
  | Swap n -> Printf.sprintf "SWAP%d" n
  | _ -> String.uppercase (show t)
