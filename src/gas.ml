let exp_gas = 10
let exp_byte_gas = 10
let sha3_gas = 30
let sha3_word_gas = 6
let sstore_set_gas = 20000
let sstore_reset_gas = 5000
let sstore_unchanged_gas = 200
let sstore_refund_gas = 15000
let sstore_refund_nonzero_gas = 4800
let jumpdest_gas = 1
let log_gas = 375
let log_data_gas = 8
let log_topic_gas = 375
let create_gas = 32000
let call_stipend = 2300
let call_value_transfer_gas = 9000
let call_new_account_gas = 25000
let suicide_refund_gas = 24000
let memory_gas = 3
let quad_coeff_div = 512
let create_data_gas = 200
let tx_gas = 21000
let tx_create_gas = 53000
let tx_data_zero_gas = 4
let tx_data_non_zero_gas = 68
let copy_gas = 3
let extcodesize_gas = 700
let extcodecopy_gas = 700
let balance_gas = 400
let sload_gas = 200
let call_gas = 700
let suicide_gas = 5000
let zero_tier_gas = 0
let base_tier_gas     = 2
let very_low_tier_gas = 3
let low_tier_gas      = 5
let mid_tier_gas      = 8
let high_tier_gas     = 10
let ext_tier_gas      = 20



let get_gas_cost ?stack:_stack ?storage:_storage op =
  let open Op in
  match op with
  | Stop       -> zero_tier_gas
  | Add        -> very_low_tier_gas
  | Mul        -> very_low_tier_gas
  | Sub        -> low_tier_gas
  | Div        -> low_tier_gas
  | Sdiv       -> low_tier_gas
  | Mod        -> low_tier_gas
  | Smod       -> low_tier_gas
  | Addmod     -> mid_tier_gas
  | Mulmod     -> mid_tier_gas
  | Exp        -> exp_gas  (* TODO: handle special *)
  | Signextend -> low_tier_gas

  | Lt     -> very_low_tier_gas
  | Gt     -> very_low_tier_gas
  | Slt    -> very_low_tier_gas
  | Sgt    -> very_low_tier_gas
  | Eq     -> very_low_tier_gas
  | Iszero -> very_low_tier_gas
  | And    -> very_low_tier_gas
  | Or     -> very_low_tier_gas
  | Xor    -> very_low_tier_gas
  | Not    -> very_low_tier_gas
  | Byte   -> very_low_tier_gas
  | Shl    -> very_low_tier_gas
  | Shr    -> very_low_tier_gas
  | Sar    -> very_low_tier_gas

  | Keccak256 -> high_tier_gas (* TODO: handle special *)

  | Address         -> base_tier_gas
  | Balance         -> high_tier_gas (* TODO: handle special *)
  | Origin          -> base_tier_gas
  | Caller          -> base_tier_gas
  | Callvalue       -> base_tier_gas
  | Calldataload    -> very_low_tier_gas
  | Calldatasize    -> base_tier_gas
  | Calldatacopy    -> very_low_tier_gas
  | Codesize        -> balance_gas
  | Codecopy        -> very_low_tier_gas
  | Gasprice        -> base_tier_gas
  | Extcodesize     -> high_tier_gas (* TODO: handle special *)
  | Extcodecopy     -> high_tier_gas (* TODO: handle special *)
  | Returndatasize  -> base_tier_gas
  | Returndatacopy  -> very_low_tier_gas
  | Extcodehash     -> high_tier_gas (* TODO: handle special *)

  | Blockhash     -> high_tier_gas (* TODO: handle special *)
  | Coinbase      -> base_tier_gas
  | Timestamp     -> base_tier_gas
  | Number        -> base_tier_gas
  | Difficulty    -> base_tier_gas
  | Gaslimit      -> base_tier_gas

  | Pop          -> base_tier_gas
  | Mload        -> very_low_tier_gas
  | Mstore       -> very_low_tier_gas
  | Mstore8      -> very_low_tier_gas
  | Sload        -> sload_gas (* TODO: handle special *)
  | Sstore       -> sstore_set_gas (* TODO: handle special *)
  | Jump         -> mid_tier_gas
  | Jumpi        -> high_tier_gas
  | Pc           -> base_tier_gas
  | Msize        -> base_tier_gas
  | Gas          -> base_tier_gas
  | Jumpdest     -> base_tier_gas (* TODO: handle special *)

  | Push (_n, _value) -> very_low_tier_gas

  | Dup _n   -> very_low_tier_gas

  | Swap _n  -> very_low_tier_gas

  | Log0
  | Log1
  | Log2
  | Log3
  | Log4    -> log_topic_gas (* TODO: handle special *)

  | Create        -> create_gas (* TODO: handle special *)
  | Call          -> call_gas (* TODO: handle special *)
  | Callcode      -> call_gas (* TODO: handle special *)
  | Return        -> zero_tier_gas
  | Delegatecall  -> call_gas (* TODO: handle special *)
  | Create2       -> create_gas (* TODO: handle special *)
  | Staticcall    -> call_gas (* TODO: handle special *)

  | Revert        -> very_low_tier_gas (* TODO: handle special *)
  | Invalid       -> zero_tier_gas
  | Selfdestruct  -> suicide_gas (* TODO: handle special *)

  | Unknown _op -> zero_tier_gas
