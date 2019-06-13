val exp_gas: int
val exp_byte_gas: int
val sha3_gas: int
val sha3_word_gas: int
val sstore_set_gas: int
val sstore_reset_gas: int
val sstore_unchanged_gas: int
val sstore_refund_gas: int
val sstore_refund_nonzero_gas: int
val jumpdest_gas: int
val log_gas: int
val log_data_gas: int
val log_topic_gas: int
val create_gas: int
val call_stipend: int
val call_value_transfer_gas: int
val call_new_account_gas: int
val suicide_refund_gas: int
val memory_gas: int
val quad_coeff_div: int
val create_data_gas: int
val tx_gas: int
val tx_create_gas: int
val tx_data_zero_gas: int
val tx_data_non_zero_gas: int
val copy_gas: int
val extcodesize_gas: int
val extcodecopy_gas: int
val balance_gas: int
val sload_gas: int
val call_gas: int
val suicide_gas: int
val zero_tier_gas: int
val base_tier_gas: int
val very_low_tier_gas: int
val low_tier_gas: int
val mid_tier_gas: int
val high_tier_gas: int
val ext_tier_gas: int


val get_gas_cost:
  ?stack:TracerTypes.EVMStack.t ->
  ?storage:TracerTypes.EVMStorage.t ->
  Op.t ->
  int
