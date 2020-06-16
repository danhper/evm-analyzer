open Core
open TracerTypes
open FullTrace

let with_result f db full_trace =
  Option.iter ~f:(fun res -> f db res full_trace) full_trace.result

let tag_output' db result { args; _ } =
  let f arg = FactDb.add_int_rel2 db "is_output" (result.StackValue.id, arg.StackValue.id) in
  List.iter ~f args
let tag_output = with_result tag_output'

let tag_storage' db result { trace; _ } =
  match trace.Trace.op with
  | Sload -> FactDb.add_int_rel1 db "uses_storage" result.StackValue.id;
  | _ -> ()
let tag_storage = with_result tag_storage'

let tag_uint_size' db _result { trace; args; _ } =
  match trace.Trace.op, args with
  | Op.And, [a; b] ->
    let succ_a = BigInt.add a.StackValue.value BigInt.one in
    if a.StackValue.value < b.StackValue.value &&
      BigInt.is_power succ_a ~power:16 &&
      BigInt.is_power (BigInt.of_int (BigInt.log ~base:16 succ_a)) ~power:2
        then FactDb.add_int_rel2 db "has_uint_size" (b.StackValue.id, BigInt.log ~base:2 succ_a)
  | _ -> ()
let tag_uint_size = with_result tag_uint_size'

let tag_signed db { trace; args; _ } =
  match trace.Trace.op, args with
  | Op.Signextend, [_bits; value] ->
    FactDb.add_int_rel1 db "is_signed_operand" value.StackValue.id
  (* | (Op.Sdiv | Op.Smod | Op.Slt | Op.Sgt), [a; b] ->
    FactDb.add_int_rel1 db "is_signed_operand" a.StackValue.id;
    FactDb.add_int_rel1 db "is_signed_operand" b.StackValue.id *)
  | _ -> ()

let tag_int_size db { trace; args; _ } =
  match trace.Trace.op, args with
  | Op.Signextend, [bits; value] ->
    let int_size = ((BigInt.to_int bits.StackValue.value) + 1) * 8 in
    FactDb.add_int_rel2 db "has_int_size" (value.StackValue.id, int_size)
  | _ -> ()

let tag_used_in_condition db { trace; args; _ } =
  match trace.Trace.op, args with
  | Op.Jumpi, [_dest; condition] ->
    FactDb.add_int_rel1 db "used_in_condition" condition.StackValue.id
  | _ -> ()


let bigint_value_count = BigInt.(pow two 256)
let bigint_max_value = BigInt.(bigint_value_count - one)
let tag_overflow'' db result { trace; args; _ } =
  let open StackValue in
  let is_gas id = FactDb.get_bool db (Printf.sprintf "is_gas(%d)" id) in
  let negated_const id = FactDb.get_bool db (Printf.sprintf "negated_const(%d)" id) in
  let should_do_check a b =
      not (is_gas a.id || is_gas b.id) (* gas related computation are inserted by the compiler *)
      && not (negated_const a.id || negated_const b.id) (* compiler inserts ADD (NOT CONST) *)
      (* TODO: check for sload/mload *)
  in
  match trace.Trace.op, args with
  (* compiler inserted patterns *)
  | Op.Add, (a :: b :: _)
      when a.value = bigint_max_value || b.value = bigint_max_value ||
           a.value = BigInt.zero || b.value = BigInt.zero -> ()
  | Op.Sub, (a :: b :: _)
      when a.value = BigInt.zero && b.value = BigInt.one -> ()
  | (Op.Add | Op.Sub | Op.Mul | (* Op.Div | Op.Sdiv | *) Op.Exp) as op, [a; b]
      when should_do_check a b ->
    let actual_result = result.StackValue.value in
    let expected_result = Op.execute_binary_op op a.value b.value in
    if expected_result >= BigInt.zero && expected_result <= BigInt.of_int 127
      then ()
      else
        let is_signed = FactDb.get_bool db (Printf.sprintf "is_signed(%d)" result.StackValue.id) in
        let (bits, expected_result) =
          if is_signed then
            let size = FactDb.get_int db 1 (Printf.sprintf "int_size(%d, N)" result.id) in
            let output_bits = Option.value ~default:256 size in
            (output_bits, BigInt.twos_complement actual_result output_bits)
          else
            let size = FactDb.get_int db 1 (Printf.sprintf "uint_size(%d, N)" result.id) in
            let output_bits = Option.value ~default:256 size in
            (output_bits, if expected_result > BigInt.zero then
              BigInt.limit_bits actual_result output_bits else expected_result)
        in
        if expected_result <> actual_result then
          (* let _ = BigInt.(Printf.printf "%s %s %s = %s (!= %s), (%d bits)\n"
            (to_string a.value) (Op.to_infix_string op) (to_string b.value)
            (to_string expected_result) (to_string actual_result) bits) in *)
          FactDb.add_rel5 db FactDb.Relations.overflow (result.id, is_signed, bits, expected_result, actual_result)
  | _ -> ()

let tag_const' db result { trace; _ } =
  match trace.op with
  | Op.Push _ ->
    FactDb.add_int_rel1 db "const" result.StackValue.id
  | _ -> ()
let tag_const = with_result tag_const'


let tag_not' db result { trace; _ } =
  match trace.op with
  | Op.Not -> FactDb.add_int_rel1 db "not" result.StackValue.id
  | _ -> ()
let tag_not = with_result tag_not'


let tag_overflow' db result ({trace; _} as full_trace) =
  match trace.op with
  | (Op.Add | Op.Sub | Op.Mul | Op.Div | Op.Sdiv | Op.Exp) ->
    tag_overflow'' db result full_trace
  | _ -> ()
let tag_overflow = with_result tag_overflow'

let tag_failed_call' db result { trace; args; _ } =
  let open StackValue in
  let failed_call_rel = FactDb.Relations.failed_call in
  match trace.op, args, result.StackValue.value with
  | Op.Call, _ :: { value = address; _ } :: { value = value; _ } :: _ , v when v = BigInt.zero ->
    FactDb.add_rel3 db failed_call_rel (result.StackValue.id, address, value)
  | _ -> ()
let tag_failed_call = with_result tag_failed_call'

let tag_empty_delegate_call' db result { trace; args; _ } =
  let open StackValue in
  let call_entry_rel = FactDb.get_rel2 "call_entry" ~k1:FactDb.Types.int ~k2:FactDb.Types.bigint_key in
  match trace.op, args, result with
  | (Op.Delegatecall | Op.Staticcall),
        { id = top_id; _ ;} :: { value = address; _; } :: _, { id = result_id; _; } ->
    FactDb.add_rel2 db call_entry_rel (top_id, address);
    FactDb.add_int_rel1 db "call_exit" result_id;
  | _ -> ()
let tag_empty_delegate_call = with_result tag_empty_delegate_call'

let tag_call' db result { trace; env; args; _ } =
  let module T = FactDb.Types in
  let open Op in
  match trace.op, args with
  | (Call | Callcode), (gas :: addr :: value :: _rest)
      when result.StackValue.value = BigInt.one ->
    let db_args = (gas.StackValue.id, env.Env.address, addr.value, value.value) in
    FactDb.add_rel4 db FactDb.Relations.direct_call db_args
  | _ -> ()
let tag_call = with_result tag_call'

let tag_tx_sstore db { trace; env; args; _ } =
  match trace.op, args with
  | Op.Sstore, key :: _ ->
    let db_args = (env.Env.block_number, env.Env.tx_hash, key.value) in
    FactDb.add_rel3 db FactDb.Relations.tx_sstore db_args
  | _ -> ()

let tag_tx_sload db { trace; env; args; _ } =
  match trace.op, args with
  | Op.Sload, key :: _ ->
    let db_args = (env.Env.block_number, env.Env.tx_hash, key.value) in
    FactDb.add_rel3 db FactDb.Relations.tx_sload db_args
  | _ -> ()

let tag_gas' db result { trace; _ } =
  match trace.op with
  | Op.Gas -> FactDb.add_int_rel1 db "is_gas" result.StackValue.id
  | _ -> ()
let tag_gas = with_result tag_gas'


let all = [
  [tag_output;
   tag_storage;
   tag_uint_size;
   tag_used_in_condition;
   tag_signed;
   tag_int_size;
   tag_failed_call;
   tag_empty_delegate_call;
   tag_call;
   tag_const;
   tag_not;
  ];

  [tag_overflow;]
]

let for_vulnerability vulnerability_type = match vulnerability_type with
  | "integer-overflow" ->
    [[tag_output; tag_uint_size; tag_int_size; tag_signed;
      tag_gas; tag_const; tag_not];
     [tag_overflow];
    ]
  | "unhandled-exception" ->
    [[tag_output; tag_failed_call; tag_used_in_condition;]]
  | "reentrancy" ->
    [[tag_call;]]
  | "locked-ether" ->
    [[tag_empty_delegate_call;]]
  | "tod" ->
    [[tag_tx_sload; tag_tx_sstore;]]
  | _ -> failwithf "unknown vulnerability %s" vulnerability_type ()
