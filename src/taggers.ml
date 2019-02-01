open Core
open TracerTypes
open FullTrace

let with_result f db full_trace =
  Option.iter ~f:(fun res -> f db res full_trace) full_trace.result

let tag_output' db result { args; _ } =
  let f arg = FactDb.add_rel2 db "is_output" (result.StackValue.id, arg.StackValue.id) in
  List.iter ~f args
let tag_output = with_result tag_output'

let tag_storage' db result { trace; _ } =
  match trace.Trace.op with
  | Sload -> FactDb.add_rel1 db "uses_storage" result.StackValue.id;
  | _ -> ()
let tag_storage = with_result tag_storage'

let tag_uint_size' db result { trace; args; _ } =
  match trace.Trace.op, args with
  | Op.And, [a; b] ->
    let succ_a = BigInt.add a.StackValue.value BigInt.one in
    if a.StackValue.value < b.StackValue.value &&
      BigInt.is_power succ_a ~power:16 &&
      BigInt.is_power (BigInt.of_int (BigInt.log ~base:16 succ_a)) ~power:2
        then FactDb.add_rel2 db "has_uint_size" (result.StackValue.id, BigInt.log ~base:2 succ_a)
  | _ -> ()
let tag_uint_size = with_result tag_uint_size'

let tag_signed db { trace; args; _ } =
  match trace.Trace.op, args with
  | Op.Signextend, [_bits; value] ->
    FactDb.add_rel1 db "is_signed_operand" value.StackValue.id
  | (Op.Sdiv | Op.Smod | Op.Slt | Op.Sgt), [a; b] ->
    FactDb.add_rel1 db "is_signed_operand" a.StackValue.id;
    FactDb.add_rel1 db "is_signed_operand" b.StackValue.id
  | _ -> ()

let tag_int_size db { trace; args; _ } =
  match trace.Trace.op, args with
  | Op.Signextend, [bits; value] ->
    let int_size = ((BigInt.to_int bits.StackValue.value) + 1) * 8 in
    FactDb.add_rel2 db "has_int_size" (value.StackValue.id, int_size)
  | _ -> ()

let tag_used_in_condition db { trace; args; _ } =
  match trace.Trace.op, args with
  | Op.Jumpi, [_dest; condition] ->
    FactDb.add_rel1 db "used_in_condition" condition.StackValue.id
  | _ -> ()


let tag_overflow ~get_size ~cast_value ~name db result { trace; args; _ } =
  let open StackValue in
  match trace.Trace.op, args with
  | (Op.Add | Op.Sub | Op.Mul | Op.Div | Op.Sdiv | Op.Exp) as op, [a; b] ->
    begin match get_size a.id, get_size b.id with
    | Some bits_a, Some bits_b ->
      let output_bits = Int.max bits_a bits_b in
      let expected_result = cast_value (Op.execute_binary_op op a.value b.value) output_bits in
      if expected_result <> result.value then
        FactDb.add_rel1 db name result.id
    | _ -> ()
    end
  | _ -> ()

let tag_signed_overflow' db result full_trace =
  let get_size id = FactDb.get_int db 1 (Printf.sprintf "int_size(%d, N)" id) in
  tag_overflow ~get_size ~cast_value:BigInt.twos_complement ~name:"is_signed_overflow"
                db result full_trace
let tag_signed_overflow = with_result tag_signed_overflow'

let tag_unsigned_overflow' db result full_trace =
  let get_size id = FactDb.get_int db 1 (Printf.sprintf "uint_size(%d, N)" id) in
  tag_overflow ~get_size ~cast_value:BigInt.limit_bits ~name:"is_unsigned_overflow"
                db result full_trace
let tag_unsigned_overflow = with_result tag_unsigned_overflow'


let all = [
  [tag_output;
   tag_storage;
   tag_uint_size;
   tag_used_in_condition;
   tag_signed;
   tag_int_size;
  ];

  [tag_signed_overflow;
   tag_unsigned_overflow;
  ]
]
