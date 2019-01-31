open Core
open TracerTypes
open FullTrace

let with_result f db full_trace = match full_trace.result with
  | None -> ()
  | Some res -> f db res full_trace

let tag_output' db result { args; _ } =
  let f arg = FactDb.add_rel2 db "is_output" (result.StackValue.id, arg.StackValue.id) in
  List.iter ~f args
let tag_output = with_result tag_output'

let tag_storage' db result { trace; _ } =
  match trace.Trace.op with
  | Sload -> FactDb.add_rel1 db "uses_storage" result.StackValue.id;
  | _ -> ()
let tag_storage = with_result tag_storage'

let tag_overflow' db result { trace; args; _ } =
  match trace.Trace.op, args with
  | Op.And, [first; second] ->
    let a = first.StackValue.value in
    let b = second.StackValue.value in
    let succ_a = BigInt.add a BigInt.one in
    if a < b &&
      BigInt.is_power succ_a ~power:16 &&
      BigInt.is_power (BigInt.of_int (BigInt.log ~base:16 succ_a)) ~power:2
        then FactDb.add_rel1 db "is_overflow" result.StackValue.id
  | _ -> ()
let tag_overflow = with_result tag_overflow'

let tag_used_in_condition db { trace; args; _ } =
  match trace.Trace.op, args with
  | Op.Jumpi, [_dest; condition] ->
    FactDb.add_rel1 db "used_in_condition" condition.StackValue.id
  | _ -> ()

let all = [tag_output; tag_storage; tag_overflow; tag_used_in_condition]
