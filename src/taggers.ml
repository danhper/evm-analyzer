open Core
open TracerTypes
open FullTrace

let with_result f db full_trace = match full_trace.result with
  | None -> ()
  | Some res -> f db res full_trace

let tag_output' db result { args; _ } =
  let f arg = Db.add_rel2 db "is_output" (result.StackValue.id, arg.StackValue.id) in
  List.iter ~f args
let tag_output = with_result tag_output'

let tag_storage' db result { trace; _ } =
  match trace.Trace.op with
  | Sload -> Db.add_rel1 db "uses_storage" result.StackValue.id;
  | _ -> ()
let tag_storage = with_result tag_storage'

let tag_overflow' db result { trace; args; _ } =
  match trace.Trace.op, args with
  | Op.And, [left; right] ->
    let a = left.StackValue.value in
    let b = right.StackValue.value in
    let succ_b = BigInt.add b BigInt.one in
    if a > b &&
      BigInt.is_power succ_b ~power:16 &&
      BigInt.is_power (BigInt.of_int (BigInt.log ~base:16 succ_b)) ~power:2
        then
          let () = Printf.printf "%s %s\n" (BigInt.format "#0x%x" a) (BigInt.format "#0x%x" b) in
          Db.add_rel1 db "is_overflow" result.StackValue.id
  | _ -> ()
let tag_overflow = with_result tag_overflow'

let all = [tag_output; tag_storage; tag_overflow]
