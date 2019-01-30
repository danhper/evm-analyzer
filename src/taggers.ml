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

let all = [tag_output ;tag_storage]
