open Core
open TracerTypes

type t = {
  contract_address: String.t;
  tx_hash: String.t;
  block_number: Int.t;
  taggers: Tagger.t List.t List.t;
}

let create ~block_number ~tx_hash ~taggers contract_address =
  { contract_address; taggers; tx_hash; block_number; }

let log ~debug ~env trace =
  let open Trace in
  if debug then
    let stack_str = EStack.to_string ~f:StackValue.show env.Env.stack in
    let op_code_str = Op.to_string trace.op in
    Out_channel.printf "%d: %s %s\n" trace.pc op_code_str stack_str

let nested_call_address op args =
  let open Op in
  match op, args with
  | (Call | Callcode | Staticcall | Delegatecall), (_gas :: addr :: _rest) ->
    Some (addr.StackValue.value)
  | _ -> None

let execute_traces ?debug:(debug=false) ?db t traces =
  let db = Option.value ~default:(FactDb.create ()) db in
  let rec execute_trace ~env ~taggers trace =
    let open Trace in
    log ~debug ~env trace;
    match trace.op with
    | Op.Dup n -> EStack.dup env.Env.stack (n - 1)
    | Op.Swap n -> EStack.swap env.Env.stack (n - 1)
    | _ -> ();
    let args_count = Op.input_count trace.op in
    let args = List.rev (List.init args_count ~f:(fun _ -> EStack.pop env.Env.stack)) in
    let result = Option.map ~f:(StackValue.create ~id:trace.Trace.index) trace.result in
    let full_trace = FullTrace.({ result; args; trace; env; }) in
    List.iter ~f:(fun t -> t db full_trace) taggers;
    Option.iter result ~f:(EStack.push env.Env.stack);
    match trace.Trace.children with
    | [] -> ()
    | children ->
      let new_address = Option.value ~default:env.address (nested_call_address trace.op args) in
      execute_traces new_address children taggers
  and execute_traces address traces taggers =
    let env = Env.create ~block_number:t.block_number ~tx_hash:t.tx_hash address in
    List.iter traces ~f:(execute_trace ~env ~taggers:taggers)
  in
  List.iter ~f:(execute_traces (BigInt.of_hex t.contract_address) traces) t.taggers;
  db
