open Core
open TracerTypes

type t = {
  taggers: (FactDb.t -> FullTrace.t -> unit) List.t List.t;
}

let create taggers = { taggers; }

let log ~debug ~env trace =
  let open Trace in
  if debug then
    let stack_str = EStack.to_string ~f:StackValue.show env.Env.stack in
    let op_code_str = Op.to_string trace.op in
    Out_channel.printf "%d: %s %s\n" trace.pc op_code_str stack_str

let execute_traces ?debug:(debug=false) t traces =
  let db = FactDb.create () in
  let execute_trace ~env ~taggers trace =
    let open Trace in
    log ~debug ~env trace;
    match trace.op with
    | Op.Dup n -> EStack.dup env.Env.stack (n - 1)
    | Op.Swap n -> EStack.swap env.Env.stack (n - 1)
    | _ -> ();
    let args_count = Op.input_count trace.op in
    let args = List.rev (List.init args_count ~f:(fun _ -> EStack.pop env.Env.stack)) in
    let result = Option.map ~f:(StackValue.create ~id:trace.Trace.index) trace.result in
    let full_trace = FullTrace.({ result; args; trace; }) in
    List.iter ~f:(fun t -> t db full_trace) taggers;
    Option.iter result ~f:(EStack.push env.Env.stack)
  in
  let execute_traces taggers =
    let env = Env.create () in
    List.iter traces ~f:(execute_trace ~env ~taggers)
  in
  List.iter ~f:execute_traces t.taggers;
  db
