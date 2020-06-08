open Core


let parse_json struct_logs =
  let open Yojson.Safe.Util in

  let make_trace ~op ~result ~index ~children trace =
    let open Trace in
    { gas = trace |> member "gas" |> to_int;
      gas_cost = trace |> member "gasCost" |> to_int;
      index;
      op;
      depth = trace |> member "depth" |> to_int;
      pc = trace |> member "pc" |> to_int;
      children;
      result;
    }
  in

  let get_result traces =
    (* NOTE: result is the top value in the stack of the next trace *)
    let get_stack_top json = match member "stackTop" json with
    | `String s -> s
    | _ -> json |> member "stack" |> to_list |> List.last_exn |> to_string
    in
    traces |> List.hd |> Option.map ~f:get_stack_top
  in

  let get_op_string trace = trace |> member "op" |> to_string in
  let get_op trace rest =
    let op_string = get_op_string trace in
    let full_op = if String.is_prefix ~prefix:"PUSH" op_string
      then
        let arg = get_result rest in
        op_string ^ " " ^ (Option.value_exn arg)
      else op_string
    in
    Op.of_string_exn full_op
  in

  let index = ref ~-1 in
  let rec make_call trace rest =
    let depth = trace |> member "depth" |> to_int in
    let condition = fun t -> t |> member "depth" |> to_int = depth + 1 in
    parse ~condition rest []
  and parse ~condition traces acc = match traces with
  | [] -> (List.rev acc, [])
  | trace :: [] when String.is_prefix ~prefix:"PUSH" (get_op_string trace) ->
    (List.rev acc, [])
  | trace :: _ when not (condition trace) -> (List.rev acc, traces)
  | trace :: rest ->
    index := !index + 1;
    let op = get_op trace rest in
    let (children, rest) =
      if Op.is_call op
        then make_call trace rest
        else ([], rest)
    in
    let result = match op with
    | Push (_, res) -> Some res
    | op when Op.has_result op ->
      Option.map ~f:(BigInt.of_string_base 16) (get_result rest)
    | _ -> None
    in
    let new_trace = make_trace ~op ~result ~index:(!index) ~children trace in
    parse ~condition rest (new_trace :: acc)
  in

  parse ~condition:(Fn.const true) (to_list struct_logs) [] |> fst


let parse_string string =
  let json = Yojson.Safe.from_string string in
  let struct_logs = Yojson.Safe.Util.member "structLogs" json in
  parse_json struct_logs

let parse_file filepath =
  let json = Yojson.Safe.from_file filepath in
  let struct_logs = Yojson.Safe.Util.member "structLogs" json in
  parse_json struct_logs
