open Core

let parse_json struct_logs =
  let open Yojson.Safe.Util in

  let make_trace ~op ~result ~children trace =
    let open Trace in
    { gas = trace |> member "gas" |> to_int;
      gas_cost = trace |> member "gasCost" |> to_int;
      op;
      depth = trace |> member "depth" |> to_int;
      pc = trace |> member "pc" |> to_int;
      children;
      result;
    }
  in

  let get_result traces =
    (* NOTE: result is the top value in the stack of the next trace *)
    traces |> List.hd_exn |> member "stack" |> to_list
    |> List.last_exn |> to_string
  in

  let get_op trace rest =
    let op_string = trace |> member "op" |> to_string in
    let full_op = if String.is_prefix ~prefix:"PUSH" op_string
      then
        let arg = get_result rest in
        op_string ^ " " ^ arg
      else op_string
    in
    Op.of_string full_op
  in

  let rec make_call trace rest =
    let depth = trace |> member "depth" |> to_int in
    let condition = fun t -> t |> member "depth" |> to_int = depth + 1 in
    parse ~condition rest []
  and parse ~condition traces acc = match traces with
  | [] -> (List.rev acc, [])
  | trace :: _ when not (condition trace) -> (List.rev acc, traces)
  | trace :: rest ->
    let op = get_op trace rest in
    let (children, rest) = if Op.has_children op
                             then make_call trace rest
                             else ([], rest) in
    let result = match op with
    | Push (_, res) -> Some res
    | op when Op.has_result op -> Some (BigInt.of_string_base 16 (get_result rest))
    | _ -> None
    in
    parse ~condition rest (make_trace ~op ~result ~children trace :: acc)
  in

  parse ~condition:(Fn.const true) (to_list struct_logs) [] |> fst
