open Core

let parse_json struct_logs =
  let open Yojson.Safe.Util in

  let make_trace op trace =
    let open Trace in
    { gas = trace |> member "gas" |> to_int;
      gas_cost = trace |> member "gasCost" |> to_int;
      op = Op.of_string op;
      depth = trace |> member "depth" |> to_int;
      pc = trace |> member "pc" |> to_int;
    }
  in

  let rec parse traces acc = match traces with
  | [] -> List.rev acc
  | trace :: rest ->
    let op = trace |> member "op" |> to_string in
    let full_op = if String.is_prefix ~prefix:"PUSH" op
      then
        (* NOTE: PUSH argument is the top value in the stack of the next trace *)
        let arg = rest |> List.hd_exn |> member "stack" |> to_list
                  |> List.last_exn |> to_string in
        op ^ " " ^ arg
      else op
    in
    parse rest (make_trace full_op trace :: acc)
  in

  parse (to_list struct_logs) []
