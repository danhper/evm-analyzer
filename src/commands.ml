open Core

let opcodes_command ?output ~split_instructions input_file =
  let run in_channel =
    let content = In_channel.input_all in_channel in
    let program = OpcodeParser.parse_bytecode content in
    let stringified = Program.to_string ~split_instructions program in
    match output with
    | Some filepath -> Out_channel.write_all filepath ~data:stringified
    | None -> Out_channel.print_endline stringified
  in
  if input_file = "-"
    then run In_channel.stdin
    else In_channel.with_file input_file ~f:run
