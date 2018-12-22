open Core

let opcodes_command ?output input_file =
  let content = In_channel.read_all input_file in
  let result = OpcodeParser.parse_bytecode content in
  let stringified = String.concat ~sep:"\n" (List.map ~f:Opcode.to_string result) ^ "\n" in
  match output with
  | Some filepath -> Out_channel.write_all filepath ~data:stringified
  | None -> Out_channel.print_endline stringified
