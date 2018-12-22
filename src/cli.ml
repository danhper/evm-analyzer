open Core

let opcodes_command =
  let open Command.Let_syntax in
  Command.basic
  ~summary:"Retrieve opcodes from EVM bytecode"
  [%map_open
    let input_file = anon ("input-file" %: string)
    and output = flag "output" (optional string) ~doc:"ouptut file" in
    fun () ->
      Commands.opcodes_command ?output input_file
  ]

let evm_analyzer_command =
  Command.group ~summary:"Analysis tool for EVM" [
    ("opcodes", opcodes_command);
  ]


let run () =
  Command.run ~build_info:"" ~version:"0.1" evm_analyzer_command
