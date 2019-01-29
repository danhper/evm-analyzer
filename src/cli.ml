open Core

let opcodes_command =
  let open Command.Let_syntax in
  Command.basic
  ~summary:"Retrieve opcodes from EVM bytecode"
  [%map_open
    let input_file = anon ("input-file" %: string)
    and show_pc = flag "show-pc" no_arg ~doc:"show program counter"
    and show_sourcemap = flag "show-sourcemap" no_arg ~doc:"show sourcemap"
    and output = flag "output" (optional string) ~doc:"ouptut file" in
    fun () ->
      Commands.opcodes_command ?output ~show_pc ~show_sourcemap input_file
  ]

let analyze_traces_command =
  let open Command.Let_syntax in
  Command.basic
  ~summary:"Analyze transaction traces"
  [%map_open
    let input_file = anon ("input-file" %: string)
    and debug = flag "debug" no_arg ~doc:"debug mode" in
    fun () ->
      Commands.analyze_traces ~debug input_file
  ]


let evm_analyzer_command =
  Command.group ~summary:"Analysis tool for EVM" [
    ("opcodes", opcodes_command);
    ("analyze-traces", analyze_traces_command);
  ]


let run () =
  Command.run ~build_info:"" ~version:"0.1" evm_analyzer_command
