open Core

let opcodes_command ?output ~show_pc ~show_sourcemap input_file =
  let program =
    if input_file = "-"
      then Program.of_channel In_channel.stdin
      else Program.from_file input_file
  in
  let stringified = Program.format_ops ~show_pc ~show_sourcemap program in
  match output with
  | Some filepath -> Out_channel.write_all filepath ~data:stringified
  | None -> Out_channel.output_string Out_channel.stdout stringified


let analyze_traces ~debug filepath =
  let json = Yojson.Safe.from_file filepath in
  let struct_logs = Yojson.Safe.Util.member "structLogs" json in
  let traces = TraceParser.parse_json struct_logs in
  let tracer = Tracer.create [Taggers.tag_output; Taggers.tag_storage] in
  Tracer.execute_traces ~debug tracer traces
