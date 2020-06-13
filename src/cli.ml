open Core

let process_debug_flag debug =
  if debug then
    Logs.set_level (Some Logs.Debug)

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
    and query = anon ("query" %: string)
    and contract_address = flag "contract-address" (optional string) ~doc:"contract address (for reentrancy)"
    and debug = flag "debug" no_arg ~doc:"debug mode" in
    fun () ->
      process_debug_flag debug;
      Commands.analyze_traces ~debug ~contract_address input_file query
  ]

let run_full_analysis_command =
  let open Command.Let_syntax in
  Command.basic
  ~summary:"Run full analysis on all transactions in given files"
  [%map_open
    let input_file = anon ("input-files" %: string)
    and output_dir = flag "output" (required string) ~doc:"ouptut directory"
    and debug = flag "debug" no_arg ~doc:"debug mode" in
    fun () ->
      process_debug_flag debug;
      Commands.run_full_analysis ~output_dir input_file
  ]


let analyze_vulnerabilities_command =
  let open Command.Let_syntax in
  Command.basic
  ~summary:"Analyze vulnerabilities of different transactions"
  [%map_open
    let vulnerability = anon ("vulnerability" %: string)
    and addresses = flag "addresses" (listed string) ~doc:"addresses to analize"
    and output = flag "output" (required string) ~doc:"path output"
    and debug = flag "debug" no_arg ~doc:"debug mode"
    and timeout = flag "timeout" (optional float) ~doc:"analysis timeout" in
    fun () ->
      process_debug_flag debug;
      Commands.analyze_vulnerabilities ?timeout ~output ~addresses vulnerability
  ]

let analyze_reentrancy_results_command =
  let open Command.Let_syntax in
  Command.basic
  ~summary:"Analyze reentrancy results"
  [%map_open
    let file = anon ("file" %: string)
    and min_value = flag "min-value" (optional float) ~doc:"minimum exploit value" in
    fun () ->
      Commands.analyze_reentrancy_results ?min_value file
  ]

let analyze_unhandled_exception_command =
  let open Command.Let_syntax in
  Command.basic
  ~summary:"Analyze unhandled exception results"
  [%map_open
    let file = anon ("file" %: string)
    and min_value = flag "min-value" (optional float) ~doc:"minimum exploit value"
    and min_balance = flag "min-balance" (optional float) ~doc:"minimum account balance"
    and historical_balance = flag "historical-balance" no_arg ~doc:"use historical balance" in
    fun () ->
      Commands.analyze_unhandled_exception_results ~historical_balance ?min_value ?min_balance file
  ]


let analyze_results_command =
  Command.group ~summary:"Analyze results" [
    ("reentrancy", analyze_reentrancy_results_command);
    ("unhandled-exception", analyze_unhandled_exception_command);
  ]


let evm_analyzer_command =
  Command.group ~summary:"Analysis tool for EVM" [
    ("opcodes", opcodes_command);
    ("run-full-analysis", run_full_analysis_command);
    ("analyze-traces", analyze_traces_command);
    ("analyze-vulnerabilities", analyze_vulnerabilities_command);
    ("analyze-results", analyze_results_command);
  ]


let run () =
  Logger.initialize ();
  Command.run ~build_info:"" ~version:"0.1" evm_analyzer_command
