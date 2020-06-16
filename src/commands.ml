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


let analyze_traces ~debug ~contract_address filepath query =
  let json = Yojson.Safe.from_file filepath in
  let struct_logs = Yojson.Safe.Util.member "structLogs" json in
  let traces = TraceParser.parse_json struct_logs in
  let contract_address = Option.value ~default:"0x0" contract_address in
  let tracer = Tracer.create ~block_number:0 ~tx_hash:"0x" ~taggers:Taggers.all contract_address in
  let (db, _) = Tracer.execute_traces ~debug tracer traces in
  if String.contains query '('
    then
      let results = FactDb.ask db (FactDb.CI.Parse.term_of_string query) in
      List.iter ~f:(Fn.compose print_endline FactDb.CI.Logic.T.to_string) results
    else
      let results = VulnerabilityAnalyzer.get_results db query in
      Yojson.Safe.pretty_to_channel Out_channel.stdout results
      (* let trace_indexes = FactDb.query1 db (FactDb.get_rel1 ~k:FactDb.Types.int query) in
      let traces = List.map ~f:(Fn.flip Yojson.Safe.Util.index struct_logs) trace_indexes in
      List.iter ~f:(Fn.compose print_endline Yojson.Safe.to_string) traces *)


let analyze_vulnerabilities ?timeout ~output ~addresses vulnerability =
  let addresses = match addresses with
  | [] -> None
  | list -> Some list
  in
  let result = VulnerabilityAnalyzer.analyze_vulnerabilities ~output ?timeout ?addresses vulnerability in
  PgMonad.full_run result

let run_full_analysis input_files ~output_dir =
  VulnerabilityAnalyzer.analyze_transactions_file ~output_dir input_files

let print_json ~to_json value =
  value |> to_json |> Yojson.Safe.to_string |> print_endline

let analyze_reentrancy_results ?(min_value=0.) input =
  let module RE = ResultsAnalyzer.Reentrancy in
  let result = RE.analyze_file ~min_value input in
  List.iter ~f:(print_json ~to_json:RE.to_json) result

let analyze_unhandled_exception_results
    ?(historical_balance=false) ?(min_balance=0.) ?(min_value=0.) input =
  let module UE = ResultsAnalyzer.UnhandledException in
  let result = Lwt_main.run (UE.analyze_file ~historical_balance ~min_balance ~min_value input) in
  print_json ~to_json:UE.to_json result
