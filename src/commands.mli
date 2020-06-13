open Core

val opcodes_command: ?output:String.t -> show_pc:bool -> show_sourcemap:bool -> String.t -> unit

val analyze_traces: debug:bool -> contract_address:String.t Option.t -> String.t -> String.t -> unit

val run_full_analysis: String.t -> output_dir:String.t -> unit

val analyze_vulnerabilities: ?timeout:Float.t -> output:String.t -> addresses:String.t List.t -> String.t -> unit

val analyze_reentrancy_results: ?min_value:Float.t -> String.t -> unit

val analyze_unhandled_exception_results:
  ?historical_balance:Bool.t -> ?min_balance:Float.t
    -> ?min_value:Float.t -> String.t -> unit
