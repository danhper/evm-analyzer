open Core

val opcodes_command: ?output:String.t -> show_pc:bool -> show_sourcemap:bool -> String.t -> unit

val analyze_traces: debug:bool -> contract_address:String.t Option.t -> String.t -> String.t -> unit

val analyze_vulnerabilities: output:String.t -> addresses:String.t List.t -> String.t -> unit

val analyze_reentrancy_results: ?min_value:Float.t -> String.t -> unit

val analyze_unhandled_exception_results: ?min_balance:Float.t -> ?min_value:Float.t -> String.t -> unit
