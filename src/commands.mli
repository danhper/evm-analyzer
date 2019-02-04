open Core

val opcodes_command: ?output:String.t -> show_pc:bool -> show_sourcemap:bool -> String.t -> unit

val analyze_traces: debug:bool -> String.t -> String.t -> unit

val analyze_vulnerabilities: output:String.t -> addresses:String.t List.t -> String.t -> unit
