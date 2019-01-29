open Core

val opcodes_command: ?output:String.t -> show_pc:bool -> show_sourcemap:bool -> String.t -> unit

val analyze_traces: debug:bool -> String.t -> unit
