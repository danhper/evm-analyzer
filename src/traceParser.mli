open Core

val parse_json: Yojson.Safe.t -> Trace.t List.t
val parse_file: String.t -> Trace.t List.t
val parse_string: String.t -> Trace.t List.t
