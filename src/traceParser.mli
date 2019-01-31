open Core

val parse_json: Yojson.Safe.json -> Trace.t List.t
val parse_file: String.t -> Trace.t List.t
