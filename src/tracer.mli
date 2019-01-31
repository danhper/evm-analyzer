open Core

type t

val create: TracerTypes.Tagger.t List.t -> t

val execute_traces: ?debug:bool -> t -> Trace.t List.t -> FactDb.t
