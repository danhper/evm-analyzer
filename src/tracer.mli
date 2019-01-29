open Core


module Env: sig
  type t

  val create: unit -> t
end

type t

val create: TracerTypes.Tagger.t List.t -> t

val execute_traces: ?debug:bool -> t -> Trace.t List.t -> unit
