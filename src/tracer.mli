open Core

type t

val create: block_number:Int.t -> tx_hash:String.t ->
    taggers:TracerTypes.Tagger.t List.t List.t -> String.t -> t

val execute_traces: ?debug:bool -> ?timeout:float -> ?db:FactDb.t -> t -> Trace.t List.t -> (FactDb.t * bool)
