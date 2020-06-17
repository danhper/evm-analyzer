open Core

val all: TracerTypes.Tagger.t List.t List.t
val for_vulnerability: String.t ->  TracerTypes.Tagger.t List.t List.t
val per_block: TracerTypes.Tagger.t List.t List.t
val per_tx: TracerTypes.Tagger.t List.t List.t
