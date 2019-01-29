type 'a t


val empty: unit -> 'a t
val pop: 'a t -> 'a
val push: 'a t -> 'a -> unit
val iter: 'a t -> f:('a -> unit) -> unit
val fold: 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
val size: 'a t -> int
val swap: 'a t -> int -> unit
val dup: 'a t -> int -> unit
