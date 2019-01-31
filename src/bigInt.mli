include module type of Z

val pp: Format.formatter -> t -> unit

val is_power: power:int -> t -> bool

val log: base:int -> t -> int

val int_size: t -> int
val uint_size: t -> int

val twos_complement: t -> int -> t

val limit_bits: t -> int -> t
