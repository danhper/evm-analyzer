include module type of Z

val pp: Format.formatter -> t -> unit

val is_power: power:int -> t -> bool

val log: base:int -> t -> int
