open Core

include module type of Z

val two: t

val pp: Format.formatter -> t -> unit

val of_hex: String.t -> t
val to_hex: ?length:Int.t -> t -> String.t

val typ_of: t Rpc.Types.typ

val is_power: power:int -> t -> bool

val log: base:int -> t -> int

val int_size: t -> int
val uint_size: t -> int

val twos_complement: t -> int -> t

val limit_bits: t -> int -> t

val sexp_of_t: t -> Sexp.t
val t_of_sexp: Sexp.t -> t

include Comparator.S with type t := t
