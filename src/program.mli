open Core

type t = Opcode.t List.t

val to_string: t -> String.t
