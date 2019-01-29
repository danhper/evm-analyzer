open Core

module JumpType: sig
  type t = In | Out | Regular

  val of_string: String.t -> t
end

module Mapping: sig
  type t = {
    start: Int.t;
    length: Int.t;
    source_index: Int.t;
    jump: JumpType.t;
  }
end

type t = Mapping.t List.t

val of_string: String.t -> t
