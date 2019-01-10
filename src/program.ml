open Core

type t = Opcode.t List.t


let to_string t =
  String.concat ~sep:"\n" (List.map ~f:Opcode.to_string t) ^ "\n"
