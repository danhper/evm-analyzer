open Core

type t = Opcode.t List.t


let to_string ?(split_instructions=false) t =
  let f = Opcode.to_string ~split_instructions in
  String.concat ~sep:"\n" (List.map ~f t) ^ "\n"
