open Core

type t = Opcode.t List.t


let to_string ?(show_pc=false) t =
  let f (result, pc) opcode =
    let pc_string = if show_pc then Printf.sprintf "%d " pc else "" in
    (result ^ pc_string ^ Opcode.to_string opcode ^ "\n", pc + Opcode.size opcode)
  in
  List.fold ~init:("", 1) ~f t |> fst
