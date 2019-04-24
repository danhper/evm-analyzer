open Core

module JumpType = struct
  type t = In | Out | Regular

  let of_string string = match string with
    | "-" -> Regular
    | "i" -> In
    | "o" -> Out
    | _   -> failwith (Printf.sprintf "could not parse JumpType %s" string)
end

module Mapping = struct
  type t = {
    start: Int.t;
    length: Int.t;
    source_index: Int.t;
    jump: JumpType.t;
  }

  let of_string ?(current_mappings=[]) string =
    let mapping = String.split ~on:':' string in
    let get_value ?default ~f ~accessor idx =
      match List.nth mapping idx with
      | None | Some "" ->
        let result = current_mappings |> List.hd |> Option.map ~f:accessor in
        Option.value_exn (Option.first_some result default)
      | Some v -> f v
    in
    { start = get_value ~f:Int.of_string ~accessor:(fun v -> v.start) 0;
      length = get_value ~f:Int.of_string ~accessor:(fun v -> v.length) 1;
      source_index = get_value ~f:Int.of_string ~accessor:(fun v -> v.source_index) 2;
      jump = get_value ~default:JumpType.Regular ~f:JumpType.of_string ~accessor:(fun v -> v.jump) 3;
    }
end

type t = Mapping.t List.t

let of_list mappings =
  let f acc elem = (Mapping.of_string ~current_mappings:acc elem) :: acc in
  mappings |> List.fold ~init:[] ~f |> List.rev


let of_string string = string |> String.split ~on:';' |> of_list
