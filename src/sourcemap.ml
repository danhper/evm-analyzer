open Core

module JumpType = struct
  type t = [%import: Sourcemap.JumpType.t]

  let of_string string = match string with
    | "-" -> Regular
    | "i" -> In
    | "o" -> Out
    | _   -> failwith (Printf.sprintf "could not parse JumpType %s" string)
end

module Mapping = struct
  type t = [%import: Sourcemap.Mapping.t]
end

type t = [%import: Sourcemap.t]

let of_list mappings =
  let f acc elem =
    let open Mapping in
    let mapping = String.split ~on:':' elem in
    let get_value ~f ~accessor idx =
      match List.nth mapping idx with
      | None | Some "" -> acc |> List.hd_exn |> accessor
      | Some v -> f v
    in
    { start = get_value ~f:Int.of_string ~accessor:(fun v -> v.start) 0;
      length = get_value ~f:Int.of_string ~accessor:(fun v -> v.length) 1;
      source_index = get_value ~f:Int.of_string ~accessor:(fun v -> v.source_index) 2;
      jump = get_value ~f:JumpType.of_string ~accessor:(fun v -> v.jump) 3;
    } :: acc
  in
  mappings |> List.fold ~init:[] ~f |> List.rev


let of_string string = string |> String.split ~on:';' |> of_list
