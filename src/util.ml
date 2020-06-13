open Core

let exec command =
  let process_in = Unix.open_process_in command in
  let close () = ignore (Unix.close_process_in process_in) in
  Exn.protect ~finally:close ~f:(fun () -> In_channel.input_all process_in)

let cpu_count () =
  try match Sys.os_type with
  | "Win32" ->
    let nproc_env = Sys.getenv "NUMBER_OF_PROCESSORS" in
    Option.value_map ~default:1 ~f:int_of_string nproc_env
  | _ ->
    Scanf.sscanf (exec "getconf _NPROCESSORS_ONLN") "%d" ident
  with
  | Not_found_s _ | Sys_error _ | Failure _ | Scanf.Scan_failure _
  | End_of_file | Unix.Unix_error (_, _, _) -> 1

let mean numbers =
  let sum = List.sum (module Float) ~f:Fn.id numbers in
  sum /. (Int.to_float (List.length numbers))

let variance numbers =
  let num_mean = mean numbers in
  let squared_mean = mean (List.map ~f:(fun v -> v *. v) numbers) in
  squared_mean -. (num_mean *. num_mean)

let stdev numbers = sqrt (variance numbers)
