open Core


module Channel_helpers = struct
  module type S = sig
    type t
    val to_stream: t -> string Stream.t
  end

  module type M = sig
    type t
    val input_line: t -> string option
  end

  module Make(Mod: M): S with type t := Mod.t = struct
    let to_stream ch = Stream.from (fun _ -> Mod.input_line ch)
  end
end

module type S = sig
  type t
  val with_file: string -> f:(t -> 'a) -> 'a
  val iter_lines: t -> f:(string -> unit) -> unit
  val fold_lines: t -> init:'a -> f:('a -> string -> 'a) -> 'a
  val cat_command: string
  include Channel_helpers.S with type t := t
end

module Text_channel: S = struct
  module T = struct
    type t = In_channel.t
    let input_line ch = In_channel.input_line ch
  end
  include T
  include Channel_helpers.Make(T)
  let with_file filename ~f = In_channel.with_file filename ~f
  let iter_lines ch ~f = In_channel.iter_lines ch ~f
  let fold_lines ch ~init ~f = In_channel.fold_lines ch ~f ~init
  let cat_command = "cat"
end

module Gzip_channel: S = struct
  module T = struct
    type t = {
      stream: Gzip.in_channel;
      buf_size: int;
      buf: Bytes.t;
      mutable buf_available: int;
      mutable buf_pos: int;
    }

    let renew_buffer ch =
      let n = Gzip.input ch.stream ch.buf 0 ch.buf_size in
      ch.buf_available <- n;
      ch.buf_pos <- 0;
      n

    let input_char ch =
      if (ch.buf_available = 0) && (renew_buffer ch) = 0 then
        raise End_of_file
      else
        let char = Bytes.get ch.buf ch.buf_pos in
        ch.buf_pos <- ch.buf_pos + 1;
        ch.buf_available <- ch.buf_available - 1;
        char

    let rec read_until_eol ch buf = match input_char ch with
      | '\n' -> ()
      | c    ->
        Buffer.add_char buf c;
        read_until_eol ch buf

    let input_line ch =
      let buf = Buffer.create 1024 in
      let _ = try read_until_eol ch buf with End_of_file -> () in
      if (Buffer.length buf) = 0
      then None
      else Some (Buffer.contents buf)
  end

  include T

  let create ?buffer_size:(size=1024) filename = {
    stream = Gzip.open_in filename;
    buf_size = size;
    buf = Bytes.create size;
    buf_available = 0;
    buf_pos = 0;
  }

  let cat_command = "zcat"

  let with_file filename ~f =
    let finalizer = fun ch -> Gzip.close_in ch.stream in
    Exn.protectx (create filename) ~f ~finally:finalizer

  let iter_lines ch ~f = Stream.iter f (Stream.from (fun _ -> input_line ch))

  let fold_lines ch ~init ~f =
    let acc = ref init in
    let stream = Stream.from (fun _ -> input_line ch) in
    Stream.iter (fun line -> acc := f !acc line) stream;
    !acc

  include Channel_helpers.Make(T)
end

let module_for filename: (module S) =
  if String.is_suffix filename ~suffix:".gz"
  then (module Gzip_channel)
  else (module Text_channel)

let iter_file_lines filename ~f =
  let (module ChannelImpl: S) = module_for filename in
  ChannelImpl.with_file filename ~f:(ChannelImpl.iter_lines ~f)

let count_lines filename =
  let count = ref 0 in
  let (module ChannelImpl: S) = module_for filename in
  let count_lines = ChannelImpl.iter_lines ~f:(fun _ -> incr count) in
  ChannelImpl.with_file filename ~f:count_lines;
  !count

let count_lines_wc filename =
  let (module ChannelImpl: S) = module_for filename in
  let command = Printf.sprintf "%s %s | wc -l" ChannelImpl.cat_command filename in
  Scanf.sscanf (Util.exec command) "%d" ident
