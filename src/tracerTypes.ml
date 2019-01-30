open Core

module Tags = struct
  type t = [%import: TracerTypes.Tags.t]
  let to_string t =
    let f = fun (a, b) -> a ^ "=" ^ (Yojson.Safe.to_string b) in
    List.to_string ~f (String.Table.to_alist t)
  let pp f t = Format.pp_print_string f (to_string t)
end

module StackValue = struct
  type t = {
    value: BigInt.t;
    tags: Tags.t;
  } [@@deriving show]

  let create value = { value; tags = String.Table.create () }

  let set_tag t ~key ~value = Hashtbl.set t.tags ~key ~data:value
  let get_tag t key = Hashtbl.find t.tags key
  let get_tag_exn t key = Hashtbl.find_exn t.tags key
  let has_tag t ?value key = match get_tag t key, value with
    | None, _ -> false
    | Some _, None -> true
    | Some v, Some o -> v = o
end

module FullTrace = struct
  type t = [%import: TracerTypes.FullTrace.t]
end

module Tagger = struct
  type t = [%import: TracerTypes.Tagger.t]
end

module Env = struct
  type t = [%import: TracerTypes.Env.t]

  let create () = { stack = EStack.create () }
end
