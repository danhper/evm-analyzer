open Core

type 'a node = {
  value: 'a;
  mutable next: 'a node Option.t;
}

type 'a t = {
  mutable head: 'a node Option.t;
  copy: ('a -> 'a);
}

module Funcs = struct
  let create ?copy () = { head = None; copy = Option.value ~default:Fn.id copy; }
  let pop t = match t.head with
  | None -> failwith "empty stack"
  | Some n -> t.head <- n.next; n.value

  let push t value =
    t.head <- Some { value; next = t.head }

  let iter t ~f =
    let rec iter_nodes node =
      Option.iter node ~f:(fun n -> f n.value; iter_nodes n.next)
    in
    iter_nodes t.head

  let fold t ~init ~f =
    let rec fold_left acc node =
      Option.fold ~init:acc ~f:(fun a n -> fold_left (f a n.value) n.next) node
    in
    fold_left init t.head

  let size t = fold ~init:0 ~f:(fun acc _elem -> acc + 1) t

  let nth_node t n =
    let message = Printf.sprintf "stack smaller than %d" n in
    let rec get opt_node i =
      let node = Option.value_exn ~message opt_node in
      if i = 0
        then node
        else get node.next (i - 1)
    in
    get t.head n

  let swap t n =
    let head = Option.value_exn ~message:"empty stack" t.head in
    let prev_node = nth_node t n in
    let node = Option.value_exn ~message:"stack too small" prev_node.next in
    let next_node = node.next in
    prev_node.next <- Some head;
    node.next <- head.next;
    head.next <- next_node;
    t.head <- Some node

  let dup t n =
    let node = nth_node t n in
    push t (t.copy node.value)

  let to_string ~f t =
    let elems = List.rev_map ~f (fold ~init:[] ~f:(Fn.flip List.cons) t) in
    "(" ^ String.concat ~sep:" " elems ^ ")"
end

include Funcs
