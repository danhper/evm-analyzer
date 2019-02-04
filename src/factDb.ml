open Core

module CI = Datalog_caml_interface

type t = CI.Logic.DB.t

let create () =
  let db = CI.Logic.DB.create () in
  let () = match CI.Parse.parse_string Generated.clauses with
  | `Ok clauses -> CI.Logic.DB.add_clauses db clauses
  | `Error _ -> failwith "could not parse clauses"
  in
  db

let ask = CI.Logic.ask

let make_rel1 name = CI.Rel1.create ~k:CI.Univ.int name
let make_rel2 name = CI.Rel2.create ~k1:CI.Univ.int ~k2:CI.Univ.int name
let make_rel3 name = CI.Rel3.create ~k1:CI.Univ.int ~k2:CI.Univ.int ~k3:CI.Univ.int name

let add_rel1 t name arg = CI.Rel1.add_list t (make_rel1 name) [arg]
let add_rel2 t name args = CI.Rel2.add_list t (make_rel2 name) [args]
let add_rel3 t name args = CI.Rel3.add_list t (make_rel3 name) [args]

let query1 t name = CI.Rel1.find t (make_rel1 name)
let query2 t name = CI.Rel2.find t (make_rel2 name)
let query3 t name = CI.Rel3.find t (make_rel3 name)

let get_int t index clause_string =
  let clause = CI.Parse.term_of_string clause_string in
  match ask t clause with
  | [CI.Logic.T.Apply (_, values)] when Array.length values > index ->
    begin match values.(index) with
    | CI.Logic.T.Apply (value, _) -> CI.Univ.unpack ~key:CI.Univ.int value
    | _ -> None
    end
  | _ -> None

let get_bool t clause_string =
  get_int t 0 clause_string |> Option.is_some
