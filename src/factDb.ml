open Core

module CI = Datalog_caml_interface

type t = CI.Logic.DB.t

module T = struct
  let bigint_key = CI.Univ.new_key
                    ~eq:BigInt.equal
                    ~hash:BigInt.hash
                    ~print:(BigInt.format "#0x%x")
                    ()
  let int = CI.Univ.int
  let string = CI.Univ.string
end

let create () =
  let db = CI.Logic.DB.create () in
  CI.add_builtin db;
  let () = match CI.Parse.parse_string Generated.clauses with
  | `Ok clauses -> CI.Logic.DB.add_clauses db clauses
  | `Error _ -> failwith "could not parse clauses"
  in
  db

let ask = CI.Logic.ask

let get_rel1 ~k name = CI.Rel1.create ~k name
let get_rel2 ~k1 ~k2 name = CI.Rel2.create ~k1 ~k2 name
let get_rel3 ~k1 ~k2 ~k3 name = CI.Rel3.create ~k1 ~k2 ~k3 name

let add_rel1 t rel arg = CI.Rel1.add_list t rel [arg]
let add_rel2 t rel args = CI.Rel2.add_list t rel [args]
let add_rel3 t rel args = CI.Rel3.add_list t rel [args]

let add_int_rel1 t name arg = add_rel1 t (get_rel1 ~k:T.int name) arg
let add_int_rel2 t name args = add_rel2 t (get_rel2 ~k1:T.int ~k2:T.int name) args
let add_int_rel3 t name args = add_rel3 t (get_rel3 ~k1:T.int ~k2:T.int ~k3:T.int name) args

let query1 = CI.Rel1.find
let query2 = CI.Rel2.find
let query3 = CI.Rel3.find


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
