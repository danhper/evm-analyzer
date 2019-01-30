module CI = Datalog_caml_interface

type t = CI.Logic.DB.t

let create () =
  let db = CI.Logic.DB.create () in
  let () = match CI.Parse.parse_string Generated.clauses with
  | `Ok clauses -> CI.Logic.DB.add_clauses db clauses
  | `Error _ -> failwith "could not parse clauses"
  in
  db

let make_rel1 name = CI.Rel1.create ~k:CI.Univ.int name
let make_rel2 name = CI.Rel2.create ~k1:CI.Univ.int ~k2:CI.Univ.int name

let add_rel1 t name arg = CI.Rel1.add_list t (make_rel1 name) [arg]
let add_rel2 t name args = CI.Rel2.add_list t (make_rel2 name) [args]

let query1 t name = CI.Rel1.find t (make_rel1 name)
let query2 t name = CI.Rel2.find t (make_rel2 name)
