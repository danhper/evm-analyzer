open Core

module CI = Datalog_caml_interface

let num_facts db = CI.Logic.DB.num_facts db
let num_clauses db = CI.Logic.DB.num_clauses db

module Rel4 = struct
  type ('a,'b,'c,'d) t = [%import: FactDb.Rel4.t]

  let create ?(k1=CI.Univ.new_key ()) ?(k2=CI.Univ.new_key ())
    ?(k3=CI.Univ.new_key ()) ?(k4=CI.Univ.new_key ()) name = (CI.of_string name, k1, k2, k3, k4)

  let get (name,k1,k2,k3,k4) t =
    let module T = CI.Logic.T in
    match t with
    | T.Apply (name',
      [| T.Apply(u1, [| |])
      ;  T.Apply(u2, [| |])
      ;  T.Apply(u3, [| |])
      ;  T.Apply(u4, [| |])
      |]) when CI.Univ.eq name name' ->
      begin match CI.Univ.unpack ~key:k1 u1, CI.Univ.unpack ~key:k2 u2,
                  CI.Univ.unpack ~key:k3 u3, CI.Univ.unpack ~key:k4 u4 with
      | Some x1, Some x2, Some x3, Some x4 -> Some (x1,x2,x3,x4)
      | _ -> None
      end
    | _ -> None

  let find db ((n,_,_,_,_) as rel) =
    let module T = CI.Logic.T in
    let query = T.mk_apply n [| T.mk_var 0; T.mk_var 1; T.mk_var 2; T.mk_var 3 |] in
    List.fold_left (CI.Logic.ask db query) ~init:[]
      ~f:(fun acc t -> match get rel t with
        | None -> acc
        | Some (x,y,z, z') -> (x,y,z, z') :: acc)

  let make (n,k1,k2,k3,k4) x1 x2 x3 x4 =
    let module T = CI.Logic.T in
    let a1 = T.mk_const (CI.Univ.pack ~key:k1 x1) in
    let a2 = T.mk_const (CI.Univ.pack ~key:k2 x2) in
    let a3 = T.mk_const (CI.Univ.pack ~key:k3 x3) in
    let a4 = T.mk_const (CI.Univ.pack ~key:k4 x4) in
    T.mk_apply n [| a1; a2; a3; a4 |]

  let add_list db rel l =
    List.iter l
      ~f:(fun (x1,x2,x3,x4) -> CI.Logic.DB.add_fact db (make rel x1 x2 x3 x4))
end

module Rel5 = struct
  type ('a,'b,'c,'d,'e) t = [%import: FactDb.Rel5.t]

  let create ?(k1=CI.Univ.new_key ()) ?(k2=CI.Univ.new_key ())
    ?(k3=CI.Univ.new_key ()) ?(k4=CI.Univ.new_key ())
    ?(k5=CI.Univ.new_key ())
    name = (CI.of_string name, k1, k2, k3, k4, k5)

  let get (name,k1,k2,k3,k4,k5) t =
    let module T = CI.Logic.T in
    match t with
    | T.Apply (name',
      [| T.Apply(u1, [| |])
      ;  T.Apply(u2, [| |])
      ;  T.Apply(u3, [| |])
      ;  T.Apply(u4, [| |])
      ;  T.Apply(u5, [| |])
      |]) when CI.Univ.eq name name' ->
      begin match CI.Univ.unpack ~key:k1 u1, CI.Univ.unpack ~key:k2 u2,
                  CI.Univ.unpack ~key:k3 u3, CI.Univ.unpack ~key:k4 u4,
                  CI.Univ.unpack ~key:k5 u5 with
      | Some x1, Some x2, Some x3, Some x4, Some x5 -> Some (x1,x2,x3,x4,x5)
      | _ -> None
      end
    | _ -> None

  let find db ((n,_,_,_,_,_) as rel) =
    let module T = CI.Logic.T in
    let query = T.mk_apply n [| T.mk_var 0; T.mk_var 1; T.mk_var 2; T.mk_var 3; T.mk_var 4 |] in
    List.fold_left (CI.Logic.ask db query) ~init:[]
      ~f:(fun acc t -> match get rel t with
        | None -> acc
        | Some (x,y,z,z',z'') -> (x,y,z, z',z'') :: acc)

  let make (n,k1,k2,k3,k4,k5) x1 x2 x3 x4 x5 =
    let module T = CI.Logic.T in
    let a1 = T.mk_const (CI.Univ.pack ~key:k1 x1) in
    let a2 = T.mk_const (CI.Univ.pack ~key:k2 x2) in
    let a3 = T.mk_const (CI.Univ.pack ~key:k3 x3) in
    let a4 = T.mk_const (CI.Univ.pack ~key:k4 x4) in
    let a5 = T.mk_const (CI.Univ.pack ~key:k5 x5) in
    T.mk_apply n [| a1; a2; a3; a4; a5; |]

  let add_list db rel l =
    List.iter l
      ~f:(fun (x1,x2,x3,x4,x5) -> CI.Logic.DB.add_fact db (make rel x1 x2 x3 x4 x5))
end


type t = CI.Logic.DB.t

module Types = struct
  let bigint_key = CI.Univ.new_key
                    ~eq:BigInt.equal
                    ~hash:BigInt.hash
                    ~print:BigInt.to_hex
                    ()
  let int = CI.Univ.int
  let string = CI.Univ.string
end

let add_successor db =
  CI.Rel2.from_fun db
    (CI.Rel2.create ~k1:CI.Univ.int ~k2:CI.Univ.int "successor")
    (fun a b -> a = b + 1)

let create () =
  let db = CI.Logic.DB.create () in
  CI.add_builtin db;
  add_successor db;
  let () = match CI.Parse.parse_string Generated.clauses with
  | `Ok clauses -> CI.Logic.DB.add_clauses db clauses
  | `Error _ -> failwith "could not parse clauses"
  in
  db

let ask = CI.Logic.ask

let get_rel1 ~k name = CI.Rel1.create ~k name
let get_rel2 ~k1 ~k2 name = CI.Rel2.create ~k1 ~k2 name
let get_rel3 ~k1 ~k2 ~k3 name = CI.Rel3.create ~k1 ~k2 ~k3 name
let get_rel4 ~k1 ~k2 ~k3 ~k4 name = Rel4.create ~k1 ~k2 ~k3 ~k4 name
let get_rel5 ~k1 ~k2 ~k3 ~k4 ~k5 name = Rel5.create ~k1 ~k2 ~k3 ~k4 ~k5 name

let add_rel1 t rel arg = CI.Rel1.add_list t rel [arg]
let add_rel2 t rel args = CI.Rel2.add_list t rel [args]
let add_rel3 t rel args = CI.Rel3.add_list t rel [args]
let add_rel4 t rel args = Rel4.add_list t rel [args]
let add_rel5 t rel args = Rel5.add_list t rel [args]

let add_int_rel1 t name arg = add_rel1 t (get_rel1 ~k:Types.int name) arg
let add_int_rel2 t name args = add_rel2 t (get_rel2 ~k1:Types.int ~k2:Types.int name) args
let add_int_rel3 t name args = add_rel3 t (get_rel3 ~k1:Types.int ~k2:Types.int ~k3:Types.int name) args
(* let add_int_rel4 t name args = add_rel4 t (get_rel4 ~k1:Types.int ~k2:Types.int ~k3:Types.int ~k4:Types.int name) args *)

let query1 = CI.Rel1.find
let query2 = CI.Rel2.find
let query3 = CI.Rel3.find
let query4 = Rel4.find
let query5 = Rel5.find



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


module Relations = struct
  let reentrant_call = get_rel5 ~k1:Types.int ~k2:Types.bigint_key
                            ~k3:Types.bigint_key ~k4:Types.bigint_key
                            ~k5:Types.bigint_key
                            "reentrant_call"

  let direct_call = get_rel4 ~k1:Types.int ~k2:Types.bigint_key ~k3:Types.bigint_key
                             ~k4:Types.bigint_key "direct_call"
  let call = get_rel4 ~k1:Types.int ~k2:Types.bigint_key ~k3:Types.bigint_key
                      ~k4:Types.bigint_key "direct_call"

  let tx_sstore = get_rel3 ~k1:Types.int ~k2:Types.string ~k3:Types.bigint_key "tx_sstore"
  let tx_sload = get_rel3 ~k1:Types.int ~k2:Types.string ~k3:Types.bigint_key "tx_sload"
  let tod = get_rel4 ~k1:Types.int ~k2:Types.string ~k3:Types.string ~k4:Types.bigint_key "tod"
end