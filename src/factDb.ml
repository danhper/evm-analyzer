open Core

module CI = Datalog_caml_interface

type t = {
  db: CI.Logic.DB.t;
  int_cache: (string, int option) Hashtbl.t;
}

let num_facts t = CI.Logic.DB.num_facts t.db
let num_clauses t = CI.Logic.DB.num_clauses t.db

module Rel4 = struct
  type ('a,'b,'c,'d) t = CI.const * 'a CI.Univ.key * 'b CI.Univ.key * 'c CI.Univ.key * 'd CI.Univ.key

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
  type ('a,'b,'c,'d,'e) t = CI.const * 'a CI.Univ.key * 'b CI.Univ.key * 'c CI.Univ.key * 'd CI.Univ.key * 'e CI.Univ.key

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



module Types = struct
  let bigint_key = CI.Univ.new_key
                    ~eq:BigInt.equal
                    ~hash:BigInt.hash
                    ~print:BigInt.to_hex
                    ()
  let int = CI.Univ.int
  let string = CI.Univ.string
  let bool = CI.Univ.bool
end

let add_successor db =
  CI.Rel2.from_fun db
    (CI.Rel2.create ~k1:CI.Univ.int ~k2:CI.Univ.int "successor")
    (fun a b -> a = b + 1)

let add_lt db =
  CI.Rel2.from_fun db
    (CI.Rel2.create ~k1:CI.Univ.int ~k2:CI.Univ.int "lt")
    (fun a b -> a < b)

let add_lt_bi db =
  CI.Rel2.from_fun db
    (CI.Rel2.create ~k1:Types.bigint_key ~k2:Types.bigint_key "lt_bi")
    (fun a b -> a < b)

let add_lte db =
  CI.Rel2.from_fun db
    (CI.Rel2.create ~k1:CI.Univ.int ~k2:CI.Univ.int "lte")
    (fun a b -> a <= b)

let add_lte_bi db =
  CI.Rel2.from_fun db
    (CI.Rel2.create ~k1:Types.bigint_key ~k2:Types.bigint_key "lte_bi")
    (fun a b -> a <= b)

let add_gt db =
  CI.Rel2.from_fun db
    (CI.Rel2.create ~k1:CI.Univ.int ~k2:CI.Univ.int "gt")
    (fun a b -> a > b)

let add_gt_bi db =
  CI.Rel2.from_fun db
    (CI.Rel2.create ~k1:Types.bigint_key ~k2:Types.bigint_key "gte_bi")
    (fun a b -> a > b)

let add_gte db =
  CI.Rel2.from_fun db
    (CI.Rel2.create ~k1:CI.Univ.int ~k2:CI.Univ.int "gte")
    (fun a b -> a >= b)

let add_gte_bi db =
  CI.Rel2.from_fun db
    (CI.Rel2.create ~k1:Types.bigint_key ~k2:Types.bigint_key "gte_bi")
    (fun a b -> a >= b)

let is_zero_bi db =
  CI.Rel1.from_fun db
    (CI.Rel1.create ~k:Types.bigint_key "is_zero_bi")
    (fun a -> a = BigInt.zero)

let add_comparisons db =
  List.iter ~f:(fun f -> f db) [
    add_lt; add_lte; add_gt; add_gte;
    add_lt_bi; add_lte_bi; add_gt_bi; add_gte_bi; is_zero_bi;]

let create () =
  let db = CI.Logic.DB.create () in
  CI.add_builtin db;
  add_successor db;
  add_comparisons db;
  let () = match CI.Parse.parse_string Generated.clauses with
  | `Ok clauses -> CI.Logic.DB.add_clauses db clauses
  | `Error _ -> failwith "could not parse clauses"
  in
  { db; int_cache = Hashtbl.create (module String) }

let ask ?oc ?with_rules ?with_facts t term = CI.Logic.ask ?oc ?with_rules ?with_facts t.db term

let get_rel1 ~k name = CI.Rel1.create ~k name
let get_rel2 ~k1 ~k2 name = CI.Rel2.create ~k1 ~k2 name
let get_rel3 ~k1 ~k2 ~k3 name = CI.Rel3.create ~k1 ~k2 ~k3 name
let get_rel4 ~k1 ~k2 ~k3 ~k4 name = Rel4.create ~k1 ~k2 ~k3 ~k4 name
let get_rel5 ~k1 ~k2 ~k3 ~k4 ~k5 name = Rel5.create ~k1 ~k2 ~k3 ~k4 ~k5 name

let add_rel1 t rel arg = CI.Rel1.add_list t.db rel [arg]
let add_rel2 t rel args = CI.Rel2.add_list t.db rel [args]
let add_rel3 t rel args = CI.Rel3.add_list t.db rel [args]
let add_rel4 t rel args = Rel4.add_list t.db rel [args]
let add_rel5 t rel args = Rel5.add_list t.db rel [args]

let add_int_rel1 t name arg = add_rel1 t (get_rel1 ~k:Types.int name) arg
let add_int_rel2 t name args = add_rel2 t (get_rel2 ~k1:Types.int ~k2:Types.int name) args
let add_int_rel3 t name args = add_rel3 t (get_rel3 ~k1:Types.int ~k2:Types.int ~k3:Types.int name) args
(* let add_int_rel4 t name args = add_rel4 t (get_rel4 ~k1:Types.int ~k2:Types.int ~k3:Types.int ~k4:Types.int name) args *)

let add_bigint_rel1 t name arg = add_rel1 t (get_rel1 ~k:Types.bigint_key name) arg

let query1 t = CI.Rel1.find t.db
let query2 t = CI.Rel2.find t.db
let query3 t = CI.Rel3.find t.db
let query4 t = Rel4.find t.db
let query5 t = Rel5.find t.db


let get_int' t index clause_string =
  let clause = CI.Parse.term_of_string clause_string in
  match ask t clause with
  | [CI.Logic.T.Apply (_, values)] when Array.length values > index ->
    begin match values.(index) with
    | CI.Logic.T.Apply (value, _) -> CI.Univ.unpack ~key:CI.Univ.int value
    | _ -> None
    end
  | _ -> None

let get_int t index clause_string =
  match Hashtbl.find t.int_cache clause_string with
  | Some result -> result
  | None ->
    let result = get_int' t index clause_string in
    Hashtbl.set t.int_cache ~key:clause_string ~data:result;
    result

let get_bool t clause_string =
  get_int t 0 clause_string |> Option.is_some


module Relations = struct
  let reentrant_call = get_rel5 ~k1:Types.int ~k2:Types.bigint_key
                            ~k3:Types.bigint_key ~k4:Types.bigint_key
                            ~k5:Types.bigint_key
                            "reentrant_call"

  let direct_call = get_rel5 ~k1:Types.int ~k2:Types.int ~k3:Types.bigint_key ~k4:Types.bigint_key
                             ~k5:Types.bigint_key "direct_call"
  let call = get_rel4 ~k1:Types.int ~k2:Types.bigint_key ~k3:Types.bigint_key
                      ~k4:Types.bigint_key "call"
  let delegate_call = get_rel2 ~k1:Types.int ~k2:Types.bigint_key "delegate_call"

  let failed_call = get_rel3 "failed_call" ~k1:Types.int ~k2:Types.bigint_key ~k3:Types.bigint_key
  let overflow = get_rel5 "overflow" ~k1:Types.int ~k2:Types.bool ~k3:Types.int ~k4:Types.bigint_key ~k5:Types.bigint_key
  let tx_sstore = get_rel5 ~k1:Types.int ~k2:Types.bigint_key  ~k3:Types.string ~k4:Types.int ~k5:Types.bigint_key "tx_sstore"
  let tx_sload = get_rel5 ~k1:Types.int ~k2:Types.bigint_key ~k3:Types.string ~k4:Types.int ~k5:Types.bigint_key "tx_sload"
  let tod = get_rel5 ~k1:Types.int ~k2:Types.bigint_key ~k3:Types.string ~k4:Types.string ~k5:Types.bigint_key "tod"

  let caller = get_rel2 ~k1:Types.int ~k2:Types.bigint_key "caller"
  let selfdestruct = get_rel2 ~k1:Types.int ~k2:Types.bigint_key "selfdestruct"

  let unsafe_selfdestruct = get_rel2 ~k1:Types.int ~k2:Types.bigint_key "unsafe_selfdestruct"
  let unsafe_sstore = get_rel2 ~k1:Types.int ~k2:Types.bigint_key "unsafe_sstore"
  let unsafe_call = get_rel3 ~k1:Types.int ~k2:Types.bigint_key ~k3:Types.bigint_key "unsafe_call"
  let unsafe_delegate_call = get_rel2 ~k1:Types.int ~k2:Types.bigint_key "unsafe_delegate_call"

  let mdepends_w = get_rel3 ~k1:Types.int ~k2:Types.bigint_key ~k3:Types.bigint_key "mdepends_w"
  let mdepends_r = get_rel3 ~k1:Types.int ~k2:Types.bigint_key ~k3:Types.bigint_key "mdepends_r"
  let data_load_m = get_rel2 ~k1:Types.bigint_key ~k2:Types.bigint_key "data_load_m"
end
