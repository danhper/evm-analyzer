open! Core


type t = [%import: ReentrantCall.t]

let bigint_tuple_comparator = Tuple2.comparator BigInt.comparator BigInt.comparator

let get_reentrants db =
  let calls = FactDb.query5 db FactDb.Relations.reentrant_call in
  let empty_set = Set.Using_comparator.empty ~comparator:bigint_tuple_comparator in
  let f acc (_idx, addr1, addr2, _v1, _v2) = Set.add acc (addr1, addr2) in
  List.fold ~init:empty_set ~f calls


let aggregate_calls db =
  let reentrants = get_reentrants db in
  let empty_map = Map.Using_comparator.empty ~comparator:bigint_tuple_comparator in
  let f acc (_index, addr1, addr2, value) =
    let key, record = if addr1 > addr2
      then ((addr1, addr2),
            { alice = BigInt.to_hex addr1; bob = BigInt.to_hex addr2; alice_amount = value;
             bob_amount = BigInt.zero; alice_calls = 1; bob_calls = 0; })
      else ((addr2, addr1),
            { alice = BigInt.to_hex addr2; bob = BigInt.to_hex addr1; alice_amount = BigInt.zero;
             bob_amount = value; alice_calls = 0; bob_calls = 1; })
    in
    let make_update current = match current with
    | None -> record
    | Some ({ alice_amount; bob_amount; alice_calls; bob_calls; _;  } as v) ->
      { v with alice_amount = BigInt.add alice_amount record.alice_amount;
               alice_calls = alice_calls + record.alice_calls;
               bob_amount = BigInt.add bob_amount record.bob_amount;
               bob_calls = bob_calls + record.bob_calls; }

    in
    if Set.mem reentrants key
      then Map.update acc key ~f:make_update
      else acc
  in
  FactDb.query4 db FactDb.Relations.call
  |> List.fold ~init:empty_map ~f
  |> Map.data


let to_ether wei = BigInt.(to_int (wei / (pow (of_int 10) 18)))
let ether_diff t = to_ether BigInt.(abs (t.bob_amount - t.alice_amount))

let to_json t =
  `Assoc [("alice", `String t.alice);
          ("bob", `String t.bob);
          ("alice_amount", `Int (to_ether t.alice_amount));
          ("bob_amount", `Int (to_ether t.bob_amount));
          ("alice_calls", `Int t.alice_calls);
          ("bob_calls", `Int t.bob_calls);
          ("ether_diff", `Int (ether_diff t));
  ]
