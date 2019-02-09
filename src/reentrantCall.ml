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
            { alice = BigInt.to_hex ~length:40 addr1;
              bob = BigInt.to_hex ~length:40 addr2; alice_amount = value;
              bob_amount = BigInt.zero; alice_calls = 1; bob_calls = 0; })
      else ((addr2, addr1),
            { alice = BigInt.to_hex ~length:40 addr2;
              bob = BigInt.to_hex ~length:40 addr1; alice_amount = BigInt.zero;
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


let wei_per_ether = BigInt.(pow (of_int 10) 18)
let to_ether wei = BigInt.(to_int (wei / wei_per_ether))
let ether_diff t = to_ether BigInt.(abs (t.bob_amount - t.alice_amount))
let to_wei ether = BigInt.((of_int ether) * wei_per_ether)

let to_json t =
  `Assoc [("alice", `String t.alice);
          ("bob", `String t.bob);
          ("alice_amount", `Int (to_ether t.alice_amount));
          ("bob_amount", `Int (to_ether t.bob_amount));
          ("alice_calls", `Int t.alice_calls);
          ("bob_calls", `Int t.bob_calls);
          ("ether_diff", `Int (ether_diff t));
  ]

let of_json json =
  let open Yojson.Safe.Util in
  { alice = json |> member "alice" |> to_string;
    bob = json |> member "bob" |> to_string;
    alice_amount = json |> member "alice_amount" |> to_int |> to_wei;
    bob_amount = json |> member "bob_amount" |> to_int |> to_wei;
    bob_calls = json |> member "alice_calls" |> to_int;
    alice_calls = json |> member "bob_calls" |> to_int;
  }

module ContractResult = struct
  type t = [%import: ReentrantCall.ContractResult.t]

  let analyze ?(min_value=0) contract =
    let (%) f g = Fn.compose f g in
    let open Yojson.Safe.Util in
    let address = contract |> member "address" |> to_string in
    let results = contract |> member "result" |> to_assoc in
    let concatted = List.map ~f:of_json (List.bind ~f:(to_list % snd) results) in
    let process_call acc call =
      let make_update address amount =
        let current_value = Option.value ~default:0 (Map.find acc address) in
        Map.set acc ~key:address ~data:(current_value + to_ether amount)
      in
      if call.alice = address
        then make_update call.bob BigInt.(call.bob_amount - call.alice_amount)
      else if call.bob = address
        then make_update call.alice BigInt.(call.alice_amount - call.bob_amount)
      else acc
    in
    let res = List.fold ~init:(Map.empty (module String)) ~f:process_call concatted in
    let reentrant_calls = res |> Map.to_alist |> List.filter ~f:(fun (_, v) -> abs v >= min_value) in
    { address = address; reentrant_calls; }

  let analyze_file ?(min_value=0) file =
    let contracts = List.map ~f:Yojson.Safe.from_string (In_channel.read_lines file) in
    List.map ~f:(analyze ~min_value) contracts
    |> List.filter ~f:(fun t -> not (List.is_empty t.reentrant_calls))

  let to_string t =
    let f (contract, value) = Printf.sprintf "\t%s: %5d" contract value in
    let reentrants = List.map ~f t.reentrant_calls in
    Printf.sprintf "%s\n%s\n" t.address (String.concat ~sep:"\n" reentrants)
end
