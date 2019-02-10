open Core

module Util = struct
  let get_address json = Yojson.Safe.Util.(json |> member "address" |> to_string)
  let get_results json = Yojson.Safe.Util.(json |> member "result" |> to_assoc)
  let get_concat_results ~f json =
    let results = get_results json in
    let get_list_value (tx_hash, values) =
      let module U = Yojson.Safe.Util in
      let f value = `Assoc (("tx_hash", `String tx_hash) :: U.to_assoc value) in
      List.map ~f (U.to_list values)
    in
    List.map ~f (List.bind ~f:get_list_value results)
end

module Reentrancy = struct
  type t = {
    address: String.t;
    reentrant_calls: (String.t * Float.t) List.t;
  }

  let analyze ?(min_value=0.) contract =
    let open ReentrantCall in
    let address = Util.get_address contract in
    let concatted = Util.get_concat_results ~f:ReentrantCall.of_json contract in
    let process_call acc call =
      let make_update address amount =
        let current_value = Option.value ~default:0. (Map.find acc address) in
        Map.set acc ~key:address ~data:(current_value +. EthUtil.eth_of_wei amount)
      in
      if call.alice = address
        then make_update call.bob BigInt.(call.bob_amount - call.alice_amount)
      else if call.bob = address
        then make_update call.alice BigInt.(call.alice_amount - call.bob_amount)
      else acc
    in
    let res = List.fold ~init:(Map.empty (module String)) ~f:process_call concatted in
    let reentrant_calls = res |> Map.to_alist |> List.filter ~f:(fun (_, v) -> Float.abs v >= min_value) in
    { address = address; reentrant_calls; }

  let analyze_file ?(min_value=0.) file =
    let contracts = List.map ~f:Yojson.Safe.from_string (In_channel.read_lines file) in
    List.map ~f:(analyze ~min_value) contracts
    |> List.filter ~f:(fun t -> not (List.is_empty t.reentrant_calls))

  let to_json t =
    let call_to_json (addr, amount) = `Assoc [("address", `String addr);
                                              ("amount", `Float amount)] in
    let calls = List.map ~f:call_to_json t.reentrant_calls in
    `Assoc [("address", `String t.address);
            ("reentrant_calls", `List calls)]

  let to_string t =
    let f (contract, value) = Printf.sprintf "\t%s: %5f" contract value in
    let reentrants = List.map ~f t.reentrant_calls in
    Printf.sprintf "%s\n%s\n" t.address (String.concat ~sep:"\n" reentrants)
end


module UnhandledException = struct
  module FailedCall = struct
    type t = {
      to_: String.t;
      op: String.t;
      ether: Float.t Option.t;
      overflow: Bool.t;
      tx_hash: String.t;
    }

    let to_string t =
      let ether = if t.overflow
        then "overflow"
        else Option.value_map ~f:Float.to_string_hum ~default:"NA" t.ether in
      Printf.sprintf "%s(%s, %s)" t.op t.to_ ether

    let to_json t =
      let ether = Option.value_map ~default:`Null ~f:(fun v -> `Float v) t.ether in
      `Assoc [("tx_hash", `String t.tx_hash);
              ("to", `String t.to_);
              ("op", `String t.op);
              ("ether", ether);
              ("overflow", `Bool t.overflow);]

    let of_json json =
      let open Yojson.Safe.Util in
      let op = json |> member "op" |> to_string in
      let stack = json |> member "stack" |> to_list in
      let tx_hash = json |> member "tx_hash" |> to_string in
      let contract_length = 40 in
      match List.rev stack with
      | `String _gas :: `String raw_to :: `String value_str :: _ ->
          let value = BigInt.of_hex value_str in
          let to_ = String.sub ~pos:(String.length raw_to - contract_length) ~len:contract_length raw_to in
          let eth_value = EthUtil.eth_of_wei value in
          if eth_value > Float.of_int (Int.pow 10 6)
            then { to_; op; ether = None; overflow = true; tx_hash; }
            else { to_; op; ether = Some eth_value; overflow = false; tx_hash; }
      | _ -> { to_ = "NA"; op; ether = None; overflow = false; tx_hash; }
  end

  type t = {
    address: String.t;
    balance: Float.t;
    failed_calls: FailedCall.t List.t;
  }

  let to_json t =
    let failed_calls = List.map ~f:FailedCall.to_json t.failed_calls in
    `Assoc [("address", `String t.address);
            ("balance", `Float t.balance);
            ("failed_calls", `List failed_calls);]

  let is_call failed_call =
    failed_call.FailedCall.op |> Op.of_string |> Option.value_map ~default:false ~f:Op.is_call

  let get_call_block rpc calls =
    let open PgMonad.LwtMonad.Let_syntax in
    let module Tx = EthTypes.Transaction in
    let call = List.last_exn calls in
    let tx_hash = call.FailedCall.tx_hash in
    let%map tx = EthRpc.Eth.get_transaction rpc tx_hash in
    Option.value_map ~default:`Latest ~f:(fun v -> `Block v.Tx.block_number) tx


  let with_balance ~rpc address failed_calls historical =
    let open PgMonad.LwtMonad.Let_syntax in
    let%bind tag =
      if historical
        then get_call_block rpc failed_calls
        else Lwt.return `Latest
    in
    let%map balance = EthRpc.Eth.get_balance ~tag rpc address in
    { address; failed_calls; balance = EthUtil.eth_of_wei balance; }


  let analyze ?(historical_balance=false) ?(min_value=0.) ~rpc contract =
    let address = Util.get_address contract in
    let concatted = Util.get_concat_results ~f:FailedCall.of_json contract in
    let filter = is_call in
    let fulfills_value call =
      match call.FailedCall.ether with
      | None -> true
      | Some v when v > min_value -> true
      | _ -> false
      in
    let filter = if min_value > 0. then (fun v -> filter v && fulfills_value v) else filter in
    let failed_calls = List.filter ~f:filter concatted in
    if List.length failed_calls > 0
      then with_balance ~rpc address failed_calls historical_balance
      else Lwt.return { address; failed_calls; balance = 0.; }

  let analyze_file ?(historical_balance=false) ?(min_balance=0.) ?(min_value=0.) file =
    let open PgMonad.LwtMonad.Let_syntax in
    let rpc = EthRpc.new_client (Sys.getenv_exn "ETH_URL") in
    let contracts = List.map ~f:Yojson.Safe.from_string (In_channel.read_lines file) in
    let promises = List.map ~f:(analyze ~historical_balance ~min_value ~rpc) contracts in
    let%map results = PgMonad.LwtMonad.all promises in
    List.filter ~f:(fun t -> not (List.is_empty t.failed_calls) && t.balance > min_balance) results

  let to_string t =
    let failed_calls = List.map ~f:FailedCall.to_string t.failed_calls in
    let balance = Float.to_string_hum t.balance in
    let failed_calls_str = String.concat ~sep:"\n" failed_calls in
    Printf.sprintf "%s (%s ETH)\n%s\n" t.address balance failed_calls_str
end
