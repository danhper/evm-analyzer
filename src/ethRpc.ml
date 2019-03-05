open Core

module LwtMonad = PgMonad.LwtMonad

type t = Rpc.call -> Rpc.response Lwt.t
type tag = [`Block of Int.t | `Latest | `Earliest | `Pending]


let new_client url =
  let open LwtMonad.Let_syntax in
  let module Client = Cohttp_lwt_unix.Client in
  let module C = Cohttp in
  let module H = Cohttp_lwt in
  let uri = Uri.of_string url in
  let do_request call =
    let body = call |> Jsonrpc.string_of_call |> H.Body.of_string in
    let headers = C.Header.of_list [("content-type", "application/json")] in
    let%bind (resp, body) = Client.post ~body ~headers uri in
    let%map string_body = H.Body.to_string body in
    match resp |> C.Response.status |> C.Code.code_of_status with
    | 200 -> Jsonrpc.response_of_string string_body
    | _ -> Rpc.failure (Rpc.String string_body)
  in
  do_request

module Eth = struct
  open LwtMonad.Let_syntax

  let make_call rpc name params = rpc { Rpc.name = name; Rpc.params =  params; }

  let get_rpc_tag tag =
    match tag with
    | `Latest -> Rpc.String "latest"
    | `Earliest -> Rpc.String "earliest"
    | `Pending -> Rpc.String "pending"
    | `Block block -> Rpc.String (Printf.sprintf "0x%x" block)

  let get_string_response_exn result = match result with
    | { Rpc.success = true; Rpc.contents = Rpc.String res; } -> res
    | v -> failwithf "unexpected response: %s" (Rpc.string_of_response v) ()

  let get_code ?(tag=`Latest) rpc address =
    let%map result = make_call rpc "eth_getCode" [String address; get_rpc_tag tag] in
    get_string_response_exn result

  let get_balance ?(tag=`Latest) rpc address =
    let%map result = make_call rpc "eth_getBalance" [String address; get_rpc_tag tag] in
    result |> get_string_response_exn |> BigInt.of_string

  let get_transaction rpc tx_hash =
    let%map result = make_call rpc "eth_getTransactionByHash" [String tx_hash] in
    if result.Rpc.success
      then
        let unmarshalled = Rpcmarshal.unmarshal EthTypes.Transaction.typ_of result.Rpc.contents in
        Result.iter_error ~f:(fun (`Msg str) -> Logs.err (fun m -> m "could not get %s: %s" tx_hash str)) unmarshalled;
        Result.ok unmarshalled
      else None
end
