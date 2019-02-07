open Core

module LwtMonad = PgMonad.LwtMonad

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
  open Rpc

  let get_code rpc address tag =
    let call = { name = "eth_getCode"; params = [String address; String tag]; } in
    let%map result = rpc call in
    match result with
    | { success = true; contents = Rpc.String res; } -> res
    | v -> failwithf "unexpected response: %s" (Rpc.string_of_response v) ()
end
