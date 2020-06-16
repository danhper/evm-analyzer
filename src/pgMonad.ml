open Core
open Monads.Std

type 'a t = ('a, Caqti_error.t) result Lwt.t
type 'a e = ('a, Caqti_error.t) result Lwt.t
type 'a m = 'a Lwt.t

module LwtMonad = Monad.Make(
  struct
    type 'a t = 'a Lwt.t
    let return = Lwt.return
    let bind = Lwt.bind
    let map =
      let _map x ~f = Lwt.map f x in
      `Custom _map
  end)

include Monad.Result.Make(struct type t = Caqti_error.t end)(
  struct
    type 'a t = 'a Lwt.t
    include LwtMonad
  end)

let format_error error = Caqti_error.show error

let full_run t =
  match Lwt_main.run t with
  | Ok v -> v
  | Error err ->
    failwithf "%s\n" (format_error err) ()
