open Core

type t

val create: String.t -> t PgMonad.t

val get_vulnerable_contracts: t -> String.t -> String.t List.t PgMonad.t

val get_contract_transactions:
  ?include_indirect:Bool.t -> ?limit:Int.t -> ?offset:Int.t -> t -> String.t
  -> (String.t * String.t * String.t) List.t PgMonad.t

val disconnect: t -> unit PgMonad.t
