include Monads.Std.Monad.Result.S
  with type 'a t = ('a, Caqti_error.t) result Lwt.t
    and type 'a e = ('a, Caqti_error.t) result Lwt.t
    and type 'a m = 'a Lwt.t
    and type err := Caqti_error.t

val full_run: unit t -> unit

module LwtMonad: sig
  include Monads.Std.Monad.S with type 'a t := 'a Lwt.t
end
