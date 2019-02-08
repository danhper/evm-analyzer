open Core
include Z



let pp f t = Format.pp_print_string f (Z.format "#0x%x" t)

let two = of_int 2

let of_hex raw_string =
  let string =
    if String.is_prefix ~prefix:"0x" raw_string
      then String.drop_prefix raw_string 2
      else raw_string
  in
  of_string_base 16 string

let to_hex = format "#0x%x"

let is_power ~power n =
  let rec is_power' n power =
    if n = one then true
    else if n mod (of_int power) = zero then is_power' (n / of_int power) power
    else false
  in
  if n = one || n = zero
    then n = of_int power
    else is_power' n power

let log ~base n =
  let rec log' n base acc =
    if n < base then acc
    else log' (n / base) base (acc + one)
  in
  to_int (log' n (of_int base) zero)

let int_size n =
  let lower bits = neg (pow two Int.(bits - 1)) in
  let upper bits = (pow two Int.(bits - 1)) - one in
  let in_range v bits = v >= lower bits && v <= upper bits in
  List.find ~f:(in_range n) [8; 16; 32; 64; 128; 256; 512]
  |> Option.value ~default:1024

let uint_size n =
  if n < zero then failwith "negative uint"
  else
    let in_range v bits = v <= (pow two bits) - one in
    List.find ~f:(in_range n) [8; 16; 32; 64; 128; 256; 512]
    |> Option.value ~default:1024

let twos_complement n bits =
  if n land (one lsl Int.(bits - 1)) = zero
    then n
    else n - (one lsl bits)

let limit_bits n bits = n land ((one lsl bits) - one)

module T = struct
  type t = Z.t
  let compare = Z.compare
  let sexp_of_t t = Sexp.of_string (to_hex t)
end


include Comparator.Make(T)
