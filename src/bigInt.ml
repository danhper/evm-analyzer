include Z

let pp f t = Format.pp_print_string f (Z.format "#0x%x" t)

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
