include Z

let pp f t = Format.pp_print_string f (Z.format "#0x%x" t)
