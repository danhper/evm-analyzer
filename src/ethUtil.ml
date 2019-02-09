open Core

let kwei_per_ether = BigInt.(pow (of_int 10) 15)
let eth_of_wei wei = BigInt.(to_float (wei / kwei_per_ether)) /. 1000.
let wei_of_eth eth = BigInt.(of_float (eth *. 1000.) * kwei_per_ether)
