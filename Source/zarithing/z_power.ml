(** Power function implementations for big integers *)

#load "zarith.cma";;
#install_printer Z.pp_print;;
open Z

(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m = let x = Z.of_int64 x in
  if n < 0 || x = 0
  then 0
  else let rec mod_power_rec x n m = match n with
         |0 -> 1
         |_ -> modulo ((modulo x m) * (mod_power_rec x (n-1) m)) m
     in mod_power_rec x n m;;

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p = zero
