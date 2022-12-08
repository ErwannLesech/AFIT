(** Power function implementations for builtin integers *)

(*#mod_use "builtin.ml";;
#mod_use "basic_arithmetics";;*)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)

let pow x n =
  if n = 0
  then 1
  else if n < 0
  then 0
  else let rec pow_rec x n = match n with
         |1 -> x
         |n -> x * pow_rec x (n-1)
       in pow_rec x n;;
            

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  if n = 0
  then 1
  else if n < 0 || x = 0
  then 0
  else let rec power_rec x n = match n with
         |1 -> x
         |2 -> x*x
         |n when (modulo n 2 = 0) -> power_rec (x*x) (n/2)
         |_ -> x * (power_rec (x*x) ((n-1)/2))
       in power_rec x n;;


(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
  if n < 0 || x = 0
  then 0
  else let rec mod_power_rec i n x = match n with
         |0 -> i
         |n when modulo n 2 = 1 -> mod_power_rec (modulo (i*x) m) (quot n 2) (modulo (x * x) m)
         |n -> mod_power_rec i (quot n 2) (modulo (x * x) m) 
     in mod_power_rec 1 n x;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)

let prime_mod_power x n p = if x = 0
                            then 0
                            else mod_power (modulo x p) (modulo n (p-1)) p;;
