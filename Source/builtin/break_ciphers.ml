(** Factoring Builtin int primes *)

#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key = let rec break_rec n i = match n with
                  |n when n <= i -> (i, n)
                  |n when modulo n i = 0 -> (i, quot n i)
                  |_ -> break_rec n (i + 1)
                in let (n,_) = key in break_rec n 2;;
