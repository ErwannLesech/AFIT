(** Testing for primality *)

(*#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
#mod_use "power.ml";;*)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
   
let is_prime n =
  if n < 0
  then invalid_arg "test_primes_is_prime: n must be a natural number"
  else if n < 2
  then false
  else let rec is_prime_rec n d = match d with
         |d when (d*d) > n -> true
         |_ -> if modulo n d = 0
               then false
               else is_prime_rec n (d+1)
       in is_prime_rec n 2;;
               
               
(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)

let is_pseudo_prime p test_seq =
  if test_seq = []
  then invalid_arg "test_primes_is_pseudo_prime: List empty"
  else let rec is_pseudo_prime_rec p test_seq = match test_seq with
  |[] -> true
  |e::l -> if p = 2
           then true
           else if gcd e p > 1
           then false
           else if modulo (power p (e - 1)) e = 1 
           then true && is_pseudo_prime_rec p l
           else is_pseudo_prime_rec p l
  in is_pseudo_prime_rec p test_seq;;

