(** Basic arithmetics with builtin integers *)

(*#mod_use "builtin.ml";;*)

open Builtin;;

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
*)
let rec gcd a b = (*Using the Euclid's algorithm*)
  if b < 0
  then gcd a (-b)
  else if b = 0
  then a
  else gcd b (modulo a b);;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
 *)

let bezout a b = (*Using the extended Euclid's algorithm*)
  if a = 0 || b = 0
  then invalid_arg "basic_arithmetics_bezout: Invalid ranks"
  else let rec bezout u v r u2 v2 r2 = match r2 with
         |0 -> (u,v,r)
         |_ -> let q = quot r r2
               in bezout u2 v2 r2 (u - q * u2) (v - q * v2) (r - q * r2)
       in bezout 1 0 a 0 1 b;;
  
