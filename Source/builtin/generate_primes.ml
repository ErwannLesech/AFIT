(** Generating primes *)

(*#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;*)

open Builtin
open Basic_arithmetics

(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
let init_eratosthenes n =
  if n < 2 
  then invalid_arg "generate_primes_init_eratosthenes: n must be superior than 2"
  else let rec init_eratosthenes_rec n list = match n with
         |2 -> 2 :: list
         |n -> if modulo n 2 = 0
               then init_eratosthenes_rec (n-1) list
               else init_eratosthenes_rec (n-1) (n::list)
       in init_eratosthenes_rec n [];;

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
 *)

let eratosthenes n = if n < 2 
                     then invalid_arg "generate_primes_init_eratosthenes: n must be superior than 2"
                     else let rec eratosthenes_rec list  = match list with
                            |[] -> []
                            |e::l ->
                              begin
                                let rec dividers_verif d list = match list with
                                  |[] -> []
                                  |e::l -> if modulo e d = 0
                                           then dividers_verif d l
                                           else e::dividers_verif d l
                                in e::eratosthenes_rec (dividers_verif e list)
                              end
                          in eratosthenes_rec (init_eratosthenes n);;


(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let oc = open_out file in 
  let rec aux = function
         [] -> close_out oc
        |e::l -> Printf.fprintf oc "%i \n" e; aux l
     in aux li;; 

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = 
    let list = eratosthenes n
    in write_list list file;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
  
let read_list_primes file =
 let ic = open_in file in
  let try_read () =
    try Some (input_line ic) with End_of_file-> None in
  let rec loop() = match try_read () with
        Some i -> i::(loop())
       |None -> close_in ic; []
  in let rec cast_all_number e i n = match i with
       |(-1) -> 0
       |i -> int_of_string(Char.escaped e.[i]) * n + cast_all_number e (i-1) (n*10)
     in let rec cast_string_to_int list = match list with
          |[] -> []
          |e::l -> (cast_all_number e ((String.length e)-2) 1)::(cast_string_to_int l)
     in cast_string_to_int (loop());;


(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t;;

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t;;

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  if limit < 2
  then invalid_arg "generate_primes_double_primes: limit needs to be superior at 2"
  else let rec double_primes_rec list isprime = match list with
         |[] -> []
         |e::l ->
           begin
             let double = e*2+1
             in if (isprime double)
                then (e, double)::double_primes_rec l isprime
                else double_primes_rec l isprime
           end
       in double_primes_rec (eratosthenes limit) isprime;;

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  if limit < 2
  then invalid_arg "generate_primes_double_primes: limit needs to be superior at 2"
  else let rec twin_primes_rec list isprime = match list with
         |[] -> []
         |e::l ->
           begin
             let twin = e+2
             in if (isprime twin)
                then (e, twin)::twin_primes_rec l isprime
                else twin_primes_rec l isprime
           end
       in twin_primes_rec (eratosthenes limit) isprime;;
