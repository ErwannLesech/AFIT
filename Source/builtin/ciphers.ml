(** Ciphers
    Builtin integer based ciphers.
*)

(*#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
#mod_use "power.ml";;*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)

   
let encrypt_cesar k m b =
  let rec encrypt_cesar_rec k m b = match m with
  |[] -> []
  |e::l ->
    begin
      let rec verif_base e k b =
        let code = e + k in
        if code > b
        then verif_base (e - b) k b
        else code::encrypt_cesar_rec k l b
      in verif_base e k b
    end
  in encrypt_cesar_rec k m b;;  

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)
let decrypt_cesar k m b =
  let rec decrypt_cesar_rec k m b = match m with
    |[] -> []
    |e::l ->
      begin
        let rec verif_base e k b =
          let code = e - k in
          if code < 0
          then verif_base (e + b) k b
          else code::decrypt_cesar_rec k l b
        in verif_base e k b
      end
  in decrypt_cesar_rec k m b;;  

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  let n = p*q and landa = (p-1)*(q-1) in
  let rec choose_e n landa e = if ((gcd e landa = 1) && (gcd e n = 1))
                               then e
                               else choose_e n landa (e-1)
  in let e = choose_e n landa (landa - 1)
    in let rec determine_d e landa d = 
         if modulo (d*e) landa = 1
         then d
         else determine_d e landa (d+e)
       in let d = determine_d e landa e in ((n,e),(n,d));;
                               

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n;;(*1 ligne*)

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n;;


(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p = (0, 0)

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = (0, 0)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = (0, 0)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = 0
