(** Encoding Strings *)

(*#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
#mod_use "power.ml";;*)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)

let rec binary_encode x bits = match ((quot x 2), bits) with
  |(_,0) -> ""
  |((-1),_) -> binary_encode (quot x 2) (bits - 1) ^ "0" 
  |(0,_) -> binary_encode (quot x 2) (bits - 1) ^ string_of_int x
  |(_,bits) -> binary_encode (quot x 2) (bits - 1) ^ string_of_int(modulo x 2);;

let rec binary_decode x length bits i = match bits with
  |0 -> 0
  |b -> (int_of_char(x.[length-1]) - 48) * power 2 i + binary_decode x (length-1) (bits-1) (i+1);;

let encode str bits =
  if bits < 1
  then invalid_arg "encoding_msg_encode: Invalid ranks"
  else let rec encode_rec str bits i = match i with
         |i when i == ((String.length str)) -> ""
         |i -> binary_encode ((int_of_char str.[i])) bits ^ encode_rec str bits (i+1)
       in let str2 = (encode_rec str bits 0) in binary_decode str2 (String.length str2) (String.length str2) 0;;
           
(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)

let rec binary_encode_bis x = match (quot x 2) with
  |0 -> string_of_int x 
  |_ -> binary_encode_bis (quot x 2) ^ string_of_int(modulo x 2);;
                                                 
let decode msg bits =
  if bits < 1
  then invalid_arg "encoding_msg_decode: Invalid ranks"
  else let msg_bin = binary_encode_bis msg
       in let rec decode_rec msg length bits = match length with
         |0 -> ""
         |l -> decode_rec msg (length - bits) bits ^ Char.escaped(Char.chr(binary_decode msg length bits 0))
       in decode_rec msg_bin (String.length msg_bin) bits ;;

 
