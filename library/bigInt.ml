open Printf
open Debug

let _ =
   if !debug_load then
      eprintf "Loading BigInt%t" eflush

 (*module type BigIntSig =
sig
(************************************************************************
 * Types                                                                *
 ************************************************************************)

type bigint

exception IntSize of string * int

val create : int -> bigint

val lband : bigint -> bigint -> bigint
val lbor : bigint -> bigint -> bigint
val lbsl : bigint -> int -> bigint
 (*val lbsr : bigint -> int -> bigint*)
val basr : bigint -> int -> bigint

val bequal : bigint -> bigint -> bool
val blt : bigint -> bigint -> bool
val bgt : bigint -> bigint -> bool
val blte : bigint -> bigint -> bool
val bgte : bigint -> bigint -> bool
 end

module BigInt:BigIntSig =
 struct*)

type bigint = int * int

exception IntSize of string * int

let create i =
  if (i > 0xFFFF) then raise (IntSize ("create", i))
  else (0, i)

let mk_bint i = 
  let a = abs i in let b = ((a asr 16) land 0xFFFF) and c = (a land 0xFFFF) in
  if i >= 0 then (b, c) else (-b, c)
let make_bigint (i, k) =
  if ((i > 0xFFFF) or (k > 0xFFFF)) then raise (IntSize ("make_bigint", i))
  else (i, k)
let dest_bint (a, b) =
  let c = abs a in
  if c > 0x3FFF then failwith "bigint too big"
  else let i = (c lsl 16) lor b in if a >= 0 then i else (-i)
let dest_bigint (a, b) = (a, b)

let lband (x, w) (y, z) = ((x land y),(w land z))
let lbor (x, w) (y, z) = ((x lor y), (w lor z))

let lbsl (x, w) n =
  let a = abs (w lsl n) and b = abs ((x lsl n) land 0xFFFF) in
  if a <= 0xFFFF then (b, a)
  else ((b lor (a lsr 16)), (a land 0xFFFF)) ;;
	 
let lbsl (x, w) n =
  let rec aux i y z=
    (if i = n then (y, z)
    else
      let a = z lsl  1 and b = (y lsl 1) in
      (if a <= 0xFFFF then aux (i + 1) (b land 0xFFFF) a
      else aux (i + 1) ((b lor (a lsr 16)) land 0xFFFF) (a land 0xFFFF))) in
  aux 0 x w ;;

(*
let lbsr (x, w) n =
let basr (x, w) n = (x, w)
*)
	   (*LAL*)
let bplus (x, w) n = (x, w+ n)
let bminus (x, w) n = (x, w - n)

let bequal (x, w) (y, z) = (x = y) && (w = z)
let blt (x, w) (y, z) = (x < y) or ((x = y) && (w < z))
let bgt (x, w) (y, z) = (x > y) or ((x = y) && (w > z))
let blte (x, w) (y, z) = (x < y) or ((x = y) && (w <= z))
let bgte (x, w) (y, z) = (x > y) or ((x = y) && (w >= z))

let bdecr b = let (x, y) = !b
in if y > 0 then b:=  (x, y-1) else b:= (x-1, 0xffff)
(* end ;;*)

let print_bigint b =
  let (a, c) = dest_bigint b in
  print_char '(';
  print_int a;
  print_char ',';
  print_int c; print_char ')'
	

 
