(*
 * These are the axioms of CZF set theory.
 *)

include Czf_itt_true
include Czf_itt_false
include Czf_itt_and
include Czf_itt_or
include Czf_itt_implies
include Czf_itt_all
include Czf_itt_exists
include Czf_itt_sall
include Czf_itt_sexists
include Czf_itt_dall
include Czf_itt_dexists
include Czf_itt_rel

open Printf
open Debug

let _ =
   if !debug_load then
      eprintf "Loading CZF_itt_axioms%t" eflush

(*
 * Set induction.
 *
 * H >- all x. P(x)
 * by set_induction
 * H; x: set; w: (all y: x. P(y)) >- P(x)
 * H >- P(x) wf
 *)
interactive set_induction 'H 'x 'w :
   sequent ['ext] { 'H; x: set >- "type"{'P['x]} } -->
   sequent ['ext] { 'H >- fun_prop{z. 'P['z]} } -->
   sequent ['ext] { 'H; x: set; w: dall{'x; z. 'P['z]} >- 'P['x] } -->
   sequent ['ext] { 'H >- sall{z. 'P['z]} }

interactive set_induction2 'H 'J 'x 'y 'w :
   sequent ['ext] { 'H; x: set; 'J['x]; y: set >- "type"{'C['y]} } -->
   sequent ['ext] { 'H; x: set; 'J['x] >- fun_prop{y. 'C['y]} } -->
   sequent ['ext] { 'H; x: set; 'J['x]; y: set; z: dall{'y; w. 'C['w]} >- 'C['y] }-->
   sequent ['ext] { 'H; x: set; 'J['x] >- 'C['x] }

(*
 * Restricted separation.
 *)
interactive separation 'H (bind{v. 'P['v]}) 'a 'b 'w 'x :
   sequent [squash] { 'H >- isset{'a} } -->
   sequent [squash] { 'H; b: set >- "type"{'P['b]} } -->
   sequent ['ext] { 'H >- restricted{b. 'P['b]} } -->
   sequent ['ext] { 'H; b: set; w: sall{x. iff{member{'x; 'b}; .member{'x; 'a} & 'P['x]}} >- 'C } -->
   sequent ['ext] { 'H >- 'C }

(*
 * Strong collection.
 *)
interactive collection 'H 's1 (bind{x. bind{y. 'P['x; 'y]}}) 's2 'x 'y 'w :
   sequent [squash] { 'H >- isset{'s1} } -->
   sequent [squash] { 'H; x: set; y: set >- "type"{'P['x; 'y]} } -->
   sequent ['ext] { 'H >- dall{'s1; x. sexists{y. 'P['x; 'y]}} } -->
   sequent ['ext] { 'H; s2: set; w: rel{x, y. 'P['x; 'y]; 's1; 's2} >- 'C } -->
   sequent ['ext] { 'H >- 'C }

(*
 * Subset collection.
 *)
interactive subset_collection 'H 's1 's2 's3 (bind{x. bind{y. 'P['x; 'y]}}) 'c 'u 'x 'y 'w :
   sequent ['ext] { 'H >- isset{'s1} } -->
   sequent ['ext] { 'H >- isset{'s2} } -->
   sequent [squash] { 'H; u: set; x: set; y: set >- "type"{'P['u; 'x; 'y]} } -->
   sequent ['ext] { 'H; u: set; x: set >- fun_prop{y. 'P['u; 'x; 'y]} } -->
   sequent ['ext] { 'H; u: set; y: set >- fun_prop{x. 'P['u; 'x; 'y]} } -->
   sequent ['ext] { 'H; c: set; w: sall{u. (dall{'s1; x. dexists{'s2; y. 'P['u; 'x; 'y]}}) => (dexists{'c; s3. rel{x, y. 'P['u; 'x; 'y]; 's1; 's3}})} >- 'C } -->
   sequent ['ext] { 'H >- 'C }

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
