(*
 * Structural rules.
 *
 *)

include Itt_equal
include Itt_rfun

(*
 * H; x: A; J >- A ext x
 * by hypothesis
 *)
axiom hypothesis 'H 'J 'x :
   sequent ['ext] { 'H; x: 'A; 'J['x] >- 'A }

(*
 * H, x: A, J >- A ext t
 * by thin
 * H, J >- A ext t
 *)
axiom thin 'H 'J :
   sequent ['ext] { 'H; 'J >- 'C } -->
   sequent ['ext] { 'H; x: 'A; 'J >- 'C }

(*
 * H, J >- T ext t[s]
 * by cut S
 * H, J >- S ext s
 * H, x: S, J >- T ext t[x]
 *)
axiom cut 'H 'J 'S 'x :
   sequent ['ext] { 'H; 'J >- 'S } -->
   sequent ['ext] { 'H; x: 'S; 'J >- 'T } -->
   sequent ['ext] { 'H; 'J >- 'T }

(*
 * H >- T
 * by introduction t
 * H >- t = t in T
 *)
axiom introduction 'H 't :
   sequent [squash] { 'H >- 't = 't in 'T } -->
   sequent ['ext] { 'H >- 'T }

(*
 * H >- T1[t1] ext t
 * by substitution (t1 = t2 in T2) lambda(x. T1[x])
 * H >- t1 = t2 in T2
 * H >- T1[t2]
 * H, x: T2 >- T1[x] = T1[x] in type
 *)
axiom substitution 'H ('t1 = 't2 in 'T2) lambda{x. 'T1['x]} :
   sequent [squash] { 'H >- 't1 = 't2 in 'T2 } -->
   sequent ['ext] { 'H >- 'T1['t2] } -->
   sequent [squash] { 'H; x: 'T2 >- "type"{'T1['x]} } -->
   sequent ['ext] { 'H >- 'T1['t1] }

(*
 * H, x: A, J >- T
 * by hypothesesReplacement B
 * H, x:B, J >- T
 * H, x: A, J >- A = B in type
 *)
axiom hypReplacement 'H 'J 'B univ[@i:l] :
   sequent ['ext] { 'H; x: 'B; 'J['x] >- 'T['x] } -->
   sequent [squash] { 'H; x: 'A; 'J['x] >- 'A = 'B in univ[@i:l] } -->
   sequent ['ext] { 'H; x: 'A; 'J['x] >- 'T['x] }

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:52:28  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.5  1996/09/02 19:37:42  jyh
 * Semi working package management.
 * All _univ version removed.
 *
 * Revision 1.4  1996/05/21 02:17:17  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.3  1996/04/11 13:34:19  jyh
 * This is the final version with the old syntax for terms.
 *
 * Revision 1.2  1996/03/30 01:37:20  jyh
 * Initial version of ITT.
 *
 * Revision 1.1  1996/03/28 02:51:33  jyh
 * This is an initial version of the type theory.
 *
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
