(*
 * Negation is defined in terms of implication.
 *)

include Fol_false
include Fol_implies

open Base_dtactic

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

declare "not"{'A}

(************************************************************************
 * DISPLAY                                                              *
 ************************************************************************)

prec prec_not

dform not_df : parens :: "prec"["prec_not"] :: "not"{'A} =
   tneg slot{'A}

(************************************************************************
 * COMPUTATION                                                          *
 ************************************************************************)

prim_rw unfold_not : "not"{'A} <--> implies{'A; ."false"}

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

interactive not_type {| intro [] |} 'H :
   [wf] sequent ['ext] { 'H >- "type"{'A} } -->
   sequent ['ext] { 'H >- "type"{."not"{'A}} }

interactive not_intro {| intro [] |} 'H 'x :
   [wf] sequent ['ext] { 'H >- "type"{'A} } -->
   [main] sequent ['ext] { 'H; x: 'A >- "false" } -->
   sequent ['ext] { 'H >- "not"{'A} }

interactive not_elim {| elim [] |} 'H 'J :
   [main] sequent ['ext] { 'H; x: "not"{'A}; 'J['x] >- 'A } -->
   sequent ['ext] { 'H; x: "not"{'A}; 'J['x] >- 'C['x] }

(*
 * -*-
 * Local Variables:
 * Caml-master: "pousse"
 * End:
 * -*-
 *)
