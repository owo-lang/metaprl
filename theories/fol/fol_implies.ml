(*
 * Implication.
 *)

include Fol_type

open Base_dtactic

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

declare implies{'A; 'B}
declare lambda{x. 'b['x]}
declare apply{'f; 'a}

(************************************************************************
 * DISPLAY                                                              *
 ************************************************************************)

prec prec_implies
prec prec_lambda
prec prec_apply

prec prec_lambda < prec_apply
prec prec_lambda < prec_implies
prec prec_implies < prec_apply

dform implies_df : parens :: "prec"["prec_implies"] :: implies{'A; 'B} =
   szone pushm[0] slot["le"]{'A} hspace Rightarrow `" " slot{'B} popm ezone

dform lambda_df : parens :: "prec"["prec_lambda"] :: lambda{x. 'b} =
   szone pushm[3] Nuprl_font!lambda slot{'x} `"." slot{'b} popm ezone

dform apply_df : parens :: "prec"["prec_apply"] :: apply{'f; 'a} =
   slot{'f} hspace slot{'a}

(************************************************************************
 * COMPUTATION                                                          *
 ************************************************************************)

prim_rw beta : (lambda{x. 'b['x]} 'a) <--> 'b['a]

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

prim implies_type {| intro [] |} 'H :
   [wf] sequent ['ext] { 'H >- "type"{'A} } -->
   [wf] sequent ['ext] { 'H >- "type"{'B} } -->
   sequent ['ext] { 'H >- "type"{implies{'A; 'B}} } = trivial

prim implies_intro {| intro [] |} 'H 'x :
   [wf] sequent ['ext] { 'H >- "type"{'A} } -->
   [main] ('b['x] : sequent ['ext] { 'H; x: 'A >- 'B }) -->
   sequent ['ext] { 'H >- 'A => 'B } = lambda{x. 'b['x]}

prim implies_elim {| elim [] |} 'H 'J 'f 'b :
   [assertion] ('a : sequent ['ext] { 'H; f: 'A => 'B; 'J['f] >- 'A }) -->
   [main] ('t['f; 'b] : sequent ['ext] { 'H; f: 'A => 'B; 'J['f]; b: 'B >- 'C['f] }) -->
   sequent ['ext] { 'H; f: 'A => 'B; 'J['f] >- 'C['f] } = 't['f; 'f 'a]

(*
 * -*-
 * Local Variables:
 * Caml-master: "pousse"
 * End:
 * -*-
 *)
