(*
 * We define an equality on sets.
 * The normal intensional equality ('s1 = 's2 in set) is
 * not sufficient, because it is not a small type (it is in U2).
 *
 * We define and extensional equality by induction
 * on the sets.
 *)

include Czf_itt_pre_set

open Itt_equal

open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermSubst
open Refiner.Refiner.RefineError
open Resource

open Sequent
open Tacticals
open Var

open Base_auto_tactic

open Itt_equal
open Itt_rfun
open Itt_struct

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

declare eq_inner{'s1; 's2}

(************************************************************************
 * REWRITES                                                             *
 ************************************************************************)

primrw reduce_eq_inner : eq_inner{collect{'T1; x1. 'f1['x1]}; collect{'T2; x2. 'f2['x2]}} <-->
   ((all y1 : 'T1. exst y2: 'T2. eq_inner{.'f1['y1]; .'f2['y2]})
    & (all y2 : 'T2. exst y1: 'T1. eq_inner{.'f1['y1]; .'f2['y2]}))

(************************************************************************
 * DISPLAY FORMS                                                        *
 ************************************************************************)

dform eq_inner_df : mode[prl] :: eq_inner{'s1; 's2} =
   `"eq_inner(" slot {'s1} `"; " slot{'s2} `")"

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

(*
 * Membership in a universe.
 *)
interactive eq_inner_equality1 'H :
   sequent [squash] { 'H >- is_pre_set{'s1} } -->
   sequent [squash] { 'H >- is_pre_set{'s2} } -->
   sequent ['ext] { 'H >- eq_inner{'s1; 's2} = eq_inner{'s1; 's2} in univ[1:l] }

(*
 * Membership in a universe.
 *)
interactive eq_inner_type 'H :
   sequent [squash] { 'H >- is_pre_set{'s1} } -->
   sequent [squash] { 'H >- is_pre_set{'s2} } -->
   sequent ['ext] { 'H >- "type"{eq_inner{'s1; 's2}} }

(*
 * More general equality in a universe.
 *)
interactive eq_inner_equality2 'H :
   sequent [squash] { 'H >- 's1 = 's3 in pre_set } -->
   sequent [squash] { 'H >- 's2 = 's4 in pre_set } -->
   sequent ['ext] { 'H >- eq_inner{'s1; 's2} = eq_inner{'s3; 's4} in univ[1:l] }

(*
 * Equivalence relation rules.
 *)
interactive eq_inner_ref 'H :
   sequent [squash] { 'H >- is_pre_set{'s1} } -->
   sequent ['ext] { 'H >- eq_inner{'s1; 's1} }

interactive eq_inner_sym 'H :
   sequent [squash] { 'H >- is_pre_set{'s1} } -->
   sequent [squash] { 'H >- is_pre_set{'s2} } -->
   sequent ['ext] { 'H >- eq_inner{'s2; 's1} } -->
   sequent ['ext] { 'H >- eq_inner{'s1; 's2} }

interactive eq_inner_trans 'H 's2 :
   sequent [squash] { 'H >- is_pre_set{'s1} } -->
   sequent [squash] { 'H >- is_pre_set{'s2} } -->
   sequent [squash] { 'H >- is_pre_set{'s3} } -->
   sequent ['ext] { 'H >- eq_inner{'s1; 's2} } -->
   sequent ['ext] { 'H >- eq_inner{'s2; 's3} } -->
   sequent ['ext] { 'H >- eq_inner{'s1; 's3} }

(************************************************************************
 * TACTICS                                                              *
 ************************************************************************)

(*
 * Equality of inner equality.
 *)
let eqcd_eq_innerT p =
   let goal = Sequent.concl p in
   let _, eq1, eq2 = dest_equal goal in
   let j = hyp_count p in
      if alpha_equal eq1 eq2 then
         eq_inner_equality1 j p
      else
         eq_inner_equality2 j p

let eq_inner_term = << eq_inner{'s1; 's2} >>

let eq_inner_equal_term = << eq_inner{'s1; 's2} = eq_inner{'s3; 's4} in univ[1:l] >>

let eqcd_resource = eqcd_resource.resource_improve eqcd_resource (eq_inner_term, eqcd_eq_innerT)

let d_resource = d_resource.resource_improve d_resource (eq_inner_equal_term, d_wrap_eqcd eqcd_eq_innerT)

(*
 * Typehood.
 *)
let d_eq_inner_typeT i p =
   if i = 0 then
      eq_inner_type (hyp_count p) p
   else
      raise (RefineError ("d_eq_inner_typeT", StringError "no elimination form"))

let eq_inner_type_term = << "type"{eq_inner{'s1; 's2}} >>

let d_resource = d_resource.resource_improve d_resource (eq_inner_type_term, d_eq_inner_typeT)

(*
 * Equality relations.
 *)
let eqInnerRefT p =
   eq_inner_ref (hyp_count p) p

let eqInnerSymT p =
   eq_inner_sym (hyp_count p) p

let eqInnerTransT t p =
   eq_inner_trans (hyp_count p) t p

(*
 * Always reasonable to try reflexivity.
 *)
let auto_resource =
   auto_resource.resource_improve auto_resource (**)
      { auto_name = "eqInnerRefT";
        auto_prec = trivial_prec;
        auto_tac = auto_wrap eqInnerRefT
      }

(*
 * $Log$
 * Revision 1.1  1998/07/17 15:39:06  jyh
 * CZF is complete, although we may wish to add pairing and inf.
 *
 * Revision 1.1  1998/07/14 15:43:16  jyh
 * Intermediate version with auto tactic.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
