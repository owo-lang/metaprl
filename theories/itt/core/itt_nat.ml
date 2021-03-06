doc <:doc<
   @module[Itt_nat]

   Theory of natural numbers.

   @docoff
   ----------------------------------------------------------------

   @begin[license]
   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/htmlman/default.html or visit http://metaprl.org/
   for more information.

   Copyright (C) 2001-2006 MetaPRL Group, Cornell University, California
   Institute of Technology and City University of New York

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author:
      Alexei Kopylov @email{kopylov@cs.cornell.edu}
   Modified By:
      Aleksey Nogin @email{nogin@cs.caltech.edu}
      Jason Hickey @email{jyh@cs.caltech.edu}
   @end[license]
>>

doc <:doc<
   @parents
>>
extends Itt_equal
extends Itt_dfun
extends Itt_logic
extends Itt_bool
extends Itt_struct3
extends Itt_int_base
extends Itt_int_ext
extends Itt_int_arith
extends Itt_sqsimple
extends Itt_omega
doc docoff

open Basic_tactics

open Itt_struct
open Itt_equal
open Itt_bool
open Itt_subtype
open Itt_squiggle
open Itt_sqsimple
open Itt_int_arith

doc terms

define const unfold_nat : nat <--> ({x:int | 'x>=0})
define unfold_finite_nat : nat{'k} <--> int_seg{0; 'k}

(* unused
let fold_finite_nat = makeFoldC << nat{'k} >> unfold_finite_nat
 *)

define unfold_nat_plus : nat_plus <--> ({x:int | 'x>0})

define unfoldInd : ind{'n; 'base; k,l. 'up['k;'l]} <-->
                   ind{'n; i,j.it; 'base; k,l . 'up['k;'l]}

define iform unfoldInd1 : ind{'n; 'base; l. 'up['l]} <-->
                    ind{'n; 'base; k,l . 'up['l]}

doc docoff

let foldInd = makeFoldC << ind{'n; 'base; k,l. 'up['k;'l]} >> unfoldInd

let resource elim +=
   << 0 in nat >>, wrap_elim_auto_ok thinT

(******************
 *  Display Forms *
 ******************)

dform nat_prl_df : except_mode [src] :: nat = mathbbN
dform finite_nat_df1 : except_mode [src] :: nat{'k} = mathbbN sub{'k}
dform finite_nat_df2 : mode[src] :: nat{'k} = `"{0..(" slot{'k} `"-1)}"

dform nat_plus_df : except_mode[src] :: nat_plus = mathbbN sup{slot["*"]}

dform ind_df : parens :: "prec"[prec_bor] :: except_mode[src] ::
   ind{'x; 'base; k, l. 'up['k :> Dform; 'l :> Dform]} =
   szone pushm[3]
     szone display_ind{'x} space `"where" space display_ind_n space `"=" ezone
   hspace
     math_implies{display_ind_eq{display_n;0}; display_ind_eq{display_ind_n;'base}}
   hspace
     math_implies{math_gt{display_n; 0}; display_ind_eq{display_ind_n; 'up[display_n; display_ind{math_sub{display_n;1}}]}}
   popm ezone

doc rewrites

interactive_rw reduce_ind_up {| reduce |} :
   ('x in nat) -->
   ind{'x +@ 1; 'base; k,l. 'up['k;'l]} <-->
   ('up['x +@ 1; ind{'x ; 'base; k,l. 'up['k;'l]}])

interactive_rw reduce_ind_base {| reduce |} :
   (ind{0; 'base; k,l. 'up['k;'l]}) <-->
   'base

let reduce_ind_numberC =
   unfoldInd

let resource reduce += [
   <<ind{number[n:n]; 'base; k, l. 'up['k; 'l]}>>, wrap_reduce reduce_ind_numberC;
]

let ind_term = << ind{'x; 'base; k, l. 'up['k; 'l]} >>
let ind_opname = opname_of_term ind_term
let is_ind_term = is_dep0_dep0_dep2_term ind_opname
let dest_ind = dest_dep0_dep0_dep2_term ind_opname
let mk_ind_term = mk_dep0_dep0_dep2_term ind_opname

doc rules

interactive natType {| intro [] |} :
   sequent { <H> >- "type"{nat} }

interactive natUniv {| intro [] |} :
   sequent { <H> >- nat in univ[i:l] }

interactive natMemberEquality {| intro [AutoMustComplete] |} :
   sequent { <H> >- 'a='b in int} -->
   sequent { <H> >- 'a >= 0}  -->
   sequent { <H> >- 'a='b in nat}

interactive natMemberZero {| intro [] |} :
   sequent { <H> >- 0 in nat}

interactive nat_is_int {| nth_hyp |} :
   sequent { <H> >- 'a='b in nat} -->
   sequent { <H> >- 'a='b in int}

interactive nat_is_int_left {| nth_hyp |} 'b :
   sequent { <H> >- 'a='b in nat} -->
   sequent { <H> >- 'a in int }

interactive nat_is_int_right {| nth_hyp |} 'a :
   sequent { <H> >- 'a='b in nat} -->
   sequent { <H> >- 'b in int }

interactive eq_2beq_nat 'H :
   sequent { <H>; x: 'a = 'b in nat; <J['x]>; "assert"{beq_int{'a; 'b}} >- 'C['x] } -->
   sequent { <H>; x: 'a = 'b in nat; <J['x]> >- 'C['x] }

doc docoff

let resource nth_hyp += [
   << number[m:n] = number[n:n] in nat >>, <<'C>>,
   wrap_nth_hyp_uncertain (fun i ->
      eq_2beq_nat i
         thenT rw (addrC [Subterm 1] Itt_int_base.reduce_eq_int) (-1)
         thenT Itt_bool.assert_false (-1));
   <<'a='b in nat>>, <<'b = 'a in int>>, wrap_nth_hyp_certain (argfunT (fun i p ->
      (if is_member_term (concl p) then
         nat_is_int
      else
         (nat_is_int thenT equalitySym))
      thenT hypothesis i))
]

doc docon

interactive nat_is_int2 {| nth_hyp |} 'H :
   sequent { <H>; a: nat; <J['a]> >- 'a in int }

interactive nat_is_subtype_of_int  {| intro[] |} :
   sequent { <H> >- nat subtype int }

interactive nat_sqsimple {| intro []; sqsimple |} :
   sequent { <H> >- sqsimple{nat} }

let resource sub += (RLSubtype ([<< nat >>, << int>>], nat_is_subtype_of_int  ))

let err = RefineError("Itt_nat.elim_nat_eq", StringError "not applicable")

let elim_nat_eq fwd = argfunT (fun i p ->
   let s = explode_sequent_arg p in
   let hyps = s.sequent_hyps in
   let i = get_pos_hyp_num p i in
      match SeqHyp.get hyps (i - 1) with
         Hypothesis (v, t) ->
            let len = SeqHyp.length hyps in
            let _, t1, t2 = dest_equal t in
            let dep = is_var_free v s.sequent_concl || is_var_free_hyps hyps len v i in
               if alpha_equal t1 t2 then
                  if dep then equalityElimination i thenT thinT i else thinT i
               else
                  let t_from = if fwd then t1 else t2 in
                  let ind = snd (least_fw_index hyps (free_vars_set t_from) SymbolSet.empty (i - 1)) in
                  let bind_hyps = Lm_list_util.remove_nth (i - ind - 1) (Lm_list_util.nth_tl ind (SeqHyp.to_list hyps)) in
                  let v = maybe_new_var_set v (free_vars_terms [t; s.sequent_concl]) in
                  let bind_hyps = bind_hyps @ [Hypothesis(v, mk_var_term v)] in
                  let t = mk_sequent_term { s with sequent_hyps = SeqHyp.of_list bind_hyps } in
                  let bind = var_subst_to_bind t t_from in
                  let tac =
                     if alpha_equal (snd (dest_bind1 bind)) t then
                        idT
                     else begin
                        moveHypT i (ind + 1) thenT sqsimple (ind + 1) thenLT [
                           trivialT;
                           (if fwd then sq_subst_forward else sq_subst_backward) (ind + 2) (len - ind) bind
                           thenT moveHypT (ind + 1) i
                        ]
                     end
                  in
                     if dep then
                        equalityElimination i thenT tac
                     else if tac == idT then
                        raise err
                     else
                        tac
       | Context _ ->
            raise err)

let resource elim += [
   << 'a = number[n:n] in nat >>, wrap_elim_auto_ok (elim_nat_eq true);
   << 'a = number[n:n] in int >>, wrap_elim_auto_ok (elim_nat_eq true);
   << number[n:n] = 'a in nat >>, wrap_elim_auto_ok (elim_nat_eq false);
   << number[n:n] = 'a in int >>, wrap_elim_auto_ok (elim_nat_eq false);
]

interactive eq_nat_decidable {| intro [] |} :
   [wf] sequent{ <H> >- 'a in nat } -->
   [wf] sequent{ <H> >- 'b in nat } -->
   sequent{ <H> >- decidable{('a = 'b in nat)} }

interactive natElimination 'H :
   sequent { <H>; x: int; v:'x>=0; <J['x]> >- 'C['x]}  -->
   sequent { <H>; x: nat; <J['x]> >- 'C['x]}

interactive nat2ge {| ge_elim [] |} 'H :
   sequent { <H>; x: nat; <J['x]>; 'x>=0 >- 'C['x]}  -->
   sequent { <H>; x: nat; <J['x]> >- 'C['x]}

interactive nat2ge2 {| ge_elim [] |} 'H :
   sequent { <H>; x: 'a in nat; <J['x]>; 'a >= 0 >- 'C['x]}  -->
   sequent { <H>; x: 'a in nat; <J['x]> >- 'C['x]}

interactive nat2ge3 {| ge_elim [not_member] |} 'H :
   sequent { <H>; x: 'a = 'b in nat; <J['x]>; 'a >= 0; 'a >= 'b; 'b >= 'a >- 'C['x]}  -->
   sequent { <H>; x: 'a = 'b in nat; <J['x]> >- 'C['x]}

let resource nth_hyp += [
   <<nat>>, <<!x >= 0>>, wrap_nth_hyp_uncertain (fun i -> nat2ge i thenT hypothesis (-1));
   <<'a = 'b in nat >>, << 'a >= 0 >>, wrap_nth_hyp_certain (fun i -> nat2ge3 i thenT hypothesis (-3));
]

interactive ge2nat {| ge_intro |} :
   [wf] sequent { <H> >- 'n in int }  -->
   sequent { <H>; (-1) >= 'n >- "false" } -->
   sequent { <H> >- 'n in nat }

interactive ge_eq2nat {| ge_intro |} :
   [wf] sequent { <H> >- 'n in int }  -->
   [wf] sequent { <H> >- 'm in int }  -->
   sequent { <H>; 'n <> 'm >- "false" }  -->
   sequent { <H>; (-1) >= 'n; (-1) >= 'm >- "false" } -->
   sequent { <H> >- 'n = 'm in nat }

interactive noteq2ge_elim {| ge_elim [] |} 'H :
   [wf] sequent { <H>; x: "not"{'a = 'b in nat}; <J['x]> >- 'a in nat } -->
   [wf] sequent { <H>; x: "not"{'a = 'b in nat}; <J['x]> >- 'b in int } -->
   sequent { <H>; x: "not"{'a = 'b in nat}; <J['x]>; 'a >= 'b +@ 1 >- 'C['x] } -->
   sequent { <H>; x: "not"{'a = 'b in nat}; <J['x]>; 'b >= 'a +@ 1 >- 'C['x] } -->
   sequent { <H>; x: "not"{'a = 'b in nat}; <J['x]> >- 'C['x] }

interactive nat_plusone {| nth_hyp |} 'H :
   sequent { <H>; a: nat; <J['a]> >- 'a +@ 1 in nat }

interactive nat_plus {| intro [AutoMustComplete] |} :
   sequent { <H> >- 'a in nat } -->
   sequent { <H> >- 'b in nat } -->
   sequent { <H> >- ('a +@ 'b) in nat }

interactive natInduction {| elim [ThinFirst thinT; ThinOption thinT] |} 'H  :
   [base] sequent { <H>; n: nat; <J['n]> >- 'C[0] }  -->
   [step] sequent { <H>; n: nat; <J['n]>; m: nat;  'C['m] >- 'C['m +@ 1] }  -->
   sequent { <H>; n: nat; <J['n]> >- 'C['n] }

let thinNextLastT n = (thinT (-2) thenT thinT n)

interactive natInduction2 {| elim [ThinOption thinNextLastT] |} 'H  :
   [base] sequent { <H>; n: nat; <J['n]> >- 'C[0] }  -->
   [step] sequent { <H>; n: nat; <J['n]>; m: nat; 'm<'n; 'C['m] >- 'C['m +@ 1] }  -->
   sequent { <H>; n: nat; <J['n]> >- 'C['n] }

interactive natFullInduction (* {| elim [SelectOption 1; ThinOption thinT] |} *) 'H  :
   sequent { <H>; n: nat; <J['n]>; m: nat; all k: nat. (('k < 'm) => 'C['k]) >- 'C['m] }  -->
   sequent { <H>; n: nat; <J['n]> >- 'C['n] }

interactive natBasedFullInduction 'H bind{x.'f['x]} :
    [wf] sequent { <H>; x: 'T; <J['x]>; y : 'T >- 'f['y] in nat } -->
    sequent { <H>; x: 'T; <J['x]>; x1 : 'T; all x2 : 'T. (('f['x2] < 'f['x1]) => 'C['x2]) >- 'C['x1] } -->
    sequent { <H>; x: 'T; <J['x]> >- 'C['x] }

interactive natBackInduction 'n bind{x.'C['x]}  :
   [wf] sequent { <H> >- 'n in nat }  -->
   [base] sequent { <H> >- 'C['n] }  -->
   [step] sequent { <H>; m: nat;  z: 'C['m +@ 1] >- 'C['m] }  -->
   sequent { <H>  >- 'C[0] }

interactive indEquality {| intro [complete_unless_member] |} bind{z. 'T['z]} :
   [wf] sequent { <H> >- 'n1 = 'n2 in nat } -->
   [base] sequent { <H> >- 'base1 = 'base2 in 'T[0] } -->
   [step] sequent { <H>; x: nat; 0<'x; le{'x;'n1}; y: 'T['x -@ 1] >- 'up1['x; 'y] = 'up2['x; 'y] in 'T['x] } -->
   sequent { <H> >- ind{'n1; 'base1; k1, l1. 'up1['k1; 'l1]} = ind{'n2; 'base2; k2, l2. 'up2['k2; 'l2]} in 'T['n1] }

interactive splitNat 'n :
   [wf] sequent { <H> >- 'n in nat } -->
   [base] sequent { <H>; 'n = 0 in nat >- 'C } -->
   [step] sequent { <H>; 'n in { x: int| 'x >= 1 } >- 'C } -->
   sequent { <H> >- 'C }

interactive_rw reduce_ind_up2 'n:
   'n in { x: int| 'x >= 1 } -->
   ind{'n; 'base; k,l. 'up['k;'l]} <-->
   ('up['n; ind{'n -@ 1 ; 'base; k,l. 'up['k;'l]}])

doc docoff

let splitNatT =
   let rec iter i =
      if i = 0 then
         idT
      else
         let tac = tryT (progressT (hypSubstT (-1) i) thenT rwh reduce_ind_base i) in
            if i = 1 then tac else (tac thenT iter (i - 1))
   in
      argfunT (fun t p ->
         splitNat t thenLT [
            autoT;
            iter (Sequent.hyp_count p) thenT thinT (-1);
            (onAllMHypsT (rwh (reduce_ind_up2 t)) thenAT nthHypT (-1)) thenMT (**)
               (dT (-1) thenT dT (-1) thenT thinT (-2))
         ])

interactive finiteNatType {| intro []; nth_hyp |} :
   sequent { <H> >- 'k in int} -->
   sequent { <H> >- "type"{nat{'k}} }

interactive finiteNatUniv {| intro []; nth_hyp |} :
   sequent { <H> >- 'k in int} -->
   sequent { <H> >- nat{'k} in univ[i:l] }

interactive finiteNatMemberEquality {| intro []; nth_hyp |} :
   sequent { <H> >- 'a = 'b in int_seg{0; 'k} } -->
   sequent { <H> >- 'a = 'b in nat{'k} }

interactive finiteNatElimination {| elim [] |} 'H :
   sequent { <H>; x: int; v:'x >= 0; w: 'x < 'k; <J['x]> >- 'C['x] }  -->
   sequent { <H>; x: nat{'k}; <J['x]> >- 'C['x] }

interactive finiteNat_ge_elim {| ge_elim [] |} 'H :
	[wf] sequent { <H>; x: int; <J['x]> >- 'k in int } -->
   sequent { <H>; x: int; <J['x]>; 'x >= 0; 'k >= 'x+@1 >- 'C['x] }  -->
   sequent { <H>; x: nat{'k}; <J['x]> >- 'C['x] }

interactive finiteNat_ge_elim2 {| ge_elim [] |} 'H :
	[wf] sequent { <H>; t: 'a in nat{'k}; <J['t]> >- 'k in int } -->
   sequent { <H>; t: 'a in nat{'k}; <J['t]>; 'a >= 0; 'k >= 'a+@1 >- 'C['t] }  -->
   sequent { <H>; t: 'a in nat{'k}; <J['t]> >- 'C['t] }

interactive finiteNat_ge_elim3 {| ge_elim [not_member] |} 'H :
	[wf] sequent { <H>; t: 'a = 'b in nat{'k}; <J['t]> >- 'k in int } -->
   sequent { <H>; t: 'a = 'b in nat{'k}; <J['t]>; 'a >= 0; 'k >= 'a+@1; 'a >= 'b; 'b >= 'a >- 'C['t] }  -->
   sequent { <H>; t: 'a = 'b in nat{'k}; <J['t]> >- 'C['t] }

interactive finiteNatIsInt {| nth_hyp |} 'H :
   sequent { <H>; x: nat{'k}; <J['x]> >- 'x in int }

interactive finiteNatIsNat {| nth_hyp |} 'H :
   sequent { <H>; x: nat{'k}; <J['x]> >- 'x in nat }

interactive finiteNatIsSmall {| nth_hyp |} 'H :
   sequent { <H>; x: nat{'k}; <J['x]> >- 'x < 'k }

interactive finiteNatIsInt2 {| nth_hyp |} 'H :
   sequent { <H>; t: 'x in nat{'k}; <J['t]> >- 'x in int }

interactive finiteNatIsNat2 {| nth_hyp |} 'H :
   sequent { <H>; t: 'x in nat{'k}; <J['t]> >- 'x in nat }

interactive finiteNatIsSmall2 {| nth_hyp |} 'H :
   sequent { <H>; t: 'x in nat{'k}; <J['t]> >- 'x < 'k }

doc docoff

let natBackInductionT =
   argfunT (fun n p -> natBackInduction n (get_bind_from_arg_or_concl_subst p <<0>>))

interactive max_nat_wf {| intro[AutoMustComplete] |} :
   [wf] sequent { <H> >- 'a in nat } -->
   [wf] sequent { <H> >- 'b in nat } -->
   sequent { <H> >- max{'a; 'b} in nat }

interactive min_nat_wf {| intro [] |} :
   [wf] sequent { <H> >- 'a in nat } -->
   [wf] sequent { <H> >- 'b in nat } -->
   sequent { <H> >- min{'a; 'b} in nat }

(*
 * Some applications
 *)

interactive int_div_rem {| intro [] |} :
   sequent { <H> >- 'm in int } -->
   sequent { <H> >- 'k in int } -->
   sequent { <H> >- 'k > 0 } -->
   sequent { <H> >- exst q: int. exst r: nat. (('m = 'k *@ 'q +@ 'r in int) & 'r < 'k) }

(*
 * If there is a positive number x such that P[x], then there is
 * a smallest positive number k such that P[k], as long as P[y]
 * is well-formed and decidable for any integer y.
 *)
interactive positive_rule1 {| intro [] |} :
   [wf] sequent { <H>; a: int >- "type"{'P['a]} } -->
   [wf] sequent { <H> >- 'n in int } -->
   [wf] sequent { <H> >- 'n > 0 } -->
   [decidable] sequent { <H>; a: int >- decidable{'P['a]} } -->
   sequent { <H> >- (all b: int. ('b > 0 & 'b <= 'n => not{'P['b]})) or (exst u: int. ('u > 0 & 'u <= 'n & 'P['u] & all b: int. (('b > 0 & 'P['b]) => 'b >= 'u))) }

let positiveRule1T = positive_rule1

interactive smallest_positive {| intro [] |} :
   [wf] sequent { <H>; a: int >- "type"{'P['a]} } -->
   [decidable] sequent { <H>; a: int >- decidable{'P['a]} } -->
   [main] sequent { <H> >- exst a: int. ('a > 0 & 'P['a]) } -->
   sequent { <H> >- exst u: int. ('u > 0 & 'P['u] & all b: int. (('b > 0 & 'P['b]) => 'b >= 'u)) }

let positiveRule2T = smallest_positive

(*interactive smallest_positive_elim {| elim [] |} 'H :
   sequent { <H>; x: exst a: int. ('a > 0 & 'P['a]); <J['x]>; y: exst u: int. ('u > 0 & 'P['u] & all b: int. (('b > 0 & 'P['b]) => 'b < 'u)) >- 'C['x] } -->
   sequent { <H>; x: exst a: int. ('a > 0 & 'P['a]); <J['x]> >- 'C['x] }
*)

(*
 * Some arithmetic helper rules.
 *)
interactive nat_plus_one_eq_zero_elim {| elim [] |} 'H :
   [wf] sequent { <H>; <J[it]> >- 'j in nat } -->
   sequent { <H>; x: ('j +@ 1 = 0 in nat); <J['x]> >- 'C['x] }

interactive nat_plus_one_eq_nat_plus_one_elim {| elim [] |} 'H :
   [wf] sequent { <H>; <J[it]> >- 'i in nat } -->
   [wf] sequent { <H>; <J[it]> >- 'j in nat } -->
   sequent { <H>; 'i = 'j in nat; <J[it]> >- 'C[it] } -->
   sequent { <H>; x: ('i +@ 1 = 'j +@ 1 in nat); <J['x]> >- 'C['x] }

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
