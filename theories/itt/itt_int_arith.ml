doc <:doc<
   @begin[doc]
   @module[Itt_int_arith]

	This module defines @hrefconv[normalizeC] and @hreftactic[arithT].

   @noindent @hrefconv[normalizeC] converts polynomials to the canonical form.

   @noindent @hreftactic[arithT] proves simple inequalities.
   @end[doc]

   ----------------------------------------------------------------

   @begin[license]
   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/index.html for information on Nuprl,
   OCaml, and more information about this system.

   Copyright (C) 2003 Yegor Bryukhov, GC CUNY

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

   Author: Yegor Bryukhov
   @email{ynb@mail.ru}
   @end[license]
>>

extends Itt_equal
extends Itt_rfun
extends Itt_logic
extends Itt_bool
extends Itt_int_ext
doc docoff

open Printf
open Lm_debug
open Lm_num
open Opname
open Term_sig
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermSubst
open Refiner.Refiner.TermType
open Refiner.Refiner.RefineError
open Term_order

open Tactic_type
open Tactic_type.Tacticals
open Tactic_type.Conversionals

open Dtactic

open Top_conversionals

open Itt_equal
open Itt_struct
open Itt_logic
open Itt_bool

open Itt_int_base
open Itt_int_ext

module TO=TermOrder(Refiner.Refiner)
open TO

let _ = show_loading "Loading Itt_int_ext%t"

let debug_int_arith =
   create_debug (**)
      { debug_name = "debug_int_arith";
        debug_description = "Print out some debug info as tactics proceed";
        debug_value = false
      }

(*******************************************************
 * ARITH
 *******************************************************)

let reportT = funT (fun p ->
	if !debug_int_arith then
		let g=Sequent.goal p in
		let c=nth_concl g 1 in
		eprintf "Conclusion is:%a%t" debug_print c eflush
	else
		();
	idT
	)

interactive lt2ge 'H :
   [wf] sequent { <H>; x: 'a < 'b; <J['x]> >- 'a in int } -->
   [wf] sequent { <H>; x: 'a < 'b; <J['x]> >- 'b in int } -->
   [main] sequent { <H>; x: 'a < 'b; <J['x]>; 'b >= ('a +@ 1) >- 'C['x] } -->
   sequent { <H>; x: 'a < 'b; <J['x]> >- 'C['x] }

interactive gt2ge 'H :
   [wf] sequent { <H>; x: 'a > 'b; <J['x]> >- 'a in int } -->
   [wf] sequent { <H>; x: 'a > 'b; <J['x]> >- 'b in int } -->
   [main] sequent { <H>; x: 'a > 'b; <J['x]>; 'a >= ('b +@ 1) >- 'C['x] } -->
   sequent { <H>; x: 'a > 'b; <J['x]> >- 'C['x] }

interactive eq2ge 'H :
   sequent { <H>; x: 'a = 'b in int; <J['x]>; 'a >= 'b; 'b >= 'a >- 'C['x] } -->
   sequent { <H>; x: 'a = 'b in int; <J['x]> >- 'C['x] }

interactive eq2ge1 'H :
   sequent { <H>; x: 'a = 'b in int; <J['x]>; 'a >= 'b >- 'C['x] } -->
   sequent { <H>; x: 'a = 'b in int; <J['x]> >- 'C['x] }

interactive eq2ge2 'H :
   sequent { <H>; x: 'a = 'b in int; <J['x]>; 'b >= 'a >- 'C['x] } -->
   sequent { <H>; x: 'a = 'b in int; <J['x]> >- 'C['x] }

interactive notle2ge :
   [wf] sequent { <H> >- 'a in int } -->
   [wf] sequent { <H> >- 'b in int } -->
   [aux] sequent { <H> >- "not"{('a <= 'b)} } -->
   sequent { <H> >- 'a >= ('b +@ 1) }

interactive nequal_elim {| elim [] |} 'H :
   [wf] sequent { <H> >- 'a in int } -->
   [wf] sequent { <H> >- 'b in int } -->
   [main] sequent { <H>; <J[it]>; y: (('a >= 'b +@ 1) or ('b >= 'a +@ 1)) >- 'C[it] } -->
   sequent { <H>; x: nequal{'a;'b}; <J['x]> >- 'C['x] }

(*
let notle2geT t =
   let (l,r)=dest_le t in
   let newt = mk_ge_term l (mk_add_term r <<1>>) in
*)

let anyArithRel2geT = argfunT (fun i p ->
   let t = Sequent.nth_hyp p i in
   if is_le_term t then rw fold_ge i
   else if is_lt_term t then lt2ge i
   else if is_gt_term t then gt2ge i
   else match explode_term t with
      << 'l = 'r in 'tt >> when alpha_equal tt <<int>> && not (alpha_equal l r) -> eq2ge i
    | _ -> idT
   ) (*if is_not_term t then
      let t1=dest_not t in
         if is_ge_term t1 then notge2geT t1
         else if is_le_term t1 then notle2geT t1
         else if is_lt_term t1 then notlt2geT t1
         else if is_gt_term t1 then notgt2geT t1 *)

interactive_rw bnot_lt2ge_rw :
   ('a in int) -->
   ('b in int) -->
   "assert"{bnot{lt_bool{'a; 'b}}} <--> ('a >= 'b)

let bnot_lt2geC = bnot_lt2ge_rw

let lt2ConclT = magicT thenLT [(addHiddenLabelT "wf"); rwh bnot_lt2geC (-1)]
let ltInConcl2HypT = (rwh unfold_lt 0) thenMT lt2ConclT
let gtInConcl2HypT = (rwh unfold_gt 0) thenMT ltInConcl2HypT

interactive_rw bnot_le2gt_rw :
   ('a in int) -->
   ('b in int) -->
   "assert"{bnot{le_bool{'a; 'b}}} <--> ('a > 'b)

let bnot_le2gtC = bnot_le2gt_rw

let leInConcl2HypT =
   (rwh unfold_le 0) thenMT (magicT thenLT [idT;rwh bnot_le2gtC (-1)])

let geInConcl2HypT =
   (rwh unfold_ge 0) thenMT leInConcl2HypT

interactive eq2pair_of_ineq :
   [wf] sequent { <H> >- 'a in int } -->
   [wf] sequent { <H> >- 'b in int } -->
   [main] sequent { <H> >- 'a >= 'b } -->
   [main] sequent { <H> >- 'b >= 'a } -->
   sequent { <H> >- 'a = 'b in int }

let eqInConcl2HypT t =
	let (t,a,b)=dest_equal t in
   if alpha_equal t <<int>> then
   	if alpha_equal a b then
      	idT
		else
			eq2pair_of_ineq thenMT geInConcl2HypT
   else
   	idT

let neqInConcl2HypT =
	(rw (unfold_neq_int thenC (addrC [0] unfold_bneq_int)) 0)
	thenMT
	((dT 0) thenMT (dT (-1)))

let arithRelInConcl2HypT = funT (fun p ->
   let g=Sequent.goal p in
   let t=Refiner.Refiner.TermMan.nth_concl g 1 in
   if is_lt_term t then ltInConcl2HypT
   else if is_gt_term t then gtInConcl2HypT
   else if is_le_term t then leInConcl2HypT
   else if is_ge_term t then geInConcl2HypT
   else if is_equal_term t then eqInConcl2HypT t
   else if is_neq_int_term t then neqInConcl2HypT
   else if is_not_term t then dT 0
   else idT)

let arith_rels=[
	opname_of_term << 'x<'y >>;
	opname_of_term << 'x>'y >>;
	opname_of_term << 'x<='y >>;
	opname_of_term << 'x>='y >>]

let rec is_arith_rel t =
	let op=opname_of_term t in
   (List.mem op arith_rels) or
   (is_equal_term t && (let (t',_,_)=dest_equal t in alpha_equal t' <<int>>)) or
   (is_not_term t && is_arith_rel (dest_not t))

let negativeHyp2ConclT = argfunT (fun i p ->
   let t = Sequent.nth_hyp p i in
	if is_not_term t then
      if is_arith_rel (dest_not t) then
      	(dT i) thenMT arithRelInConcl2HypT
		else
      	idT
	else if is_neq_int_term t then
   	(rw (unfold_neq_int thenC (addrC [0] unfold_bneq_int)) i)
   	thenMT
      ((dT i) thenMT
      (eq_2beq_int thenMT arithRelInConcl2HypT))
   else
   	idT)

interactive_rw mul_BubblePrimitive_rw :
   ( 'a in int ) -->
   ( 'b in int ) -->
   ( 'c in int ) -->
   ('a *@ ('b *@ 'c)) <--> ('b *@ ('a *@ 'c))

let mul_BubblePrimitiveC = mul_BubblePrimitive_rw

(* One step of sorting of production of some terms with simultaneous
   contraction of product of integers
 *)
let mul_BubbleStepC tm =
   if is_mul_term tm then
      let (a,s) = dest_mul tm in
         if is_mul_term s then
            let (b,c) = dest_mul s in
				if is_number_term a then
					if is_number_term b then
						(mul_AssocC thenC (addrC [0] reduceC))
					else
						failC
				else
					if (compare_terms b a)=Less or (is_number_term b) then
						mul_BubblePrimitiveC
					else
                  failC
         else
            if (is_number_term a) & (is_number_term s) then
	       		reduceC
	    		else
               if ((compare_terms s a)=Less & not (is_number_term a)) or
                	(is_number_term s) then
	          		mul_CommutC
	       		else
	          		failC
   else
      failC

let mul_BubbleStep2C = termC mul_BubbleStepC

(* here we apply mul_BubbleStepC as many times as possible thus
   finally we have all mul subterms positioned in order
 *)
let mul_BubbleSortC = repeatC (higherC (mul_BubbleStep2C))

(*
let inject_coefC t =
	if is_add_term t then
   	let (a,b)=dest_add t in
      begin
      	if !debug_int_arith then
         	eprintf "\ninject_coefC: %a %a%t" debug_print a debug_print b eflush;
      	let aC=addrC [0] (mul_Id3C thenC
      							(repeatC (higherC mul_uni_AssocC)) thenC
      							(addrC [0] reduceC)
             				  )
			in
      	if not (is_add_term b) then
      		aC thenC (addrC [1] (mul_Id3C thenC
      									(repeatC (higherC mul_uni_AssocC)) thenC
      									(addrC [0] reduceC)
              					 	  ))
       	else
         	aC
      end
	else
   	failC

let injectCoefC = sweepUpC (termC inject_coefC)
*)

let rec inject_coefC t =
	if is_add_term t then
   	let (a,b)=dest_add t in
      begin
      	if !debug_int_arith then
         	eprintf "\ninject_coefC: %a %a%t" debug_print a debug_print b eflush;
      	let aC=addrC [0] (mul_Id3C thenC
      							(repeatC (higherC mul_uni_AssocC)) thenC
      							(addrC [0] reduceC)
             				  )
			in
      	if not (is_add_term b) then
      		aC thenC (addrC [1] (mul_Id3C thenC
      									(repeatC (higherC mul_uni_AssocC)) thenC
      									(addrC [0] reduceC)
              					 	  ))
       	else
         	aC thenC (addrC [1] (termC inject_coefC))
      end
   else
   	failC

let injectCoefC = higherC (termC inject_coefC)

let mul_normalizeC = injectCoefC thenC mul_BubbleSortC

interactive_rw sum_same_products1_rw :
   ('a in int) -->
   ((number[i:n] *@ 'a) +@ (number[j:n] *@ 'a)) <-->
   ((number[i:n] +@ number[j:n]) *@ 'a)

let sum_same_products1C = sum_same_products1_rw

(*
interactive_rw sum_same_products2_rw :
   ('a in int) -->
   ((number[i:n] *@ 'a) +@ 'a) <--> ((number[i:n] +@ 1) *@ 'a)

let sum_same_products2C = sum_same_products2_rw

interactive_rw sum_same_products3_rw :
   ('a in int) -->
   ('a +@ (number[j:n] *@ 'a)) <--> ((number[j:n] +@ 1) *@ 'a)

let sum_same_products3C = sum_same_products3_rw

interactive_rw sum_same_products4_rw :
   ('a in int) -->
   ('a +@ 'a) <--> (2 *@ 'a)

let sum_same_products4C = sum_same_products4_rw
*)

let same_product_aux a b =
   if (is_mul_term a) & (is_mul_term b) then
      let (a1,a2)=dest_mul a in
      let (b1,b2)=dest_mul b in
      if alpha_equal a2 b2 then
         (true, sum_same_products1C)
      else
         (false, failC)
   else
	  	(false, failC)

let same_productC t =
   if (is_add_term t) then
      let (a,b)=dest_add t in
      if is_add_term b then
         let (b1,b2)=dest_add b in
         let (same, conv)=same_product_aux a b1 in
         if same then
           (add_AssocC thenC (addrC [0] (conv thenC (addrC [0] reduceC))))
         else
           failC
      else
         let (same, conv)=same_product_aux a b in
         if same then
            conv thenC reduceC
         else
            failC
   else
      failC

interactive_rw add_BubblePrimitive_rw :
   ( 'a in int ) -->
   ( 'b in int ) -->
   ( 'c in int ) -->
   ('a +@ ('b +@ 'c)) <--> ('b +@ ('a +@ 'c))

let add_BubblePrimitiveC = add_BubblePrimitive_rw

let stripCoef t =
   if is_mul_term t then
      let (c,t')=dest_mul t in
      if (is_number_term c) then
         t'
      else
         t
   else
      t

(* One step of sorting of sum of some terms with simultenious
   contraction of sum of integers
 *)
let add_BubbleStepC tm =
  (if !debug_int_arith then
		eprintf "\nadd_BubbleStepC: %a%t" debug_print tm eflush;
   if is_add_term tm then
      let (a,s) = dest_add tm in
         if is_add_term s then
            let (b,c) = dest_add s in
	       	if is_number_term a then
					if is_number_term b then
						begin
							if !debug_int_arith then
								eprintf "add_BubbleStepC: adding numbers a b%t" eflush;
							(add_AssocC thenC (addrC [0] reduceC)) thenC (tryC add_Id2C)
						end
					else
						failC
	       	else
                  let a'=stripCoef a in
                  let b'=stripCoef b in
                  if (compare_terms b' a')=Less then
                     add_BubblePrimitiveC
                  else
                     failC
         else
            if (is_number_term a) & (is_number_term s) then
               begin
               	if !debug_int_arith then
							eprintf "add_BubbleStepC: adding numbers a s%t" eflush;
		       		reduceC
               end
	    		else
     				if (compare_terms s a)=Less then
	          		add_CommutC
	       		else
	          		failC
   else
      begin
        	if !debug_int_arith then
				eprintf "add_BubbleStepC: wrong term%t" eflush;
	      failC
      end
  )

(* here we apply add_BubbleStepC as many times as possible thus
   finally we have all sum subterms positioned in order
 *)
let add_BubbleSortC = (repeatC (higherC (termC add_BubbleStepC))) thenC
                      (repeatC (higherC (termC same_productC)))

interactive_rw sub_elim_rw :
   ( 'a in int ) -->
   ( 'b in int ) -->
   ('a -@ 'b ) <--> ('a +@ ((-1) *@ 'b))

let sub_elimC = repeatC (higherC sub_elim_rw)

let add_normalizeC = add_BubbleSortC

doc <:doc<
	@begin[doc]

	@begin[description]
	@item{@conv[normalizeC];
   The @tt[normalizeC] converts polynomials to canonical form (normalizes),
   it is supposed to work not only when applied precisely
   on a polynomial but also when the polynomial is just a subterm of the
   term the rewrite applied  For instance, if you have a hypothesis
   in the form of inequality or equality you can apply this rewrite to the whole
   hypothesis and it will normalize both sides of inequality (or equality).

   Example: The canonical form of <<'b *@ 2 *@ ('a +@ 'c) -@ ('a *@ 'b) +@ 1>> is
   <<1 +@ (('a *@ 'b) +@ (2 *@ ('b *@ 'c)))>>

   The canonical form of a polynomial is achieved by the following steps:
   @begin[enumerate]
   @item{Get rid of subtraction.}

   @item{Open parentheses using distributivity, move parentheses to the right
   using associativity of addition and multiplication, make other simplifications
   encoded in @hrefconv[reduceC].}

   @item{In every monomial sort (commuting) multipliers in increasing order,
   but pull literal integers to the left (we put coefficients first because later
   we have to reduce similar monomials) and multiply them if there is more than one
   literal integer in one monomial. If monomial does not have literal multipliers
   at all, put <<1>> in front of it for uniformity.}

   @item{Sort monomials in increasing order, reducing similar monomials on the fly.
   Again integer literals should be pulled to the left
   (i.e. considered to be the least terms).}

   @item{Get rid of zeros and ones in the resulting term using @hrefconv[reduceC]}

   @end[enumerate]
	}
	@end[description]

	@end[doc]
>>
let normalizeC = sub_elimC thenC
					  (repeatC (higherC mul_add_DistribC)) thenC
					  (repeatC (higherC mul_add_Distrib3C)) thenC
                 reduceC thenC
                 mul_normalizeC thenC
                 add_normalizeC thenC
                 reduceC

doc <:doc< @docoff >>

interactive_rw ge_addContract_rw :
   ( 'a in int ) -->
   ( 'b in int ) -->
   ('a >= ('b +@ 'a)) <--> (0 >= 'b)

interactive_rw ge_addContract_rw1 {| reduce |} :
   ( 'a in int ) -->
   ('a >= (number[i:n] +@ 'a)) <--> (0 >= number[i:n])

interactive_rw ge_addContract_rw2 {| reduce |} :
   ( 'a in int ) -->
   ((number[i:n] +@ 'a) >= 'a) <--> (number[i:n] >= 0)

interactive_rw ge_addContract_rw3 {| reduce |} :
   ( 'a in int ) -->
   ((number[i:n] +@ 'a) >= (number[j:n] +@ 'a)) <--> (number[i:n] >= number[j:n])
(*
let ge_addContractC = ge_addContract_rw1 orelseC ge_addContract_rw2 orelseC ge_addContract_rw3
*)
(*
   Reduce contradictory relation a>=a+b where b>0.
 *)
let reduceContradRelT =
   rw ((addrC [0] normalizeC) thenC (addrC [1] normalizeC) thenC
       (*(tryC ge_addContractC) thenC*)
		 reduceC)

let provideConstantC t =
   if is_number_term t then
      add_Id4C (*idC*)
   else if is_add_term t then
      let (a,b)=dest_add t in
      if is_number_term a then
         idC
      else
         add_Id3C
   else
      add_Id3C

interactive ge_addMono2 'c :
   [wf] sequent { <H> >- 'a in int } -->
   [wf] sequent { <H> >- 'b in int } -->
   [wf] sequent { <H> >- 'c in int } -->
   sequent { <H> >- ('a >= 'b) ~ (('c +@ 'a) >= ('c +@ 'b)) }

interactive_rw ge_addMono2_rw 'c :
   ( 'a in int ) -->
   ( 'b in int ) -->
   ( 'c in int ) -->
   ('a >= 'b) <--> (('c +@ 'a) >= ('c +@ 'b))

let ge_addMono2C = ge_addMono2_rw

let reduce_geLeftC = (addrC [0] (normalizeC thenC (termC provideConstantC)))
let reduce_geRightC = (addrC [1] (normalizeC thenC (termC provideConstantC)))

let reduce_geCommonConstT = argfunT (fun i p ->
   let t = Sequent.nth_hyp p i in
   let (left,right)=dest_ge t in
   let (a,b)=dest_add left in
   if is_number_term a then
      (rw (ge_addMono2_rw (mk_minus_term a)) i)
      thenMT (rw (addrC [0] normalizeC) i)
   else
      idT)

let tryReduce_geT = argfunT (fun i p ->
   let t = Sequent.nth_hyp p i in
      if is_ge_term t then
         (rw reduce_geLeftC i) thenMT
         (tryT (reduce_geCommonConstT i) thenMT
                     (rw reduce_geRightC i))
      else
         idT)

(* Generate sum of ge-relations
 *)
let sumList tl p =
   match tl with
   h::t ->
      let aux (l,r) a =
         let tm = Sequent.nth_hyp p a in
         let (al,ar) = dest_ge tm in
         (mk_add_term al l, mk_add_term ar r) in
      let h_tm = Sequent.nth_hyp p h in
      let (sl, sr)=List.fold_left aux (dest_ge h_tm) t in
      mk_ge_term sl sr
   | [] ->
      let zero = << 0 >> in
         mk_ge_term zero zero

(* Asserts sum of ge-relations and grounds it
 *)
(*
let sumListT = argfunT (fun l p ->
   let s = sumList l p in
   if !debug_int_arith then
   	eprintf "Contradictory term:%a%t" debug_print s eflush;
   (assertT s) thenAT
   				(repeatT (ge_addMono thenT (tryT (onSomeHypT nthHypT)))))
*)
(* This looks like another working solution:
 *					(repeatT ((progressT (onSomeHypT nthHypT)) orelseT ge_addMono)))
 *)

(* Test if term has a form of a>=b+i where i is a number
 *)
let good_term t =
(*
   debug_print stdout t;
   eprintf "\n %s \n" (Opname.string_of_opname (opname_of_term t));
*)
   if is_ge_term t then
     let (_,b)=dest_ge t in
        if is_add_term b then
           let (d,_)=dest_add b in
              (is_number_term d)
        else
           false
   else
     ((*debug_print stdout t;
      eprintf "%s %s\n" (Opname.string_of_opname (opname_of_term t))
                        (Opname.string_of_opname (opname_of_term ge_term));*)
      false
     )

(* Searches for contradiction among ge-relations
 *)
(*
let findContradRelT =
   let rec lprint ch = function
      h::t -> (fprintf ch "%u " h; lprint ch t)
    | [] -> fprintf ch "."
   in funT (fun p ->
   let g=Sequent.goal p in
   let l = Arith.collect good_term g in
   if !debug_int_arith then
   	begin
   		eprintf "Looking for contradiction among:";
   		let _=List.map (fun i -> (eprintf " %i" i)) l in
   		eprintf "%t" eflush;
   		()
   	end;
   let ar=Array.of_list l in
   match Arith.TG.solve (g,ar) with
      Arith.TG.Int (_,r),_ ->
         let aux3 i al = (ar.(i))::al in
         let rl = List.fold_right aux3 r [] in
         if !debug_int_arith then
            eprintf "Hyps to sum:%a%t" lprint rl eflush;
         sumListT rl
    | Arith.TG.Disconnected,_ ->
         raise (RefineError("arithT", StringError "Proof by contradiction - No contradiction found")))
*)

interactive sumGe 'H :
	[wf] 	sequent { <H>; v: 'a >= 'b; <J['v]>; w: 'c >= 'd >- 'a in int } -->
	[wf] 	sequent { <H>; v: 'a >= 'b; <J['v]>; w: 'c >= 'd >- 'b in int } -->
	[wf] 	sequent { <H>; v: 'a >= 'b; <J['v]>; w: 'c >= 'd >- 'c in int } -->
	[wf] 	sequent { <H>; v: 'a >= 'b; <J['v]>; w: 'c >= 'd >- 'd in int } -->
			sequent { <H>; v: 'a >= 'b; <J['v]>; w: 'c >= 'd; ('a +@ 'c) >= ('b +@ 'd) >- 'C['v;'w] } -->
			sequent { <H>; v: 'a >= 'b; <J['v]>; w: 'c >= 'd >- 'C['v;'w] }

let rec sumListAuxT l = funT ( fun p->
	match l with
		[] -> idT
	 | [(_,_,_,tac)] ->
			let i=Sequent.hyp_count p in
			tac thenMT sumGe i
	 | (_,_,_,tac)::tl ->
			let i=Sequent.hyp_count p in
			tac thenMT sumGe i thenMT sumListAuxT tl
)

let sumListT = function
	[] -> idT
 | [(_,_,_,tac)] -> tac
 | (_,_,_,tac)::tl ->
		tac thenMT (sumListAuxT tl)

let term2inequality (i,t) =
   let (a,b)=dest_ge t in
   let (c,d)=dest_add b in
	let n=dest_number c in
	(a,d,n,copyHypT i (-1))

let num0 = num_of_int 0
let num1 = num_of_int 1

let term2term_number t =
	if is_add_term t then
		let a,b=dest_add t in
		if is_number_term a then
			(b,dest_number a)
		else
			(t,num0)
	else
		if is_number_term t then
			(mk_number_term num0, dest_number t)
		else
			(t,num0)

let term2inequality_aux (a,b,n,tac) =
	let a1,a2=term2term_number a in
	let b1,b2=term2term_number b in
	(a1,b1,sub_num (add_num b2 n) a2,tac)

let term2inequality' (i,t) =
	if is_ge_term t then
		let a,b=dest_ge t in
		List.map term2inequality_aux [(a,b,num0,copyHypT i (-1))]
	else if is_le_term t then
		let a,b=dest_le t in
		List.map term2inequality_aux [(b,a,num0,((rw fold_ge i) thenT (copyHypT i (-1))))]
	else if is_gt_term t then
		let a,b=dest_gt t in
		List.map term2inequality_aux [(a,b,num1,gt2ge i)]
	else if is_lt_term t then
		let a,b=dest_lt t in
		List.map term2inequality_aux [(b,a,num1,lt2ge i)]
	else if is_equal_term t then
		let _,a,b=dest_equal t in
		List.map term2inequality_aux [(a,b,num0,eq2ge1 i);(b,a,num0,eq2ge2 i)]
	else
		raise (Invalid_argument "Itt_int_arith.term2triple - unexpected opname")

let rec all_hyps_aux hyps l i =
   if i = 0 then l else
   let i = pred i in
      match SeqHyp.get hyps i with
         Hypothesis t | HypBinding (_, t) ->
            all_hyps_aux hyps ((i+1,t)::l) i
       | Context _ ->
            all_hyps_aux hyps l i

let all_hyps arg =
   let hyps = (Sequent.explode_sequent arg).sequent_hyps in
      all_hyps_aux hyps [] (Term.SeqHyp.length hyps)

let good_term' t =
	let op=opname_of_term t in
   (List.mem op arith_rels) or
   (match explode_term t with
      << 'l = 'r in 'tt >> when alpha_equal tt <<int>> && not (alpha_equal l r) -> true
    | _ -> false)

let findContradRelT = funT ( fun p ->
	let hyps=all_hyps p in
	let aux (i,t) = good_term' t in
	let l=List.filter aux hyps in
	let l'=List.flatten (List.map term2inequality' l) in
	let l''=Arith.find_contradiction l' in
	sumListT l''
)

let reduceIneqT = argfunT ( fun i p ->
	if good_term' (Sequent.nth_hyp p i) then
		rw (allSubC normalizeC) i
	else
		failT
)

doc <:doc<
	@begin[doc]

	@begin[description]
	@item{@tactic[arithT];
   The @tt[arithT] proves simple inequalities. More precisely it can prove
   inequalities that logically follows from hypotheses using associativity and
   commutativity of addition and multiplication, properties of <<0>> and <<1>>,
   reflexivity, transitivity and weak monotonicity of <<Perv!nil >= Perv!nil>>.
   Weak monotonicity is the @hrefrule[lt_addMono] rule:

   $$
   @rulebox{@misspelled{lt@_addMono}; c;
     <<sequent{ <H> >- 'a in int }>>@cr
     	 <<sequent{ <H> >- 'b in int }>>@cr
     	 <<sequent{ <H> >- 'c in int }>>;
     <<sequent{ <H> >- lt_bool{'a; 'b} ~ lt_bool{('a +@ 'c); ('b +@ 'c)} }>>}
   $$

   with restriction to use only literal integers for <<'c>> (or anything that
   can be automatically reduced to literal integer by @hrefconv[reduceC]).

   @tt[arithT] supports addition, multiplication, unary minus and subtraction
   operations. Division and remainder operations are not supported.
   Among arithmetic relations it supports are << cdot = (cdot) in int >>,
	<< nequal {.Perv!nil ; .Perv!nil } >>,
   << Perv!nil < Perv!nil >>, << Perv!nil > Perv!nil >>,
   << Perv!nil <= Perv!nil >>, and << Perv!nil >= Perv!nil >>. Arbitrary many negations
   of these relations are also supported. Other logical connectives are not supported.

   @tt[arithT] puts together everything that was defined in this module:
	@begin[enumerate]
	@item{First it moves arithmetic fact from conclusion to hypotheses in negated form
	using reasoning by contradiction.}

	@item{Then it converts all negative arithmetic facts in hypotheses to positive
	ones,
	it actually adds new hypotheses and leaves originals
	intact. Because there could be several nested negations this tactic should be
	also applied to hypotheses that were just generated by this tactic.}

	@item{Next it converts all positive arithmetic facts in hypotheses
	to <<Perv!nil >= Perv!nil>>-inequalities.}

	@item{Now every << Perv!nil >= Perv!nil >>-inequality should be normalized.}

	@item{Then it tries to find the contradictory inequality
	that logically follows from that normalized <<Perv!nil >= Perv!nil>>-inequalities
	and proves this implication.
   This problem is reduced to search for positive cycle in a directed graph and
   is performed by @hrefmodule[Arith] module. If successful, found inequality
   will be derived from hypotheses.}

	@item{Finally, false is derived from found inequality, this completes proof by
   contradiction scheme.}
	@end[enumerate]}
	@end[description]

	@end[doc]
	@docoff
>>

(* Finds and proves contradiction among ge-relations
 *)
(*
let arithT =
   arithRelInConcl2HypT thenMT
   ((tryOnAllMCumulativeHypsT negativeHyp2ConclT) thenMT
   ((tryOnAllMHypsT anyArithRel2geT) thenMT
   ((tryOnAllMHypsT tryReduce_geT) thenMT
   (findContradRelT thenMT (reduceContradRelT (-1)) ))))
*)
let arithT =
   arithRelInConcl2HypT thenMT
   ((tryOnAllMCumulativeHypsT negativeHyp2ConclT) thenMT
   ((tryOnAllMHypsT reduceIneqT) thenMT
   (findContradRelT thenMT (reduceContradRelT (-1)) )))

interactive test 'a 'b 'c :
sequent { <H> >- 'a in int } -->
sequent { <H> >- 'b in int } -->
sequent { <H> >- 'c in int } -->
sequent { <H>; x: ('a >= ('b +@ 1));
                     t: ('c >= ('b +@ 3));
                     u: ('b >= ('a +@ 0))
                >- "assert"{bfalse} }

interactive test2 'a 'b 'c :
sequent { <H> >- 'a in int } -->
sequent { <H> >- 'b in int } -->
sequent { <H> >- 'c in int } -->
sequent { <H>; x: (('b +@ 1) <= 'a);
                     t: ('c > ('b +@ 2));
                     u: ('b >= ('a +@ 0))
                >- "assert"{bfalse} }

interactive test3 'a 'b 'c :
sequent { <H> >- 'a in int } -->
sequent { <H> >- 'b in int } -->
sequent { <H> >- 'c in int } -->
sequent { <H>; x: (('b +@ 1) <= 'a);
                     t: ('c > ('b +@ 2))
                >- ('b < ('a +@ 0))  }

interactive test4 'a 'b :
sequent { <H> >- 'a in int } -->
sequent { <H> >- 'b in int } -->
sequent { <H>; x: ('a >= 'b);
                     t: ('a < 'b)
                >- "assert"{bfalse} }

interactive test5 'a 'b :
sequent { <H> >- 'a in int } -->
sequent { <H> >- 'b in int } -->
sequent { <H>; x: ('a >= 'b +@ 0);
                     t: ('a < 'b)
                >- "assert"{bfalse} }

interactive test6 'b 'c :
sequent { <H> >- 'a in int } -->
sequent { <H> >- 'b in int } -->
sequent { <H> >- 'c in int } -->
sequent { <H>; x: (('c *@ ('b +@ ('a *@ 'c)) +@ ('b *@ 'c)) >= 'b +@ 0);
                     t: (((((('c *@ 'b) *@ 1) +@ (2 *@ ('a *@ ('c *@ 'c)))) +@
 (('c *@ ((-1) *@ 'a)) *@ 'c)) +@ ('b *@ 'c)) < 'b)
                >- "assert"{bfalse} }

interactive eq2ineq :
	[wf] sequent { <H> >- 'a in int } -->
	[wf] sequent { <H> >- 'b in int } -->
   [main] sequent { <H> >- 'a = 'b in int } -->
	sequent { <H> >- 'a <= 'b }

interactive_rw beq2ineq_rw :
	('a in int) -->
   ('b in int) -->
   beq_int{'a;'b} <--> band{le_bool{'a;'b}; le_bool{'b;'a}}
