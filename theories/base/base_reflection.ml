doc <:doc<
   @begin[doc]
   @module[Base_reflection]

   @end[doc]

   ----------------------------------------------------------------

   @begin[license]
   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/index.html for information on Nuprl,
   OCaml, and more information about this system.

   Copyright (C) 2004 MetaPRL Group

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

   Author: Xin Yu @email{xiny@cs.caltech.edu}
   @end[license]
>>

doc <:doc<
   @begin[doc]
   @parents
   @end[doc]
>>
extends Shell_theory
extends Top_conversionals
doc docoff

open Basic_tactics

declare bterm
declare sequent_arg{'t}
declare term

(*
if_bterm{'bt; 'tt} evaluates to 'tt when 'bt is a well-formed bterm
operator and must fail to evaluate otherwise.

rewrite axiom: if_bterm{bterm{<H>; x:_; <J> >- x}; 'tt} <--> 'tt
ML rewrite axiom:
    if_bterm{bterm{<H> >- _op_{<J1>.t1, ..., <Jn>.tn}}; 'tt}
       <-->
    if_bterm{bterm{<H>; <J1> >- 't1};
       if_bterm{<H>; <J2> >- 't2};
          ...
            if_bterm{<H>; <Jn> >- 'tn}; 'tt}...}}
*)

declare if_bterm{'bt; 'tt}

prim_rw reduce_ifbterm1 'H :
   if_bterm{ sequent [bterm] { <H>; x: 't; <J> >- 'x }; 'tt } <--> 'tt

let bterm_arg = <<sequent_arg{bterm}>>

let hyp_of_var v =
   Hypothesis(v, <<term>>)

let make_bterm_sequent hyps goal =
   mk_sequent_term {
      sequent_args = bterm_arg;
      sequent_hyps = SeqHyp.of_list hyps;
      sequent_goals = SeqGoal.of_list [goal]
   }

let dest_bterm_sequent term =
   let seq = TermMan.explode_sequent term in
      if (SeqGoal.length seq.sequent_goals = 1) && alpha_equal seq.sequent_args bterm_arg then
         SeqHyp.to_list seq.sequent_hyps, (SeqGoal.get seq.sequent_goals 0)
      else raise (RefineError ("dest_bterm_sequent", StringTermError ("not a bterm sequent", term)))

let dest_bterm_sequent_and_rename term vars =
   let seq = TermMan.explode_sequent_and_rename term vars in
      if (SeqGoal.length seq.sequent_goals = 1) && alpha_equal seq.sequent_args bterm_arg then
         SeqHyp.to_list seq.sequent_hyps, (SeqGoal.get seq.sequent_goals 0)
      else raise (RefineError ("dest_bterm_sequent_and_rename", StringTermError ("not a bterm sequent", term)))

ml_rw reduce_ifbterm2 : ('goal :  if_bterm{ sequent[bterm]{ <H> >- 't}; 'tt }) =
   let bt, tt = two_subterms goal in
   let hyps, t = dest_bterm_sequent bt in
   let t' = dest_term (unquote_term t) in
   let rec wrap_if = function
      [] -> tt
    | s :: subterms ->
         let bt = dest_bterm s in
         let new_bterm = make_bterm_sequent (hyps @ (List.map hyp_of_var bt.bvars)) bt.bterm in
            <:con<if_bterm{$new_bterm$;$wrap_if subterms$}>>
   in wrap_if t'.term_terms

let rec onSomeHypC rw = function
   1 -> rw 1
 | i when i > 0 -> rw i orelseC (onSomeHypC rw (i - 1))
 | _ -> failC

let reduce_ifbterm =
   termC (fun goal ->
      let t,tt =  two_subterms goal in
      let seq = TermMan.explode_sequent  t in
      let goal = SeqGoal.get seq.sequent_goals 0 in
         if is_quoted_term goal then reduce_ifbterm2
         else if is_var_term goal then onSomeHypC reduce_ifbterm1 (SeqHyp.length seq.sequent_hyps)
         else failC)

let resource reduce +=
   (<<if_bterm{ sequent[bterm]{ <H> >- 't}; 'tt }>>, reduce_ifbterm)




(*
dest_bterm{'bt} returns a list of sub-bterms of 'bt, undefined if 'bt
is not a well-formed bterm
ML rewrite_axiom:
    dest_bterm{bterm{<H> >- _op_{<J1>.t1, ..., <Jn>.tn}}}
       <-->
    [ bterm{<H>; <J1> >- t1}; ...; bterm{<H>; <Jn> >- tn} ]
*)

declare dest_bterm{'bt}

ml_rw reduce_dest_bterm (* {| reduce |} *) : ('goal :  dest_bterm{ sequent[bterm]{ <H> >- 't} }) =
   let bt = one_subterm goal in
   let hyps, t = dest_bterm_sequent bt in
   let t' = dest_term (unquote_term t) in
   let wrap s =
      let bt = dest_bterm s in
      	make_bterm_sequent (hyps @ (List.map hyp_of_var bt.bvars)) bt.bterm
   in mk_xlist_term (List.map wrap t'.term_terms)

let resource reduce +=
   (<<dest_bterm{ sequent[bterm]{ <H> >- 't} }>>, reduce_dest_bterm)




(*
make_bterm{'bt; 'btl} takes the top-level operator of 'bt and
replaces the subterms with the ones in 'btl (provided they have a
correct arity).

ML rewrite_axiom:
    make_bterm{bterm{<K> >- _op_{<J1>.r1, ..., <Jn>.rn}}};
       [bterm{<H>; <J1> >- t1}; ...; bterm{<H>; <Jn> >- tn}] }
    <-->
    bterm{<H> >- _op_{<J1>.t1, ..., <Jn>.tn}}

(<K> and ri are ignored).
*)

declare make_bterm{'bt; 'bt1}

let get_var = function
   Hypothesis (v, term) -> v
 | Context (v, conts, subterms) ->
      raise (Invalid_argument "Base_reflection.get_bterm_var: bterm cannot have contexts as hypotheses")

let rec ggg lista listb fvars hvar lenh l =
   match lista, listb with
      [], [] -> l
    | a1 :: a, b1:: b ->
         let b1h, b1g = dest_bterm_sequent_and_rename b1 fvars in
            if (List.length b1h - a1 = lenh) then
               let hvar', j1' = Lm_list_util.split lenh (List.map get_var b1h) in
               let b1g' = subst b1g hvar' (List.map mk_var_term hvar) in
                  ggg a b fvars hvar lenh (l @ [mk_bterm j1' b1g'])
            else raise (Invalid_argument "Base_reflection.ggg: unmatched arity.")
    | _, _ -> raise (Invalid_argument "Base_reflection.ggg: unmatched arity.")


ml_rw reduce_make_bterm (* {| reduce |} *) : ('goal :  make_bterm{ 'bt; 'bt1 }) =
   let bt, bt1 = two_subterms goal in
   let hyps1, t = dest_bterm_sequent bt in
   let t' = dest_term (unquote_term t) in
   let bt1' = dest_xlist bt1 in
      if (List.length t'.term_terms = (List.length bt1')) then
         match bt1' with
            [] -> make_bterm_sequent [] t
          | b1 :: b ->
               let fvars = free_vars_set bt1 in
               let b1h, b1g = dest_bterm_sequent_and_rename b1 fvars in
               let lenJr_list = List.map (fun tmp -> List.length (dest_bterm tmp).bvars) t'.term_terms in
               let lenJr_list' = List.tl lenJr_list in
               let lenh = List.length b1h - (List.hd lenJr_list) in
                  if lenh >= 0 then
                     let hvar' = List.map get_var b1h in
                     let hvar, j1 = Lm_list_util.split lenh hvar' in
                     let fvars = SymbolSet.add_list fvars hvar in
                     let terms = (mk_bterm j1 b1g) :: ggg lenJr_list' b fvars hvar lenh [] in
                        make_bterm_sequent (List.map (fun v -> Hypothesis (v, << term >>)) hvar) (quote_term (mk_term t'.term_op terms))
                  else raise (RefineError ("reduce_make_bterm", StringTermError ("not a qualified bterm for replacement", b1)))
      else  raise (RefineError ("reduce_make_bterm", StringError "unmatched arities"))

let resource reduce +=
   (<<make_bterm{ 'bt; 'bt1 }>>, reduce_make_bterm)