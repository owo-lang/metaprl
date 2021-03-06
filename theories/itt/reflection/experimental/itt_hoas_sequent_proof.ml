doc <:doc<
   @module[Itt_hoas_sequent_proof]

   Provability in a sequent logic.
   @docoff

   ----------------------------------------------------------------

   @begin[license]
   Copyright (C) 2005 Mojave Group, Caltech

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

   Author: Jason Hickey
   @email{jyh@cs.caltech.edu}
   @end[license]

   @parents
>>
extends Itt_hoas_bterm_wf
extends Itt_hoas_proof
extends Itt_hoas_meta_types
extends Itt_hoas_sequent
extends Itt_hoas_sequent_term
extends Itt_hoas_sequent_bterm
extends Itt_hoas_sequent_proof_step

doc docoff

open Lm_num
open Lm_debug
open Lm_printf
open Lm_int_set
open Basic_tactics
open Simple_print
open Itt_list
open Itt_bool
open Itt_dfun
open Itt_dprod
open Itt_logic
open Itt_struct
open Itt_int_base
open Itt_hoas_base
open Itt_hoas_vbind
open Itt_hoas_vector
open Itt_hoas_proof
open Itt_hoas_sequent
open Itt_hoas_sequent_term
open Itt_hoas_sequent_proof_step
open Itt_hoas_normalize
open Itt_hoas_bterm_wf

let debug_sequent_proof =
   create_debug (**)
      { debug_name = "sequent_proof1";
        debug_description = "debug Itt_hoas_sequent_proof tactics";
        debug_value = false
      }

doc <:doc<
   Provability in a sequent logic.
>>
define unfold_Provable_sequent : ProvableSequent{'logic; 'seq} <--> <:xterm<
   (seq in BSequent) && Provable{logic; seq}
>>

define unfold_IsJudgment : IsJudgment{'logic; 'seq} <--> <:xterm<
   ProvableSequent{logic; $`meta_type{| >- meta_member{seq; Judgment{}} |}}
>>

define unfold_ProvableJudgment : ProvableJudgment{'logic; 'seq} <--> <:xterm<
   ProvableSequent{logic; seq} && IsJudgment{logic; seq}
>>

(************************************************************************
 * Well-formedness.
 *)
doc <:doc<
   Well-formedness.
>>
interactive provable_sequent_wf {| intro [] |} : <:xrule<
   "wf" : <H> >- logic in Logic -->
   "wf" : <H> >- seq in BSequent -->
   <H> >- ProvableSequent{logic; seq} Type
>>

interactive provable_sequent_all_list {| intro [] |} : <:xrule<
   "wf" : <H> >- 'l in list -->
   "wf" : <H> >- all_list{'l; x. ('x in BSequent)} -->
   <H> >- all_list{'l; x. Provable{logic; 'x}} -->
   <H> >- all_list{'l; x. ProvableSequent{logic; 'x}}
>>

interactive is_judgment_wf {| intro |} : <:xrule<
   "wf" : <H> >- logic in Logic -->
   "wf" : <H> >- seq in BSequent -->
   <H> >- IsJudgment{logic; seq} Type
>>

interactive provable_judgment_wf {| intro [] |} : <:xrule<
   "wf" : <H> >- logic in Logic -->
   "wf" : <H> >- seq in BSequent -->
   <H> >- ProvableJudgment{logic; seq} Type
>>

(************************************************************************
 * Intro rules.
 *)
doc <:doc<
   A @tt[ProvableSequent] judgment introduction rule is provable if it can be refined
   by a rule in the logic.  Unfortunately, we have to provide the witness
   eagerly.  However, it should be easy to do so.
>>
interactive provable_sequent_intro0 {| intro [] |} : <:xrule<
   "wf" : <H> >- seq in BSequent -->
   <H> >- Provable{logic; seq} -->
   <H> >- ProvableSequent{logic; seq}
>>

interactive provable_sequent_intro 'premises : <:xrule<
   "wf" : <H> >- logic in Logic -->
   "wf" : <H> >- premises in list{BSequent} -->
   "wf" : <H> >- goal in BSequent -->
   "aux" : <H> >- all_list{premises; premise. ProvableSequent{logic; premise}} -->
   <H> >- exists witness: ProofStepWitness. SimpleStep{premises; goal; witness; logic} -->
   <H> >- ProvableSequent{logic; goal}
>>

doc <:doc<
   Use an explicit rule to decompose the << SimpleStep{'premises; 'goal; 'witness; 'logic} >>.
>>
interactive simple_step_intro 'step : <:xrule<
   "wf" : <H> >- logic in Logic -->
   "wf" : <H> >- premises in list{BSequent} -->
   "wf" : <H> >- goal in BSequent -->
   "wf" : <H> >- step in ProofRule -->
   "wf" : <H> >- MemLogic{step; logic} -->
   <H> >- exists witness: ProofStepWitness. "assert"{step (proof_step{premises; goal}, witness)} -->
   <H> >- exists witness: ProofStepWitness. SimpleStep{premises; goal; witness; logic}
>>

doc <:doc<
   Use a sublogic to prove << SimpleStep{'premises; 'goal; 'witness; 'logic} >>.
>>
interactive simple_step_intro0 'step : <:xrule<
   "wf" : <H> >- logic in Logic -->
   "wf" : <H> >- premises in list{BTerm} -->
   "wf" : <H> >- goal in BTerm -->
   "wf" : <H> >- step in ProofRule -->
   "wf" : <H> >- MemLogic{step; logic} -->
   <H> >- exists witness: ProofStepWitness. "assert"{step (proof_step{premises; goal}, witness)} -->
   <H> >- exists witness: ProofStepWitness. SimpleStep{premises; goal; witness; logic}
>>

interactive simple_step_sublogic 'logic1 : <:xrule<
   "wf" : <H> >- logic1 in Logic -->
   "wf" : <H> >- logic2 in Logic -->
   "wf" : <H> >- premises in list{BTerm} -->
   "wf" : <H> >- goal in BTerm -->
   <H> >- SubLogic{logic1; logic2} -->
   <H> >- exists witness: ProofStepWitness. SimpleStep{premises; goal; witness; logic1} -->
   <H> >- exists witness: ProofStepWitness. SimpleStep{premises; goal; witness; logic2}
>>

(************************************************************************
 * Forward chaining.
 *)
doc <:doc<
   Forward-chaining rules, mainly for well-formedness reasoning.
>>
interactive provable_forward 'H : <:xrule<
   <H>; ProvableSequent{logic; seq}; <J>; seq in BSequent >- C -->
   <H>; ProvableSequent{logic; seq}; <J> >- C
>>
doc docoff

let provable_forwardT i =
   provable_forward i
   thenT forward_bsequent_wf (-1)
   thenT rw normalizeBTermC (-1)

doc docon

interactive provable_forward0 'H : <:xrule<
   <H>; Provable{logic; seq}; <J>; seq in BTerm >- C -->
   <H>; Provable{logic; seq}; <J> >- C
>>

interactive provable_judgment_forward {| forward |} 'H : <:xrule<
   <H>; ProvableJudgment{logic; seq}; <J>; ProvableSequent{logic; seq}; IsJudgment{logic; seq} >- C -->
   <H>; ProvableJudgment{logic; seq}; <J> >- C
>>

(************************************************************************
 * Tactics.
 *)
doc docoff

let provable_sequent_term = << ProvableSequent{'logic; 'seq} >>
let provable_sequent_opname = opname_of_term provable_sequent_term
let is_provable_sequent_term = is_dep0_dep0_term provable_sequent_opname
let dest_provable_sequent_term = dest_dep0_dep0_term provable_sequent_opname

let it_term = << it >>
let xconcl_term = << xconcl >>
let vbind_arg_term = << vbind >>
let hyplist_arg_term = << hyplist >>
let hyp_context_arg_term = << hyp_context >>

let mk_hyplist_term v cvars args =
   let seq =
      { sequent_args = hyplist_arg_term;
        sequent_hyps = SeqHyp.singleton (Context (v, cvars, args));
        sequent_concl = xconcl_term
      }
   in
      mk_sequent_term seq

(*
 * Build a vector of it terms.
 *)
let mk_it_vector len =
   let rec collect l i =
      if i = len then
         l
      else
         collect (it_term :: l) (succ i)
   in
      collect [] 0

(*
 * Collect information about all second-order vars and contexts
 * in a term.
 *)
let rec socvars_info info t =
   if is_var_term t then
      info
   else if is_so_var_term t then
      socvars_so_var_info info t
   else if is_context_term t then
      socvars_context_info info t
   else if is_sequent_term t then
      socvars_sequent_info info t
   else
      socvars_bterms_info info (dest_term t).term_terms

and socvars_terms_info info tl =
   List.fold_left socvars_info info tl

and socvars_bterm_info info bt =
   socvars_info info (dest_bterm bt).bterm

and socvars_bterms_info info btl =
   List.fold_left socvars_bterm_info info btl

and socvars_so_var_info (cinfo, soinfo) t =
   let v, cvars, args = dest_so_var t in
   let soinfo = SymbolTable.add soinfo v (cvars, List.length args) in
      socvars_terms_info (cinfo, soinfo) args

and socvars_context_info info t =
   let _, t, _, args = dest_context t in
   let info = socvars_info info t in
      socvars_terms_info info args

and socvars_sequent_info info t =
   let { sequent_hyps = hyps;
         sequent_concl = concl
       } = explode_sequent t
   in
   let info =
      SeqHyp.fold (fun info _ h ->
            match h with
               Hypothesis (_, t) ->
                  socvars_info info t
             | Context (v, cvars, args) ->
                  let cinfo, soinfo = info in
                  let cinfo = SymbolTable.add cinfo v (cvars, List.length args) in
                     socvars_terms_info (cinfo, soinfo) args) info hyps
   in
      socvars_info info concl

(*
 * Get the variable ordering in the proof witness.
 *)
let rec find_var_index cindex soindex t =
   if is_exists_term t then
      let _, _, t = dest_exists t in
         find_var_index cindex soindex t
   else if is_assert_term t then
      find_var_index cindex soindex (dest_assert t)
   else if is_let_cvar_term t then
      let v, _, _, i, _, t = dest_let_cvar_term t in
      let v = Lm_symbol.add v in
      let i = int_of_num (dest_number i) in
      let cindex = IntTable.add cindex i v in
         find_var_index cindex soindex t
   else if is_let_sovar_term t then
      let v, _, _, i, _, t = dest_let_sovar_term t in
      let v = Lm_symbol.add v in
      let i = int_of_num (dest_number i) in
      let soindex = IntTable.add soindex i v in
         find_var_index cindex soindex t
   else
      cindex, soindex

(*
 * Build the context hypotheses.
 *)
let build_bind_context cinfo cvars arity =
   (* Add the contexts to the hyp list *)
   let hyps =
      List.fold_left (fun hyps v ->
            try
               let cvars, arity = SymbolTable.find cinfo v in
                  Context (v, cvars, mk_it_vector arity) :: hyps
            with
               Not_found ->
                  hyps) [] cvars
   in

   (* Add scalar hyps *)
   let rec add_scalar_hyps hyps vars i =
      if i = arity then
         vars, hyps
      else
         let v = Lm_symbol.make "x" i in
         let vars = mk_var_term v :: vars in
         let hyps = Hypothesis (v, it_term) :: hyps in
            add_scalar_hyps hyps vars (succ i)
   in
   let vars, hyps = add_scalar_hyps hyps [] 0 in
   let vars = List.rev vars in
   let hyps = List.rev hyps in
      vars, hyps

(*
 * Build the second-order witness.
 *)
let build_so_witness cinfo soinfo sindex =
   let witness, _ =
      IntTable.fold (fun (witness, index) index' v ->
            if index' <> index then
               raise (RefineError ("Itt_hoas_sequent_proof.build_so_witness", StringIntError ("witness ordering error", index')));

            (* Info for the var *)
            let cvars, arity =
               try SymbolTable.find soinfo v with
                  Not_found ->
                     raise (RefineError ("Itt_hoas_sequent_proof.build_so_witness", StringVarError ("unknown second-order variable", v)))
            in

            (* Construct the context *)
            let vars, hyps = build_bind_context cinfo cvars arity in

            (* The conclusion is the sovar *)
            let t = mk_so_var_term v cvars vars in

            (* Now build the witness term *)
            let seq =
               { sequent_args = vbind_arg_term;
                 sequent_hyps = SeqHyp.of_list hyps;
                 sequent_concl = t
               }
            in
            let t = mk_sequent_term seq in
               t :: witness, succ index) ([], 0) sindex
   in
      mk_list_of_list (List.rev witness)

(*
 * Build the second-order witness.
 *)
let build_context_witness cinfo cindex =
   let witness, _ =
      IntTable.fold (fun (witness, index) index' v ->
            if index' <> index then
               raise (RefineError ("Itt_hoas_sequent_proof.build_context_witness", StringIntError ("witness ordering error", index')));

            (* Info for the var *)
            let cvars, arity =
               try SymbolTable.find cinfo v with
                  Not_found ->
                     raise (RefineError ("Itt_hoas_sequent_proof.build_context_witness", StringVarError ("unknown second-order variable", v)))
            in

            (* Construct the context *)
            let vars, hyps = build_bind_context cinfo cvars arity in

            (* The conclusion is the sovar *)
            let t = mk_hyplist_term v cvars vars in

            (* Now build the witness term *)
            let seq =
               { sequent_args = hyp_context_arg_term;
                 sequent_hyps = SeqHyp.of_list hyps;
                 sequent_concl = t
               }
            in
            let t = mk_sequent_term seq in
               t :: witness, succ index) ([], 0) cindex
   in
      mk_list_of_list (List.rev witness)

(*
 * Build the entire witness term.
 *)
let build_proof_witness_term p =
   let t = concl p in
   let cinfo, soinfo = socvars_info (SymbolTable.empty, SymbolTable.empty) t in
   let cindex, soindex = find_var_index IntTable.empty IntTable.empty t in
   let cvars = build_context_witness cinfo cindex in
   let sovars = build_so_witness cinfo soinfo soindex in
      mk_proof_step_witness_term sovars cvars

(*
 * Unify the terms, then instantiate the existential.
 *)
let dest_proof_step_witness p =
   let witness = build_proof_witness_term p in
      exists_intro witness

let proofStepWitnessT = funT dest_proof_step_witness

(*
 * When applying the @tt[provable_sequent_intro] get the premises from
 * the assumptions.
 *)
let provable_count assums =
   List.fold_left (fun count assum ->
         let t = (explode_sequent assum).sequent_concl in
            if is_provable_sequent_term t then
               succ count
            else
               count) 0 assums

let provable_hyps hyps =
   let hyps =
      SeqHyp.fold (fun hyps _ h ->
            match h with
               Hypothesis (_, t) ->
                  if is_provable_sequent_term t then
                     let _, t = dest_provable_sequent_term t in
                        t :: hyps
                  else
                     hyps
             | Context _ ->
                  hyps) [] hyps
   in
      List.rev hyps

let provable_sequent_intro_tac p =
   let pcount = provable_count (Sequent.all_assums p) in
   let hyps = provable_hyps (explode_sequent (Sequent.goal p)).sequent_hyps in
   let len = List.length hyps in
   let hyps =
      if len < pcount then
         raise (RefineError ("Itt_hoas_sequent_proof", StringError "not enough Provable hyps"))
      else if len = pcount then
         hyps
      else
         Lm_list_util.firstn pcount hyps
   in
   let hyps_list = mk_list_of_list hyps in
      provable_sequent_intro hyps_list

let provableIntroT = funT provable_sequent_intro_tac

(************************************************************************
 * The final tactic.
 *)

(*
 * Bring in all the assums.
 *)
let rec assum_all i len =
   if i > len then
      idT
   else
      assumT i thenT assum_all (succ i) len

let assumAllT =
   funT (fun p -> assum_all 1 (Sequent.num_assums p))

let provableRuleStartT t unfold =
   assumAllT
   thenT tryOnAllMHypsT provable_forwardT
   thenMT rwhAll reduce_bsequent
   thenMT simpleReduceT
   thenMT forwardChainT
   thenMT provableIntroT
   thenMT simple_step_intro t
   thenMT rw (higherC unfold thenC reduceC) 0

let provableRuleT t unfold =
   provableRuleStartT t unfold
   thenMT proofStepWitnessT
   thenT proofRuleWFT

(*
 * Some properties of SubLogic.
 *)
interactive provable_sequent_sub 'logic1 : <:xrule<
   "wf" : <H> >- logic1 in Logic -->
   "wf" : <H> >- logic2 in Logic -->
   <H> >- SubLogic{logic1; logic2} -->
   <H> >- ProvableSequent{logic1; seq} -->
   <H> >- ProvableSequent{logic2; seq}
>>

interactive is_judgment_sub 'logic1 : <:xrule<
   "wf" : <H> >- logic1 in Logic -->
   "wf" : <H> >- logic2 in Logic -->
   <H> >- SubLogic{logic1; logic2} -->
   <H> >- IsJudgment{logic1; seq} -->
   <H> >- IsJudgment{logic2; seq}
>>

interactive provable_judgment_sub 'logic1 : <:xrule<
   "wf" : <H> >- logic1 in Logic -->
   "wf" : <H> >- logic2 in Logic -->
   <H> >- SubLogic{logic1; logic2} -->
   <H> >- ProvableJudgment{logic1; seq} -->
   <H> >- ProvableJudgment{logic2; seq}
>>

(************************************************************************
 * Display.
 *)
dform provable_sequent_df : ProvableSequent{'logic; 'e} =
   szone pushm[0] pushm[3] `"P[" slot{'logic} `"] <<" hspace slot{'e} popm hspace `">>" popm ezone

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
