(*
 * The refiner deals with proofs and functions on them.
 * We have the following objects in a refiner:
 *    + validation: a validation is a function on proofs
 *       for instance:
 *           f: (H, x:A, y:B, J[pair(x, y)] >> C[pair(x, y)]) -->
 *               (H, x:A, J[x] >> C[x])
 *        this declares "f" to be a validation, which is a function
 *        that takes a proof of the first sequent, and produces a
 *        proof of the second.  These validations can have
 *        arbitrary arity.
 *    + extract: an extract is a form of validation generated
 *          during proof refinement using tactics.
 *    + tactic: a tactic is a "reverse" application of a
 *      validation.  That is, given a validation f: A --> B,
 *      to produce a proof of B, all that is necessary is to
 *      produce a proof of A (modus ponens).
 *
 *    + rewrite: a rewrite can be reduced to an equivalence
 *      of terms in any context:
 *         f: A <--> B
 *      declares a rewrite that will convert an A to a B, or
 *      vice versa in any context.  This is the same as the
 *      validation:
 *         f: C:[A] <--> C:[B]
 *
 *    + cond_rewrite: conditional rewrite that requires
 *      a proof to be valid.  For instance,
 *         p: (x in A # B) --> (pair(x.1, x.2) <--> x)
 *      this rewrite can only be applied in a sequent
 *      calculus, and it means:
 *         p: (H >> x in A # B) --> (C:[pair(x.1, x.2)] <--> C:[x])
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

INCLUDE "refine_error.mlh"

open Lm_symbol

open Printf
open Lm_debug

open Opname
open Term_sig
open Term_base_sig
open Term_man_sig
open Term_subst_sig
open Term_addr_sig
open Term_meta_sig
open Term_shape_sig
open Refine_error_sig
open Rewrite_sig

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Refine%t"

let debug_refiner =
   create_debug (**)
      { debug_name = "refine";
        debug_description = "Display refinement operations";
        debug_value = false
      }

let debug_sentinal =
   create_debug (**)
      { debug_name = "sentinal";
        debug_description = "Display sentinal operations";
        debug_value = false
      }

let debug_rewrites =
   create_debug (**)
      { debug_name = "rewrites";
        debug_description = "Display rewrite applications";
        debug_value = false
      }

module Refine (**)
   (TermType : TermSig)
   (Term : TermBaseSig
    with type term' = TermType.term'
    with type term = TermType.term
    with type seq_hyps = TermType.seq_hyps
    with type seq_goals = TermType.seq_goals
    with type hypothesis = TermType.hypothesis
    with type bound_term' = TermType.bound_term'
    with type bound_term = TermType.bound_term)
   (TermMan : TermManSig
    with type hypothesis = Term.hypothesis
    with type esequent = TermType.esequent
    with type term = TermType.term)
   (TermSubst : TermSubstSig
    with type term = TermType.term)
   (TermAddr : TermAddrSig
    with type term = TermType.term)
   (TermMeta : TermMetaSig
    with type term = TermType.term)
   (TermShape : TermShapeSig
    with type term = TermType.term)
   (Rewrite : RewriteSig
    with type term = TermType.term
    with type address = TermAddr.address)
   (RefineError : RefineErrorSig
    with type term = TermType.term
    with type address = TermAddr.address
    with type meta_term = TermMeta.meta_term) =
struct
   open TermType
   open Term
   open TermMan
   open TermAddr
   open TermMeta
   open TermSubst
   open TermShape
   open Rewrite
   open RefineError

   type term = TermType.term
   type address = TermAddr.address
   type meta_term = TermMeta.meta_term

   (*
    * Refinements are on meta-sequents,
    * which are a restricted form of meta terms,
    * having only dependent functions format.
    *
    * Each hyp is labelled by its first argument.
    *)
   type msequent_free_vars =
      FreeVarsDelayed
    | FreeVars of SymbolSet.t

   type msequent =
      { mutable mseq_vars : msequent_free_vars;
        mseq_goal : term;
        mseq_hyps : term list
      }

   (*
    * Term extract computation.
    * inputs: rule parameters (addrs, terms), goal, subgoal extracts
    *)
   type term_extract = int array -> term list -> term -> term list -> term

   type ml_rewrite = term -> term

   type ml_cond_rewrite =
      SymbolSet.t ->                                 (* Free vars in the msequent *)
      term list ->                                   (* Params *)
      term ->                                        (* Term to rewrite *)
      term * term list * term_extract                (* Extractor is returned *)

   (*
    * A condition relaces an goal with a list of subgoals,
    * and it provides a function to compute the extract.
    *)
   type ml_rule =
      int array ->                                   (* sequent context addresses *)
      msequent ->                                    (* goal *)
      term list ->                                   (* params *)
      msequent list * term_extract                   (* subgoals, extractor *)

   type pre_rule = msequent

   type pre_rewrite = { 
      pre_rw_redex : term;
      pre_rw_contractum : term;
   }
   
   type pre_cond_rewrite = {
      pre_crw_redex : term;
      pre_crw_contractum : term;
      pre_crw_assums : term list;
   }
   
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * A correctness of a proof may depend of a number of axioms refered to in a proof
    *)
   type dependency =
      DepDefinition of opname
    | DepCondRewrite of opname
    | DepRewrite of opname
    | DepRule of opname 

   module Deps = struct
      type t = dependency
      let compare = Pervasives.compare
   end
   module DepSet = Lm_set.LmMake (Deps)

   (*
    * We cash a number of items with each proof derivation
    *)
   type 'a derived_proof = {
      pf_get_extract : unit -> extract;         (* Upcall *)
      pf_create_proof : extract -> 'a;          (* Function for creating a proof out of extract *)
      mutable pf_extract : extract option;      (* Cached extact from an upcall *)
      mutable pf_proof : 'a option;             (* For rules, 'a is an computational content extraction function *)
      mutable pf_dependencies : DepSet.t option (* Cached dependencies *)
   }

   (*
    * A proof has either been computed,
    * or the computation is delayed.
    *)
   and 'a proof =
      PPrim of 'a
    | PDefined
    | PDerived of 'a derived_proof

   (*
    * An extract summarizes a validation that is generated by a tactic.
    *
    * The extract type is a tree of terms.  The substitution is
    * delayed, since in most cases the extract term is never
    * computed.
    *
    * The refiner describes the rule that was applied, and
    * in most cases we also list the params to the rule that
    * was applied so that the validation can be called if
    * necessary.  The head rule of the refiner is the applied
    * rule.
    *)
   and extract =
      { ext_goal : msequent;
        ext_just : ext_just;
        ext_subgoals : msequent list
      }

   and rw_extract =
      { rw_goal : term;
        rw_just : rewrite_just;
        rw_subgoal : term
      }

   and crw_extract =
      { crw_goal : term;
        crw_just : cond_rewrite_just;
        crw_subgoal_term : term;
        crw_subgoals : cond_rewrite_subgoals
      }

   and ext_just =
      RuleJust of rule_just
    | MLJust of rule_just * term_extract
    | RewriteJust of msequent * rewrite_just * msequent
    | CondRewriteJust of msequent * cond_rewrite_just * msequent list
    | ComposeJust of ext_just * ext_just list
    | NthHypJust of msequent * int
    | CutJust of cut_just
    | Identity

   and rule_just =
      { just_goal : msequent;
        just_addrs : int array;
        just_params : term list;
        just_refiner : opname;
        just_subgoals : msequent list
      }

   and cut_just =
      { cut_goal : msequent;
        cut_hyp : term;
        cut_lemma : msequent;
        cut_then : msequent
      }

   and rewrite_just =
      RewriteHere of term * opname * term
    | RewriteML of term * opname * term
    | RewriteCompose of rewrite_just * rewrite_just
    | RewriteAddress of term * address * rewrite_just * term
    | RewriteHigher of term * rewrite_just list * term

   and cond_rewrite_subgoals =
      CondRewriteSubgoalsAddr of address * cond_rewrite_subgoals
    | CondRewriteSubgoalsList of cond_rewrite_subgoals list
    | CondRewriteSubgoals of term list

   and cond_rewrite_just =
      CondRewriteHere of cond_rewrite_here
    | CondRewriteML of cond_rewrite_here * term_extract
    | CondRewriteCompose of cond_rewrite_just * cond_rewrite_just
    | CondRewriteAddress of term * address * cond_rewrite_just * term
    | CondRewriteHigher of term * cond_rewrite_just list * term

   and cond_rewrite_here =
      { cjust_goal : term;
        cjust_params : term list;
        cjust_refiner : opname;
        cjust_subgoal_term : term;
        cjust_subgoals : term list
      }

   (*
    * A refiner contains the following items:
    *    + theorems: terms that are true in a sequent calculus
    *    + rules: implications on proofs
    *    + rewrite: term equivalences in any context
    *    + ml versions of the above
    *
    * refiners can be combined using PairRefiner.
    *)
   and refiner =
      NullRefiner

    | RuleRefiner of rule_refiner
    | MLRuleRefiner of ml_rule_refiner

    | RewriteRefiner of rewrite_refiner
    | MLRewriteRefiner of ml_rewrite_refiner

    | CondRewriteRefiner of cond_rewrite_refiner
    | MLCondRewriteRefiner of ml_cond_rewrite_refiner

    | PairRefiner of refiner * refiner
    | ListRefiner of refiner list
    | LabelRefiner of string * refiner

   and rule_refiner =
      { rule_name : opname;
        rule_info : pre_rule;
        rule_proof : term_extract proof;
        rule_refiner : refiner
      }

   and ml_rule_refiner =
      { ml_rule_name : opname;
        ml_rule_info : ml_rule;
        ml_rule_refiner : refiner
      }

   and rewrite_refiner =
      { rw_name : opname;
        rw_info: pre_rewrite;
        rw_proof : unit proof;
        rw_refiner : refiner
      }
   and ml_rewrite_refiner =
      { ml_rw_name : opname;
        ml_rw_info : ml_rewrite;
        ml_rw_refiner : refiner
      }

   and cond_rewrite_refiner =
      { crw_name : opname;
        crw_info : pre_cond_rewrite;
        crw_proof : unit proof;
        crw_refiner : refiner
      }
   and ml_cond_rewrite_refiner =
      { ml_crw_name : opname;
        ml_crw_info : ml_cond_rewrite;
        ml_crw_refiner : refiner
      }

   (*
    * A Build has a reference to a refiner,
    * and the opname of this module.
    *)
   type build =
      { build_opname : opname;
        mutable build_refiner : refiner;
        build_rules : (opname, pre_rule) Hashtbl.t;
        build_rewrites : (opname, pre_rewrite) Hashtbl.t;
        build_cond_rewrites : (opname, pre_cond_rewrite) Hashtbl.t;
      }

   (*
    * A hashtable is constructed for looking up justifications.
    *)
   type hash =
      { hash_rewrite : (opname, rewrite_refiner) Hashtbl.t;
        hash_cond_rewrite : (opname, cond_rewrite_refiner) Hashtbl.t;
        hash_rule : (opname, rule_refiner) Hashtbl.t;
        hash_refiner : (opname, refiner) Hashtbl.t
      }

   type find =
      { find_rewrite : opname -> rewrite_refiner;
        find_cond_rewrite : opname -> cond_rewrite_refiner;
        find_rule : opname -> rule_refiner;
        find_refiner : opname -> refiner
      }

   type check =
      { check_rewrite : rewrite_refiner -> unit;
        check_cond_rewrite : cond_rewrite_refiner -> unit;
        check_rule : rule_refiner -> unit
      }

   (*
    * This has a similar function.
    * It checks to see if justifications are justifiable.
    *)
   type sentinal =
      { sent_input_form : opname -> unit -> unit;
        sent_rewrite : opname -> pre_rewrite -> unit;
        sent_ml_rewrite : opname -> ml_rewrite -> unit;
        sent_cond_rewrite : opname -> pre_cond_rewrite -> unit;
        sent_ml_cond_rewrite : opname -> ml_cond_rewrite -> unit;
        sent_rule : opname -> pre_rule -> unit;
        sent_ml_rule : opname -> ml_rule -> unit
      }

   (*
    * The tactic type is the basic refinement type, and every
    * element of tactic always produces "correct" refinements
    * by construction.  In other words, only primitive rules can
    * be directly injected into the tactic type, and all else is
    * by composition.
    *)
   type tactic = sentinal -> msequent -> msequent list * ext_just

   (*
    * A rewrite replaces a term with another term.
    *)
   type rw = sentinal -> term -> term * rewrite_just

   (*
    * A conditional rewrite takes a goal, then applies the rewrite
    * and generates subgoals.  The first argument is the sequent
    * the rewrite is being applied to, and the second is the
    * particular subterm to be rewritted.
    *)
   type cond_rewrite = sentinal -> SymbolSet.t -> term -> term * cond_rewrite_subgoals * cond_rewrite_just

   (*
    * These are the forms created at compile time.
    *)
   type prim_tactic = int array -> term list -> tactic
   type prim_cond_rw = term list -> cond_rewrite
   type prim_rewrite =
      PrimRW of rw
    | CondRW of prim_cond_rw

   (*
    * Extract decription for UI purposes.
    *)
   type extract_description =
       EDRule of opname * int list * term list
     | EDRewrite
     | EDCondREwrite
     | EDComposition (* any compilcated steps will fall into this category *)
     | EDNthHyp of int
     | EDCut of term
     | EDIdentity

   (************************************************************************
    * SEQUENT OPERATIONS                                                   *
    ************************************************************************)

   let dest_msequent mseq =
      mseq.mseq_goal, mseq.mseq_hyps

   let msequent_goal mseq = mseq.mseq_goal

   let msequent_nth_assum mseq i = List.nth mseq.mseq_hyps (pred i)

   let msequent_num_assums mseq = List.length mseq.mseq_hyps

   let msequent_free_vars mseq =
      match mseq.mseq_vars with
         FreeVars vars ->
            vars
       | FreeVarsDelayed ->
            let { mseq_goal = goal; mseq_hyps = hyps } = mseq in
            let vars = free_vars_terms (goal :: hyps) in
               mseq.mseq_vars <- FreeVars vars;
               vars

   let remove_red_hbs t =
      if is_sequent_term t then
         let eseq = explode_sequent t in
         let hyps = SeqHyp.to_list eseq.sequent_hyps in
         let hyps' = remove_redundant_hypbindings hyps (SeqGoal.to_list eseq.sequent_goals) in
            if (hyps==hyps') then t else
               mk_sequent_term {eseq with sequent_hyps = SeqHyp.of_list hyps' }
      else
         t

   let msequent_remove_redundant_hypbindings mseq = {
      mseq with
      mseq_goal = remove_red_hbs mseq.mseq_goal;
      mseq_hyps = Lm_list_util.smap remove_red_hbs mseq.mseq_hyps
   }

   let mk_msequent goal subgoals =
      { mseq_goal = remove_red_hbs goal;
        mseq_hyps = Lm_list_util.smap remove_red_hbs subgoals;
        mseq_vars = FreeVarsDelayed
      }

    (*
     * Check that all the hyps in the list are equal.
     *)
   let equal_hyps hyps t =
      let check hyps' =
         List.for_all2 alpha_equal hyps' hyps
      in
         List.for_all check t

   (*
    * Compare two sequents for alpha eqivalence.
    *)
   let msequent_alpha_equal seq1 seq2 =
      if seq1 == seq2 then
         (* This is the common case *)
         true
      else
         let { mseq_goal = goal1; mseq_hyps = hyps1 } = seq1 in
         let { mseq_goal = goal2; mseq_hyps = hyps2 } = seq2 in
         let rec compare = function
            hyp1::hyps1, hyp2::hyps2 ->
               alpha_equal hyp1 hyp2 & compare (hyps1, hyps2)
          | [], [] ->
               true
          | _ ->
               false
         in
            alpha_equal goal1 goal2 & compare (hyps1, hyps2)

   (*
    * Split the goals from the hyps.
    *)
   let rec split_msequent_list = function
      { mseq_goal = goal; mseq_hyps = hyps }::t ->
         let goals, hypsl = split_msequent_list t in
            goal :: goals, hyps :: hypsl
    | [] ->
         [], []

   (************************************************************************
    * TACTICS                                                              *
    ************************************************************************)

   (*
    * Refinement is just application.
    * The application is doubled: the first argument is
    * for type tactic, and the second is for type safe_tactic.
    *)
   let refine sent (tac : tactic) (seq : msequent) =
      let subgoals, just = tac sent seq in
         subgoals, { ext_goal = seq; ext_just = just; ext_subgoals = subgoals }

   (*
    * The base tactic proves by assumption.
    *)
   let nth_hyp i sent seq =
      let { mseq_goal = goal; mseq_hyps = hyps } = seq in
         try
            if alpha_equal (List.nth hyps i) goal then
               [], NthHypJust (seq, i)
            else
               REF_RAISE(RefineError ("nth_hyp", StringError "hyp mismatch"))
         with
            Failure "nth" ->
               REF_RAISE(RefineError ("nth_hyp", IntError i))

   (*
    * Cut rule.
    *)
   let cut t sent seq =
      let { mseq_hyps = hyps; mseq_goal = goal } = seq in
      let cut_lemma = { mseq_vars = FreeVarsDelayed; mseq_hyps = hyps; mseq_goal = t } in
      let cut_then = { mseq_vars = FreeVarsDelayed; mseq_hyps = hyps @ [t]; mseq_goal = goal } in
      let cut_info = { cut_goal = seq; cut_hyp = t; cut_lemma = cut_lemma; cut_then = cut_then } in
         [cut_lemma; cut_then], CutJust cut_info

   let subgoals_of_extract ext = ext.ext_subgoals

   (*
    * Compose two extracts.
    * The subgoals of the first must match with the goals of the second.
    *)
   let compose ext extl =
      let subgoals = List.map (fun ext -> ext.ext_goal) extl in
      let _ =
         if not (Lm_list_util.for_all2 msequent_alpha_equal ext.ext_subgoals subgoals) then
            REF_RAISE(RefineError ("compose", StringError "goal mismatch"))
      in {
         ext with
         ext_just = ComposeJust (ext.ext_just, List.map (fun ext -> ext.ext_just) extl);
         ext_subgoals = Lm_list_util.flat_map (fun ext -> ext.ext_subgoals) extl
      }

   let identity goal =
      { ext_goal = goal; ext_just = Identity; ext_subgoals = [goal] }

   (************************************************************************
    * REGULAR REWRITES                                                     *
    ************************************************************************)

   (*
    * Turn the rewrite into a tactic.
    *)
   let rwtactic i rw sent mseq =
      let { mseq_hyps = hyps; mseq_goal = goal } = mseq in
      let t =
         if i = 0 then
            goal
         else if i <= List.length hyps then
            List.nth hyps (pred i)
         else
            REF_RAISE(RefineError ("rwtactic", StringIntError ("hyp number is out of range", i)))
      in
      let t', just = rw sent t in
      let subgoal =
         if i = 0 then
            { mseq_vars = FreeVarsDelayed; mseq_hyps = hyps; mseq_goal = t' }
         else
            { mseq_vars = FreeVarsDelayed; mseq_hyps = Lm_list_util.replace_nth (pred i) t' hyps; mseq_goal = goal }
      in
         [subgoal], RewriteJust (mseq, just, subgoal)

   (*
    * Apply a rewrite at an address.
    *)
   let rwaddr addr rw sent t =
      let t', just = apply_fun_arg_at_addr (rw sent) addr t in
         t', RewriteAddress (t, addr, just, t')

   (*
    * Apply the rewrite to the outermost terms where it
    * does not fail.
    *)
   let rwhigher rw sent t =
      let t', justs = apply_fun_higher (rw sent) t in
         t', RewriteHigher (t, justs, t')

   (*
    * Composition is supplied for efficiency.
    *)
   let andthenrw rw1 rw2 sent t =
      let t', just =
         rw1 sent t
      in
      let t'', just' =
         rw2 sent t'
      in
         t'', RewriteCompose (just, just')

   let orelserw rw1 rw2 sent t =
      IFDEF VERBOSE_EXN THEN
         try rw1 sent t with
            RefineError (name1, x) ->
               try rw2 sent t with
                  RefineError (name2, y) ->
                     raise (RefineError ("orelserw", PairError (name1, x, name2, y)))
      ELSE
         try rw1 sent t with
            _ ->
               rw2 sent t
      ENDIF

   (************************************************************************
    * CONDITIONAL REWRITES                                                 *
    ************************************************************************)

   (*
    * Replace the subgoals in the sequent.
    * We have to rename variables to avoid capture,
    * so we need to calculate the binding occurrences to the
    * term in question, and then rename to avoid capture in the goal.
    *)
   let replace_subgoals mseq subgoals =
      let { mseq_hyps = hyps; mseq_goal = seq } = mseq in

      (*
       * We have to rename sequent vars when we substitute into the goal.
       *)
      let replace_subgoal addr t' =
         (* Compute the extra binding variables in the clause *)
         (* HACK!!! This should go away once we implement the crw mechanism properly *)
         let seqtest = TermAddr.replace_subterm seq addr t' in
         let addr' = TermAddr.clause_address_of_address addr in
         let ttst = term_subterm seqtest addr' in
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrites then
               eprintf "Refine.replace_subgoal %a@%s with %a\n\tTest term: %a%t" print_term seq (TermAddr.string_of_address addr) print_term t' print_term ttst eflush;
         ENDIF;
         if SymbolSet.cardinal (free_vars_set ttst) < SymbolSet.cardinal (free_vars_set t') then
            REF_RAISE(RefineError ("Refine.replace_subgoals", StringWrapError("Invalid context for conditional rewrite application",AddressError(addr,seq))));

         (* Now we can replace the goal without fear *)
         let seq = replace_goal seq t' in
            { mseq_vars = FreeVarsDelayed;
              mseq_hyps = hyps;
              mseq_goal = seq
            }
      in

      (*
       * Collect all the subgoals that were given by the conditional
       * rewrite.
       *)
      let rec replace addr subgoals = function
         CondRewriteSubgoalsList subgoals' ->
            List.fold_left (replace addr) subgoals subgoals'
       | CondRewriteSubgoalsAddr (addr', subgoal) ->
            replace (TermAddr.compose_address addr addr') subgoals subgoal
       | CondRewriteSubgoals terms ->
            List.fold_left (fun subgoals t -> replace_subgoal addr t :: subgoals) subgoals terms
      in
         replace (TermAddr.make_address []) [] subgoals

   (*
    * Apply a conditional rewrite.
    *)
   let crwtactic i (crw : cond_rewrite) (sent : sentinal) (seq : msequent) =
      let { mseq_goal = goal; mseq_hyps = hyps } = seq in
      let t =
         if i = 0 then
            goal
         else if i <= List.length hyps then
            List.nth hyps (pred i)
         else
            REF_RAISE(RefineError ("crwtactic", StringIntError ("hyp is out of range", i)))
      in
      IFDEF VERBOSE_EXN THEN
         if !debug_rewrites then
            eprintf "crwtactic applied to %a%t" print_term t eflush;
      ENDIF;
      let t', subgoals, just = crw sent (msequent_free_vars seq) t in
      let subgoal =
         if i = 0 then
            { mseq_vars = FreeVarsDelayed; mseq_hyps = hyps; mseq_goal = t' }
         else
            { mseq_vars = FreeVarsDelayed; mseq_hyps = Lm_list_util.replace_nth (pred i) t' hyps; mseq_goal = goal }
      in
      let subgoals = subgoal :: replace_subgoals seq subgoals in
         subgoals, CondRewriteJust (seq, just, subgoals)

   (*
    * Apply the rewrite to an addressed term.
    *)
   let crwaddr addr (crw: cond_rewrite) sent bvars t =
      LETMACRO BODY =
         let t', (subgoals, just) =
            let f sent bvars t =
               let t, subgoals, just = crw sent bvars t in
                  t, (subgoals, just)
            in
               apply_var_fun_arg_at_addr (f sent) addr bvars t
         in
            t', CondRewriteSubgoalsAddr (addr, subgoals), CondRewriteAddress (t, addr, just, t')
      IN
      IFDEF VERBOSE_EXN THEN
         try BODY
         with
            RefineError (name, x) ->
               raise (RefineError ("crwaddr", RewriteAddressError (addr, name, x)))
      ELSE
         BODY
      ENDIF

   (*
    * Apply the rewrite at the outermost terms where it does not fail.
    *)
   let crwhigher (crw: cond_rewrite) sent bvars t =
      let t', args =
         let f sent bvars t =
            let t, subgoals, just = crw sent bvars t in
               t, (subgoals, just)
         in
            apply_var_fun_higher (f sent) bvars t
      in
      let subgoals, just = List.split args in
         t', CondRewriteSubgoalsList subgoals, CondRewriteHigher (t, just, t')

   (*
    * Composition is supplied for efficiency.
    *)
   let candthenrw crw1 crw2 sent bvars t =
      let t', subgoals, just =
         crw1 sent bvars t
      in
      let t'', subgoals', just' =
         crw2 sent bvars t'
      in
         t'', CondRewriteSubgoalsList [subgoals; subgoals'], CondRewriteCompose (just, just')

   let corelserw crw1 crw2 sent bvars t =
      IFDEF VERBOSE_EXN THEN
         try crw1 sent bvars t with
            RefineError (name1, x) ->
               try crw2 sent bvars t with
                  RefineError (name2, y) ->
                     raise (RefineError ("corelserw", PairError (name1, x, name2, y)))
      ELSE
         try crw1 sent bvars t with
            _ ->
               crw2 sent bvars t
      ENDIF

   (************************************************************************
    * UTILITIES                                                            *
    ************************************************************************)

   (*
    * Empty refiner.
    *)
   let null_refiner name =
      { build_opname = mk_opname name nil_opname;
        build_refiner = NullRefiner;
        build_rules = Hashtbl.create 19;
        build_rewrites = Hashtbl.create 19;
        build_cond_rewrites = Hashtbl.create 19;
      }

   let refiner_of_build build =
      build.build_refiner

   (*
    * Combine the refiners into a single refiner.
    *)
   let join_refiner build ref1 =
      build.build_refiner <- PairRefiner (build.build_refiner, ref1)

   (*
    * Label a refiner with the name of the module.
    *)
   let label_refiner build name =
      let refiner = LabelRefiner (name, build.build_refiner) in
         build.build_refiner <- refiner;
         refiner

   (*
    * Search for an axiom by name.
    *)
   let find_refiner refiner name =
      let rec search refiners = function
         NullRefiner ->
            refiners, None
       | RuleRefiner { rule_name = n; rule_refiner = next }
       | RewriteRefiner { rw_name = n; rw_refiner = next }
       | CondRewriteRefiner { crw_name = n; crw_refiner = next } as r ->
            if n = name then
               refiners, Some r
            else
               search refiners next
       | MLRuleRefiner { ml_rule_name = n; ml_rule_refiner = next }
       | MLRewriteRefiner { ml_rw_name = n; ml_rw_refiner = next }
       | MLCondRewriteRefiner { ml_crw_name = n; ml_crw_refiner = next } ->
            if n = name then
               REF_RAISE(RefineError (string_of_opname n, StringError "ML rules/rewrites can't be justified"))
            else
               search refiners next
       | LabelRefiner (_, next) as r ->
            if List.memq r refiners then
               refiners, None
            else
               search (r :: refiners) next
       | PairRefiner (next1, next2) ->
            begin
               match search refiners next1 with
                  refiners, None ->
                     search refiners next2
                | x ->
                     x
            end
       | ListRefiner refiners' ->
            let rec search' refiners = function
               refiner :: tl ->
                  begin
                     match search refiners refiner with
                        refiners, None ->
                           search' refiners tl
                      | x ->
                           x
                  end
             | [] ->
                  refiners, None
            in
               search' refiners refiners'
      in
         match search [] refiner with
            _, Some v ->
               v
          | _ ->
               raise Not_found

   (************************************************************************
    * EXTRACTION                                                           *
    ************************************************************************)

   (*
    * Extract decription for UI purposes.
    *)
   let describe_extract ext =
      match ext.ext_just with
         RuleJust j | MLJust (j, _) ->
            EDRule (j.just_refiner, Array.to_list j.just_addrs, j.just_params)
       | RewriteJust _ -> EDRewrite
       | CondRewriteJust _ -> EDCondREwrite
       | ComposeJust _ -> EDComposition
       | NthHypJust (_, i) -> EDNthHyp i
       | CutJust j -> EDCut j.cut_hyp
       | Identity -> EDIdentity

   let defined_rule_err _ =
      raise(Invalid_argument "Refine module bug: rule can not be defined")

   (*
    * When an term is calculated from an extract, we have to search
    * for the justifications in the current refiner.  We save them
    * in a hashtable by their names and their types.
    *)
   let hash_refiner refiner =
      let def_shapes = Hashtbl.create 19 in
      let rewrites = Hashtbl.create 19 in
      let cond_rewrites = Hashtbl.create 19 in
      let axioms = Hashtbl.create 19 in
      let rules = Hashtbl.create 19 in
      let refiners = Hashtbl.create 19 in
      let maybe_add hash name info =
         if not (Hashtbl.mem hash name) then
            Hashtbl.add hash name info
      in
      let rec insert refiners' refiner =
         match refiner with
            MLRewriteRefiner { ml_rw_refiner = next }
          | MLCondRewriteRefiner { ml_crw_refiner = next }
          | MLRuleRefiner { ml_rule_refiner = next } ->
               insert refiners' next
          | RuleRefiner rule ->
               if rule.rule_proof = PDefined then defined_rule_err ();
               let name = rule.rule_name in
                  maybe_add rules name rule;
                  maybe_add refiners name refiner;
                  insert refiners' rule.rule_refiner
          | RewriteRefiner rw ->
               if rw.rw_proof = PDefined then begin
                  let redex = rw.rw_info.pre_rw_redex in
                  let shape = shape_of_term redex in
                  if Hashtbl.mem def_shapes shape then
                     REF_RAISE(RefineError("definitional rewrite",StringTermError("shape is already defined",redex)));
                  Hashtbl.add def_shapes shape rw
               end;
               let name = rw.rw_name in
                  maybe_add rewrites name rw;
                  maybe_add refiners name refiner;
                  insert refiners' rw.rw_refiner
          | CondRewriteRefiner crw ->
               let name = crw.crw_name in
                  maybe_add cond_rewrites name crw;
                  maybe_add refiners name refiner;
                  insert refiners' crw.crw_refiner
          | LabelRefiner (_, next) as r ->
               if List.memq r refiners' then
                  refiners'
               else
                  insert (r :: refiners') next
          | PairRefiner (next1, next2) ->
               insert (insert refiners' next1) next2
          | ListRefiner refiners'' ->
               List.fold_left insert refiners' refiners''
          | NullRefiner ->
               refiners'
      in
      let _ = insert [] refiner in
         { hash_rule = rules;
           hash_rewrite = rewrites;
           hash_cond_rewrite = cond_rewrites;
           hash_refiner = refiners
         }

   (*
    * Lookup values in the hashtable, or print error messages.
    *)
   let find_of_hash { hash_rule = rules;
                      hash_rewrite = rewrites;
                      hash_cond_rewrite = cond_rewrites;
                      hash_refiner = refiners
       } =
      let find_rule name =
         try Hashtbl.find rules name with
            Not_found ->
               REF_RAISE(RefineError (string_of_opname name, StringError "rule is not justified"))
      in
      let find_rewrite name =
         try Hashtbl.find rewrites name with
            Not_found ->
               REF_RAISE(RefineError (string_of_opname name, StringError "rewrite is not justified"))
      in
      let find_cond_rewrite name =
         try Hashtbl.find cond_rewrites name with
            Not_found ->
               REF_RAISE(RefineError (string_of_opname name, StringError "cond_rewrite is not justified"))
      in
      let find_refiner name =
         try Hashtbl.find refiners name with
            Not_found ->
               REF_RAISE(RefineError (string_of_opname name, StringError "refiner is not justified"))
      in
         { find_rule = find_rule;
           find_rewrite = find_rewrite;
           find_cond_rewrite = find_cond_rewrite;
           find_refiner = find_refiner
         }

   (*
    * Also sent the matching.
    *)
   let check_of_find { find_rule = find_rule;
                       find_rewrite = find_rewrite;
                       find_cond_rewrite = find_cond_rewrite
       } =
      let check_rule rl =
         if (find_rule rl.rule_name) != rl then
            REF_RAISE(RefineError (string_of_opname rl.rule_name, StringError "rules do not match"))
      in
      let check_rewrite rw =
         if (find_rewrite rw.rw_name) != rw then
            REF_RAISE(RefineError (string_of_opname rw.rw_name, StringError "rewrites do not match"))
      in
      let check_cond_rewrite crw =
         if (find_cond_rewrite crw.crw_name) != crw then
            REF_RAISE(RefineError (string_of_opname crw.crw_name, StringError "cond_rewrites do not match"))
      in
         { check_rule = check_rule;
           check_rewrite = check_rewrite;
           check_cond_rewrite = check_cond_rewrite
         }

   (*
    * Get the extract term for an item.
    *)
   let get_derivation dp =
      match dp.pf_extract with
         Some e -> e
       | None ->
            let e = dp.pf_get_extract () in
               dp.pf_extract <- Some e;
               e
   
   let get_proof = function
      PPrim p -> p
    | PDerived dp ->
         begin match dp.pf_proof with 
             Some p -> p
           | None ->
               let p = dp.pf_create_proof (get_derivation dp) in
                  dp.pf_proof <- Some p;
                  p
         end
    | PDefined ->
         raise (Invalid_argument "Refine.get_proof")

   (*
    * Check for a valid rewrite justification.
    *)
   let rec check_rewrite_just check = function
      RewriteHere (_, op, _)
    | RewriteML (_, op, _) ->
         check op
    | RewriteAddress (_, _, just, _) ->
         check_rewrite_just check just
    | RewriteCompose (just1, just2) ->
         check_rewrite_just check just1;
         check_rewrite_just check just2
    | RewriteHigher (_, justs, _) ->
         List.iter (check_rewrite_just check) justs

   (*
    * Get the subgoal count of a step in the extract.
    *)
   let rec just_subgoal_count = function
      RuleJust { just_subgoals = subgoals }
    | MLJust ({ just_subgoals = subgoals }, _) ->
         List.length subgoals
    | RewriteJust _ ->
         1
    | CondRewriteJust (_, cond, _) ->
         cond_rewrite_just_subgoal_count cond
    | ComposeJust (_, justl) ->
         List.fold_left (fun count just -> count + just_subgoal_count just) 0 justl
    | NthHypJust _ ->
         0
    | CutJust _ ->
         2
    | Identity ->
         1

   and cond_rewrite_just_subgoal_count = function
      CondRewriteHere { cjust_subgoals = subgoals }
    | CondRewriteML ({ cjust_subgoals = subgoals }, _) ->
         List.length subgoals
    | CondRewriteCompose (_, just)
    | CondRewriteAddress (_, _, just, _) ->
         cond_rewrite_just_subgoal_count just
    | CondRewriteHigher (_, justs, _) ->
         List.fold_left (fun count just -> count + cond_rewrite_just_subgoal_count just) 0 justs

   (*
    * Get the term from an extract.
    * This will fail if some of the rules are not justified.
    *)
   let term_of_extract_nocheck refiner ext (args : term list) =
      if ext.ext_subgoals <> [] then
         raise (Invalid_argument "Refine.term_of_extract: called on an unfinished proof");
      let find = find_of_hash (hash_refiner refiner) in
      (* XXX BUG: We never call check.check_rewrite/check_cond_rewrite, but we should *)
      let check = check_of_find find in
      let check_rewrite just =
         check_rewrite_just (fun opname -> ignore (find.find_refiner opname)) just
      in
      (* XXX HACK: this approach of building a closure on-the-fly is probably too inefficient *)
      let rec construct (rest : (term list -> term) list) = function
         RuleJust just ->
            begin match find.find_refiner just.just_refiner with
               RuleRefiner r ->
                  check.check_rule r;
                  fun args -> get_proof r.rule_proof just.just_addrs just.just_params just.just_goal.mseq_goal (all_args args rest)
             | _ ->
                  raise (Invalid_argument "Refine.term_of_extract: extract refers to a non-rule")
            end
       | ComposeJust (just, justl) ->
            construct (partition_rest rest justl) just
       | MLJust (just, f) ->
            (* XXX BUG: just.just_refiner needs to be checked! *)
            fun args -> f just.just_addrs just.just_params just.just_goal.mseq_goal (all_args args rest)
       | RewriteJust (_, just, _) ->
            check_rewrite just;
            List.hd rest
       | Identity ->
            List.hd rest
       | NthHypJust (_, i) ->
            fun args -> List.nth args i
       | CondRewriteJust (_, just, _) ->
            (* XXX BUG: just needs to be checked! *)
            List.hd rest
       | CutJust _ ->
            match rest with
               [cut_lemma; cut_then] ->
                  fun args -> cut_then (args @ [cut_lemma args])
             | _ ->
                  raise (Invalid_argument "Refine.term_of_extract: cut extract is ill-formed")

      and all_args args rest =
         List.map (fun r -> r args) rest

      and partition_rest rest = function
         just :: justl ->
            let count = just_subgoal_count just in
            let rest, restl = Lm_list_util.split_list count rest in
               (construct rest just) :: partition_rest restl justl
       | [] ->
            if rest <> [] then
               raise (Invalid_argument "Refine.term_of_extract: combination extract is too long");
            []
      in
         try construct [] ext.ext_just args
         with Not_found | Failure _ ->
            raise (Invalid_argument "Refine.term_of_extract: ill-formed extract (bug!)")

   (*
    * An empty sentinal for trying refinements.
    *)
   let any_sentinal =
      let null _ _ = () in
         { sent_input_form = null;
           sent_rewrite = null;
           sent_ml_rewrite = null;
           sent_cond_rewrite = null;
           sent_ml_cond_rewrite = null;
           sent_rule = null;
           sent_ml_rule = null;
         }

   let null_sentinal =
      let null _ _ =
         raise (RefineError ("Refine", StringError "refinements are not allowed with the null sentinal"))
      in
         { sent_input_form = null;
           sent_rewrite = null;
           sent_ml_rewrite = null;
           sent_cond_rewrite = null;
           sent_ml_cond_rewrite = null;
           sent_rule = null;
           sent_ml_rule = null
         }

   (*
    * The sentinal uses a hashtable to lookup valid inferences.
    *)
   let sentinal_of_refiner refiner =
      let rewrites = Hashtbl.create 19 in
      let ml_rewrites = Hashtbl.create 19 in
      let cond_rewrites = Hashtbl.create 19 in
      let ml_cond_rewrites = Hashtbl.create 19 in
      let rules = Hashtbl.create 19 in
      let ml_rules = Hashtbl.create 19 in
      let rec insert refiners = function
         RuleRefiner r ->
               IFDEF VERBOSE_EXN THEN
                  if !debug_sentinal then
                     eprintf "sentinal_of_refiner: add rule %s%t" (string_of_opname r.rule_name) eflush
               ENDIF;
               Hashtbl.add rules r.rule_name r.rule_info;
               insert refiners r.rule_refiner
       | RewriteRefiner rw ->
               IFDEF VERBOSE_EXN THEN
                  if !debug_sentinal then
                     eprintf "sentinal_of_refiner: add rewrite %s%t" (string_of_opname rw.rw_name) eflush
               ENDIF;
               Hashtbl.add rewrites rw.rw_name rw.rw_info;
               insert refiners rw.rw_refiner
       | MLRewriteRefiner mlrw ->
               IFDEF VERBOSE_EXN THEN
                  if !debug_sentinal then
                     eprintf "sentinal_of_refiner: add ML rewrite %s%t" (string_of_opname mlrw.ml_rw_name) eflush;
               ENDIF;
               Hashtbl.add ml_rewrites mlrw.ml_rw_name mlrw.ml_rw_info;
               insert refiners mlrw.ml_rw_refiner
       | CondRewriteRefiner crw ->
               IFDEF VERBOSE_EXN THEN
                  if !debug_sentinal then
                     eprintf "sentinal_of_refiner: add cond_rewrite %s%t" (string_of_opname crw.crw_name) eflush
               ENDIF;
               Hashtbl.add cond_rewrites crw.crw_name crw.crw_info;
               insert refiners crw.crw_refiner
       | MLCondRewriteRefiner mlrw ->
               IFDEF VERBOSE_EXN THEN
                  if !debug_sentinal then
                     eprintf "sentinal_of_refiner: add ML rewrite %s%t" (string_of_opname mlrw.ml_crw_name) eflush;
               ENDIF;
               Hashtbl.add ml_cond_rewrites mlrw.ml_crw_name mlrw.ml_crw_info;
               insert refiners mlrw.ml_crw_refiner
       | MLRuleRefiner mlrule ->
               IFDEF VERBOSE_EXN THEN
                  if !debug_sentinal then
                     eprintf "sentinal_of_refiner: add ML rule %s%t" (string_of_opname mlrule.ml_rule_name) eflush
               ENDIF;
               Hashtbl.add ml_rules mlrule.ml_rule_name mlrule.ml_rule_info;
               insert refiners mlrule.ml_rule_refiner
       | LabelRefiner (_, next) as r ->
            if List.memq r refiners then
               refiners
            else
               insert (r :: refiners) next
       | PairRefiner (next1, next2) ->
            insert (insert refiners next1) next2
       | ListRefiner refiners' ->
            List.fold_left insert refiners refiners'
       | NullRefiner ->
            refiners
      in
      let _ = insert [] refiner in
      let check_sentinal table name v =
         if try Hashtbl.find table name == v with Not_found -> false then
            IFDEF VERBOSE_EXN THEN
               if !debug_sentinal then
                  eprintf "check_sentinal: found %s%t" (string_of_opname name) eflush
            ENDIF
         else
            begin
               eprintf "check_sentinal: failed %s%t" (string_of_opname name) eflush;
               REF_RAISE(RefineError
                            ("check_sentinal",
                             StringStringError ("rule is not valid in this context", (string_of_opname name))))
            end
      in
      let check_input_form name _ =
         raise (RefineError ("check_input_form", StringStringError ("input forms can't be used in a proof", string_of_opname name)))
      in
         { sent_input_form = check_input_form;
           sent_rewrite = check_sentinal rewrites;
           sent_ml_rewrite = check_sentinal ml_rewrites;
           sent_cond_rewrite = check_sentinal cond_rewrites;
           sent_ml_cond_rewrite = check_sentinal ml_cond_rewrites;
           sent_rule = check_sentinal rules;
           sent_ml_rule = check_sentinal ml_rules;
         }

   (************************************************************************
    * RULE                                                                 *
    ************************************************************************)

   let make_wildcard_ext_arg =
      let v = Lm_symbol.make "v" 0 in
      let fold (avoid, vars, conts, hyps) = function
         Context (c, _, _) as hyp ->
            (avoid, vars, c::conts, hyp::hyps)
       | Hypothesis t ->
            let v = Lm_symbol.new_name v (SymbolSet.mem avoid) in
               (SymbolSet.add avoid v, mk_var_term v :: vars , conts, HypBinding(v,t) :: hyps)
       | HypBinding (v,t) as hyp ->
            (avoid, mk_var_term v :: vars , conts, hyp::hyps)
      in fun i t ->
         let v = Lm_symbol.make "ext_arg" i in
            if is_sequent_term t then
               let vars = (SymbolSet.add_list (free_vars_set t) (binding_vars t)) in
               let eseq = explode_sequent t in
               let _, vars, conts, hyps = List.fold_left fold (vars,[],[],[]) (SeqHyp.to_list eseq.sequent_hyps) in
               let goal = mk_so_var_term v conts vars in
                  mk_sequent_term { eseq with sequent_hyps = SeqHyp.of_list hyps; sequent_goals = SeqGoal.of_list [goal] }
            else mk_so_var_term v [] []

   let make_wildcard_ext_args =
      let rec aux i = function
         [] -> []
       | t :: ts -> (make_wildcard_ext_arg i t) :: aux (succ i) ts
      in
         aux 1

   (*
    * Create a rule from a meta-term.
    * We allow first-order rules (T -> ... -> T)
    * where each T must be a term, and the arity is arbitrary,
    * and there are no dependencies.
    *)
   let create_rule build name addrs params mterm =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.create_rule: %s%t" name eflush
      ENDIF;
      let subgoals, goal = unzip_mimplies mterm in
      let seq = mk_msequent goal subgoals in
      let rw = Rewrite.term_rewrite Strict addrs (goal :: params) subgoals in
      let opname = mk_opname name build.build_opname in
      let tac addrs params sent mseq =
         let vars = msequent_free_vars mseq in
         let subgoals = apply_rewrite rw (addrs, vars) mseq.mseq_goal params in
         let make_subgoal subgoal =
            { mseq_vars = FreeVars vars; mseq_goal = subgoal; mseq_hyps = mseq.mseq_hyps }
         in
         let subgoals = List.map make_subgoal subgoals in
         let just =
            RuleJust { just_goal = mseq;
                         just_addrs = addrs;
                         just_params = params;
                         just_refiner = opname;
                         just_subgoals = subgoals
            }
         in
            sent.sent_rule opname seq;
            subgoals, just
      in
         Hashtbl.add build.build_rules opname seq;
         (tac : prim_tactic)

   (*
    * Sentinel of a rule/rewrite.
    *)
   let find_sentinal refiner opname =
      match find_refiner refiner opname with
         RuleRefiner { rule_refiner = r }
       | RewriteRefiner { rw_refiner = r }
       | CondRewriteRefiner { crw_refiner = r } ->
            sentinal_of_refiner r
       | _ ->
            (* Only the above can be user-provable and can be returned by find_sentinel *)
            raise (Invalid_argument "find_sentinal")

   (* XXX HACK: This is a pretty hackish way of combining things *)
   let is_hackable_sequent t =
      is_sequent_term t && (SeqGoal.length (explode_sequent t).sequent_goals) = 1

   let join_ext_arg_hack1 arg assum =
      if is_hackable_sequent assum && not (is_sequent_term arg) then
         replace_goal assum arg
      else arg

   let hack_terms_away =
      let i = ref 0 in
      fun t ->
      if is_hackable_sequent t then
         let seq = explode_sequent t in
         let process (vars, terms) = function
            Context (c, conts, _) ->
               vars, (Context (c, conts, vars) :: terms)
          | h ->
               let v = match h with
                  HypBinding (v, _) -> v
                | Hypothesis t -> incr i; Lm_symbol.make "___" !i
                | Context _ -> Lm_symbol.add ""
               in
                  incr i;
                  (mk_var_term v :: vars), (HypBinding (v, mk_so_var_term (Lm_symbol.make "__" !i) [Lm_symbol.add "TODO"] vars) :: terms)
         in
         let vars, hyps = List.fold_left process ([],[]) (SeqHyp.to_list seq.sequent_hyps) in
         let goal = incr i; SeqGoal.get seq.sequent_goals 0 in
         let goal = if vars <> [] && is_var_term goal then mk_so_var_term (Lm_symbol.make "__" !i) [Lm_symbol.add "TODO"] vars else goal in
            mk_sequent_term { seq with sequent_hyps = SeqHyp.of_list (List.rev hyps); sequent_goals = SeqGoal.of_list [goal] }
      else t

   let join_ext_arg_hack arg assum =
      hack_terms_away (join_ext_arg_hack1 arg assum)

   let term_of_extract refiner ext args =
      if (List.length ext.ext_goal.mseq_hyps) <> (List.length args) then
         raise (Invalid_argument "Refine.term_of_extract: wrong number of term arguments");
      term_of_extract_nocheck refiner ext (List.map2 join_ext_arg_hack1 args ext.ext_goal.mseq_hyps)

   (*
    * Theorem for a previous theorem or rule.
    * We once again use the rewriter to compute the
    * extract.
    *)
   let compute_rule_ext name addrs params goal args result =
      let args = mk_xlist_term (goal :: args) in
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.compute_rule_ext: %s: %a + [%s] [%a] -> %a%t" name print_term args (String.concat ";" (List.map string_of_symbol (Array.to_list addrs))) (print_any_list print_term) params print_term result eflush
      ENDIF;
(*
      let rw = Rewrite.term_rewrite Strict addrs (args :: params) [result] in
      let compute_ext addrs params goal args =
         let args = goal :: args in
            List.hd (apply_rewrite rw (addrs, free_vars_terms args) (mk_xlist_term args) params)
      in
         compute_ext
*)
      (* BUG: extraction still needs work *)
      (fun addrs params goal args ->
           raise (Invalid_argument "extraction disabled for now"))

   let justify_rule build name addrs params goal subgoals proof =
      let opname = mk_opname name build.build_opname in
      let r =
         try Hashtbl.find build.build_rules opname
         with Not_found -> REF_RAISE(RefineError (name, StringError "rule was not created"))
      in
         if alpha_equal r.mseq_goal goal && (List.length r.mseq_hyps) = (List.length subgoals) &&
            List.for_all2 alpha_equal r.mseq_hyps subgoals
         then
            build.build_refiner <-
               RuleRefiner {
                  rule_name = opname;
                  rule_info = r;
                  rule_proof = proof;
                  rule_refiner = build.build_refiner;
               }
         else
            REF_RAISE(RefineError (name, StringError "rule mismatch"))
   
   let prim_rule build name addrs params mterm args result =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.prim_rule: %s%t" name eflush
      ENDIF;
      let subgoals, goal = unzip_mimplies mterm in
         (* XXX BUG TODO: in addition to doing join_ext_arg_hack,
          * we need to make sure the args are "univeral" and will always match *)
         if (List.length subgoals) <> (List.length args) then
            raise (Invalid_argument "Refine.add_prim_rule: wrong number of term arguments");
         let args = List.map2 join_ext_arg_hack args subgoals in
         let result = join_ext_arg_hack1 result goal in
         let compute_ext = compute_rule_ext name addrs params goal args result in
            justify_rule build name addrs params goal subgoals (PPrim compute_ext)

   let delayed_rule build name addrs params mterm _ extf =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.delayed_rule: %s%t" name eflush
      ENDIF;
      let subgoals, goal = unzip_mimplies mterm in
      let mseq = mk_msequent goal subgoals in
      let compute_ext ext =
         if not (msequent_alpha_equal ext.ext_goal mseq) then begin
            eprintf "Rule:%t[%a] --> %a%tExtract%t[%a] --> %a%t" eflush (print_any_list print_term) subgoals print_term goal eflush eflush (print_any_list print_term) ext.ext_goal.mseq_hyps  print_term ext.ext_goal.mseq_goal eflush;
            REF_RAISE(RefineError (name, StringError "extract does not match"))
         end;
         let args = make_wildcard_ext_args subgoals in
            compute_rule_ext name addrs params goal args (term_of_extract_nocheck build.build_refiner ext args)
      in
      let dp = {
         pf_get_extract = extf;
         pf_create_proof = compute_ext;
         pf_extract = None;
         pf_proof = None;
         pf_dependencies = None;
      } in
         justify_rule build name addrs params goal subgoals (PDerived dp)

   let derived_rule build name addrs params mterm _ ext =
      delayed_rule build name addrs params mterm () (fun _ -> ext)

   (*
    * An ML rule
    *)
   let create_ml_rule build name mlr =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_ml_rule: %s%t" name eflush
      ENDIF;
      let opname = mk_opname name build.build_opname in
      let tac addrs params sent mseq =
         let subgoals, ext = mlr addrs mseq params in
         let just =
            MLJust ({ just_goal = mseq;
                      just_addrs = addrs;
                      just_params = params;
                      just_refiner = opname;
                      just_subgoals = subgoals
                    }, ext)
         in
            sent.sent_ml_rule opname mlr;
            subgoals, just
      in
         build.build_refiner <-
            MLRuleRefiner {
               ml_rule_name = opname;
               ml_rule_info = mlr;
               ml_rule_refiner = build.build_refiner
            };
         (tac : prim_tactic)

   (*
    * Just do the checking.
    *)
   let check_rule name addrs params mterm =
      let subgoals, goal = unzip_mimplies mterm in
      let vars = free_vars_terms (goal::subgoals) in
         ignore (Rewrite.term_rewrite Strict addrs (goal::params) subgoals);
         List.iter (fun p -> if is_var_term p && not (SymbolSet.mem vars (dest_var p)) then
            REF_RAISE(RefineError("check_rule", StringVarError("Unused parameter", dest_var p)))) params

   (************************************************************************
    * REWRITE                                                              *
    ************************************************************************)

   (*
    * See if the rewrite will compile.
    *)
   let check_rewrite name params subgoals redex contractum =
      ignore(Rewrite.term_rewrite Strict empty_args_spec (redex::params) (contractum::subgoals))

   (*
    * Create a simple rewrite from a meta-term.
    * The rewrite must be a MetaIff.
    *)
   let create_rewrite build name redex contractum =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.create_rewrite: %s%t" name eflush
      ENDIF;
      let rw = Rewrite.term_rewrite Strict empty_args_spec [redex] [contractum] in
      let opname = mk_opname name build.build_opname in
      let pre_rewrite = { pre_rw_redex = redex; pre_rw_contractum = contractum } in
      let rw sent t =
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrites then
               eprintf "Refiner: applying simple rewrite %s to %a%t" name print_term t eflush;
         ENDIF;
         match apply_rewrite rw empty_args t [] with
            [t'] ->
               sent.sent_rewrite opname pre_rewrite;
               t', RewriteHere (t, opname, t')
          | [] ->
               raise (Failure "Refine.create_rewrite: no contracta")
          | _ ->
               raise (Failure "Refine.create_rewrite: multiple contracta")
      in
         Hashtbl.add build.build_rewrites opname pre_rewrite;
         PrimRW rw

   (*
    * Input forms are like rewrites,
    * but they don't get added to the refiner,
    * so they will fail if you every try to use
    * them in a proof.  Use any_sentinal for input_forms.
    *)
   let create_input_form build name strict redex contractum =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.create_input_form: %s%t" name eflush
      ENDIF;
      let strictp = if strict then Strict else Relaxed in
      let rw = Rewrite.term_rewrite strictp empty_args_spec [redex] [contractum] in
      let opname = mk_opname name build.build_opname in
      let rw sent t =
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrites then
               eprintf "Refiner: applying input form %s to %a%t" name print_term t eflush;
         ENDIF;
         match apply_rewrite rw empty_args t [] with
            [t'] ->
               sent.sent_input_form opname ();
               t', RewriteHere (t, opname, t')
          | [] ->
               raise (Failure "Refine.create_input_form: no contracta")
          | _ ->
               raise (Failure "Refine.create_input_form: multiple contracta")
      in
         PrimRW rw

   let justify_rewrite build name redex contractum proof =
      let opname = mk_opname name build.build_opname in
      let rw = 
         try Hashtbl.find build.build_rewrites opname
         with Not_found -> REF_RAISE(RefineError (name, StringError "rewrite was not created"))
      in
         if alpha_equal rw.pre_rw_redex redex && alpha_equal rw.pre_rw_contractum contractum then
            build.build_refiner <- 
               RewriteRefiner {
                  rw_name = opname;
                  rw_info = rw;
                  rw_proof = proof; 
                  rw_refiner = build.build_refiner;
              }
         else
            REF_RAISE(RefineError (name, StringError "rewrite mismatch"))
      

   let prim_rewrite build name redex contractum =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.prim_rewrite: %s%t" name eflush
      ENDIF;
      justify_rewrite build name redex contractum (PPrim ())

   let rec check_bound_vars bvars = function
      [] ->
         ()
    | v::ts ->
         let v = dest_var v in
            if List.mem v bvars then
                check_bound_vars (Lm_list_util.remove v bvars) ts
            else
               REF_RAISE(RefineError ("definitional rewrite", RewriteFreeSOVar v))

   let check_def_bterm bt =
      let bt = dest_bterm bt in
      let _, _, terms = dest_so_var bt.bterm in
         check_bound_vars bt.bvars terms

   let definitional_rewrite build name redex contractum =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.definitional_rewrite: %s%t" name eflush
      ENDIF;
      let _ = List.iter check_def_bterm (dest_term redex).term_terms in
         justify_rewrite build name redex contractum PDefined

   let delayed_rewrite build name redex contractum extf =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.delayed_rewrite: %s%t" name eflush
      ENDIF;
      let compute_ext = function
         { ext_goal = { mseq_goal = goal; mseq_hyps = [] };
           ext_subgoals = [{ mseq_goal = subgoal; mseq_hyps = [] }]} as ext ->
            if alpha_equal goal redex & alpha_equal subgoal contractum then
               ignore(term_of_extract_nocheck build.build_refiner ext [])
            else
               REF_RAISE(RefineError (name, StringError "extract does not match"))
       | _ ->
            REF_RAISE(RefineError (name, StringError "bogus proof"))
      in
      let dp = {
         pf_get_extract = extf;
         pf_create_proof = compute_ext;
         pf_extract = None;
         pf_proof = None;
         pf_dependencies = None;
      } in
         justify_rewrite build name redex contractum (PDerived dp)

   let derived_rewrite build name redex contractum ext =
      delayed_rewrite build name redex contractum (fun _ -> ext)

   (*
    * An ML rewrite.
    *)
   let create_ml_rewrite build name rw =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.create_ml_rewrite: %s%t" name eflush
      ENDIF;
      let opname = mk_opname name build.build_opname in
      let mlrw (sent : sentinal) (t : term) =
         let t' = rw t in
            sent.sent_ml_rewrite opname rw;
            t', RewriteML (t, opname, t')
      in
         build.build_refiner <-
            MLRewriteRefiner {
               ml_rw_name = opname;
               ml_rw_info = rw;
               ml_rw_refiner = build.build_refiner
            };
         PrimRW mlrw

   (************************************************************************
    * CONDITIONAL REWRITE                                                  *
    ************************************************************************)

   (*
    * Conditional rewrite.
    *)
   let create_cond_rewrite build name params subgoals redex contractum =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.create_cond_rewrite: %s%t" name eflush
      ENDIF;
      let rw = Rewrite.term_rewrite Strict empty_args_spec (redex::params) (contractum :: subgoals) in
      let opname = mk_opname name build.build_opname in
      let pre_crw = {
         pre_crw_redex = redex;
         pre_crw_contractum = contractum;
         pre_crw_assums = subgoals;
      } in
      let rw' params (sent : sentinal) (bvars : SymbolSet.t) t =
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrites then
               eprintf "Refiner: applying conditional rewrite %s to %a with bvars = [%a] %t" name print_term t print_symbol_set bvars eflush;
         ENDIF;
         match apply_rewrite rw ([||], bvars) t params with
            (t' :: subgoals) ->
               sent.sent_cond_rewrite opname pre_crw;
                  t',
                  CondRewriteSubgoals subgoals,
                  CondRewriteHere { cjust_goal = t;
                                    cjust_params = params;
                                    cjust_refiner = opname;
                                    cjust_subgoal_term = t';
                                    cjust_subgoals = subgoals
                  }
             | [] ->
                  raise (Failure "Refine.create_cond_rewrite: no contracta")
      in
         Hashtbl.add build.build_cond_rewrites opname pre_crw;
         CondRW rw'

   let justify_cond_rewrite build name params subgoals redex contractum proof =
      let opname = mk_opname name build.build_opname in
      let crw =
         try Hashtbl.find build.build_cond_rewrites opname
         with Not_found -> REF_RAISE(RefineError (name, StringError "conditional rewrite was not created"))
      in
         if alpha_equal crw.pre_crw_redex redex && alpha_equal crw.pre_crw_contractum contractum 
            && alpha_equal (mk_xlist_term crw.pre_crw_assums) (mk_xlist_term subgoals)
         then
            build.build_refiner <-
               CondRewriteRefiner {
                  crw_name = opname;
                  crw_info = crw;
                  crw_proof = proof;
                  crw_refiner = build.build_refiner;
               }
         else
            REF_RAISE(RefineError (name, StringError "conditional rewrite mismatch"))

   let prim_cond_rewrite build name params subgoals redex contractum =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.prim_cond_rewrite: %s%t" name eflush
      ENDIF;
      justify_cond_rewrite build name params subgoals redex contractum (PPrim ())

   let delayed_cond_rewrite build name params subgoals redex contractum extf =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_delayed_cond_rewrite: %s%t" name eflush
      ENDIF;
      let compute_ext ext =
         let { ext_goal = goal; ext_subgoals = subgoals' } = ext in
         let { mseq_goal = goal; mseq_hyps = goal_hyps } = goal in
         let subgoals', sub_hyps = split_msequent_list subgoals' in
         let redex = replace_goal goal redex in
         let contractum = replace_goal goal contractum in
         let subgoals = List.map (replace_goal goal) subgoals in
            if equal_hyps goal_hyps sub_hyps &
               Lm_list_util.for_all2 alpha_equal (redex :: contractum :: subgoals) (goal :: subgoals)
            then
               ignore (term_of_extract_nocheck build.build_refiner ext [])
            else
               REF_RAISE(RefineError (name, StringError "derivation does not match"))
      in
      let dp = {
         pf_get_extract = extf;
         pf_create_proof = compute_ext;
         pf_extract = None;
         pf_proof = None;
         pf_dependencies = None;
     } in
         justify_cond_rewrite build name params subgoals redex contractum (PDerived dp)

   let derived_cond_rewrite build name params args redex contractum ext =
      delayed_cond_rewrite build name params args redex contractum (fun _ -> ext) 

   (*
    * An ML rewrite.
    *)
   let create_ml_cond_rewrite build name rw =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_ml_cond_rewrite: %s%t" name eflush
      ENDIF;
      let opname = mk_opname name build.build_opname in
      let crw params (sent : sentinal) (bvars : SymbolSet.t) t =
         let t', subgoals, ext = rw bvars params t in
            sent.sent_ml_cond_rewrite opname rw;
            t',
            CondRewriteSubgoals subgoals,
            CondRewriteML ({ cjust_goal = t;
                             cjust_params = params;
                             cjust_refiner = opname;
                             cjust_subgoal_term = t';
                             cjust_subgoals = subgoals
                           }, ext)
      in
         build.build_refiner <-
            MLCondRewriteRefiner {
               ml_crw_name = opname;
               ml_crw_info = rw;
               ml_crw_refiner = build.build_refiner
            };
         CondRW crw
end

