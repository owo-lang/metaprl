doc <:doc<
   @module[Auto_tactic]

   The @tt[Auto_tactic] module defines two of the most useful
   tactics in the @MetaPRL prover.  The @tactic[autoT] tactic attempts
   to prove a goal ``automatically,'' and the @tactic[trivialT] tactic
   proves goals that are ``trivial.''  Their implementations are surprisingly
   simple---all of the work in automatic proving is implemented in
   descendent theories.

   This module describes the @emph{generic} implementation of the
   @hreftactic[autoT] and @hreftactic[trivialT] tactics.  They are implemented
   using a resource
   containing collections of tactics that are added by descendent theories.
   The @Comment!resource[auto] resource builds collections of tactics specified by
   a data structure with the following type:

   @begin[center]
   @begin[verbatim]
   type auto_info =
      { auto_name : string;
        auto_tac : tactic;
        auto_prec : auto_prec;
        auto_type : auto_type;
      }

   and auto_type =
      AutoTrivial
    | AutoNormal
    | AutoComplete
   @end[verbatim]
   @end[center]

   The @tt[auto_name] is the name used to describe the entry (for
   debugging purposes).  The @tt[auto_tac] is the actual tactic to try.
   The entries are divided into precedence levels; tactics with higher precedence
   are applied first.

   Finally, @tt[auto_type] specifies how @hreftactic[autoT] and @hreftactic[trivialT]
   will use each particular entry. @tt[AutoTrivial] entries are the only ones
   used by @hreftactic[trivialT]; @hreftactic[autoT] attempts using them before
   any other entries. @tt[AutoComplete] will be used by @hreftactic[autoT] after
   all @tt[AutoTrivial] and @tt[AutoNormal] entries are exhausted. @hreftactic[autoT]
   will consider an application of an @tt[AutoComplete] entry to be successful
   only if it would be able to completely prove all subgoals generated by it.

   This theory also defines the @tactic[byDefT] tactic. @tt{byDefT }@i[conv]
   (where @i[conv] is usually an @tt[unfold_] conversional) uses @i[conv]
   (through @hrefconv[higherC]) on all the assumptions and on the goal and then
   calls @hreftactic[autoT]. The @tactic[byDefsT] tactic that takes a @tt{conv list}
   is defined similarly.

   The tactic @tt[repeatWithRwsT] @i[convs] @i[tac] tries to apply some conversional from
   the @i[convs] list to the goal and in case of a progress applies the tactic @i[tac],
   then repeats it as far as possible.
   @docoff

   ----------------------------------------------------------------

   @begin[license]

   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/htmlman/default.html or visit http://metaprl.org/
   for more information.

   Copyright (C) 1998 Jason Hickey, Cornell University

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

   Author: Jason Hickey @email{jyh@cs.caltech.edu}
   Modified by: Aleksey Nogin @email{nogin@cs.cornell.edu}
   @end[license]
>>

doc <:doc<
   @parents
>>
extends Mptop
extends Top_tacticals
extends Top_conversionals
extends Browser_resource
doc docoff

open Lm_debug
open Lm_printf
open Lm_dag_sig
open Lm_imp_dag

open Term_sig
open Rewrite_sig
open Refiner.Refiner
open Refiner.Refiner.RefineError
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermMeta
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermSubst
open Refiner.Refiner.TermAddr
open Refiner.Refiner.Rewrite
open Mp_resource
open Term_match_table
open Dform

open Tactic_type
open Tactic_type.Sequent
open Tactic_type.Tactic
open Tactic_type.Tacticals
open Top_conversionals
open Browser_resource

(*
 * Debugging.
 *)
let _ =
   show_loading "Loading Auto_tactic%t"

let debug_auto =
   create_debug (**)
      { debug_name = "auto";
        debug_description = "Display auto tactic operations";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The auto tactic just produces a list of tactics to try.
 *)
type auto_prec = unit ImpDag.node

(*
 * The info provided is a name,
 * a precedence, and a function
 * to produce a tactic.  The function
 * is called once per run of the auto tactic.
 *)
type auto_info = {
   auto_name : string;
   auto_prec : auto_prec;
   auto_tac : tactic;
   auto_type : auto_type;
}

and auto_type =
   AutoTrivial
 | AutoNormal
 | AutoComplete

(************************************************************************
 * IMPLEMENTATION - nthHypT                                             *
 ************************************************************************)

(*
 * Opname for pairing a hypothesis and a conclusion into a single term
 *)
declare pair{'hyp;'concl}

let mk_pair_term =
   mk_dep0_dep0_term (opname_of_term <<pair{'hyp;'concl}>>)

let extract_nth_hyp_data =
   let err = RefineError ("extract_nth_hyp_data", StringError "nthHypT tactic doesn't have an entry for this hypothesis/conclusion combination") in
   let rec iterate i (c: (int -> tactic) lazy_lookup) _ =
      try
         let tac, cont = c () in tac i orelseT funT (iterate i cont)
      with
         Not_found ->
            raise err
   in
   fun tbl ->
      argfunT (fun i p ->
         let t = mk_pair_term (Sequent.nth_hyp p i) (Sequent.concl p) in
            iterate i (Term_match_table.lookup_all tbl select_all t) p)

let add_nth_hyp_data tbl (hyp,concl,tac) =
   add_item tbl (mk_pair_term hyp concl) tac

let resource (term * term * (int -> tactic), int -> tactic) nth_hyp =
   Functional {
      fp_empty = empty_table;
      fp_add = add_nth_hyp_data;
      fp_retr = extract_nth_hyp_data;
   }

let nthHypT = argfunT (fun i p -> get_resource_arg p get_nth_hyp_resource i)

let someNthHypT = funT (fun p ->
   onSomeHypT (get_resource_arg p get_nth_hyp_resource))

let explode t =
   let t = TermMan.explode_sequent t in
      SeqHyp.to_list t.sequent_hyps, t.sequent_concl

let process_nth_hyp_resource_annotation name args term_args statement pre_tactic =
   let assums, goal = unzip_mfunction statement in
      match args.spec_ints, args.spec_addrs, term_args, List.map (fun (_, _, t) -> explode t) assums, explode goal with
         [| _ |], [||], [], [], ([ Context _; Hypothesis(_,t1); Context _ ], t2) ->
            [t1, t2, fun i -> Tactic_type.Tactic.tactic_of_rule pre_tactic { arg_ints = [| i |]; arg_addrs = [||] } []]
       | [||], [||], [], [ [Context _], t1 ], ( [Context _], t2) ->
            [t1, t2, fun i -> Tactic_type.Tactic.tactic_of_rule pre_tactic empty_rw_args [] thenT nthHypT i]
       | [||], [||], _, [ [Context _], t1 ], ( [Context _], t2) ->
            let addrs =
               try List.map (fun t -> List.hd (find_subterm t1 (fun t' _ -> alpha_equal t t'))) term_args
               with _ ->
                  raise (Invalid_argument (sprintf "Auto_tactic.improve_nth_hyp: %s: is not an appropriate rule" name))
            in
            let tac = argfunT (fun i p ->
               let hyp = nth_hyp p i in
               let terms = List.map (term_subterm hyp) addrs in
                  Tactic_type.Tactic.tactic_of_rule pre_tactic empty_rw_args terms thenT nthHypT i)
            in
               [t1, t2, tac]
       | _ ->
            raise (Invalid_argument (sprintf "Auto_tactic.improve_nth_hyp: %s: is not an appropriate rule" name))

(************************************************************************
 * IMPLEMENTATION - autoT                                               *
 ************************************************************************)

(*
 * We create a DAG to manage ordering in the tree.
 *)
let dag = ImpDag.create ()

(*
 * Sort the nodes in the list.
 *)
let compare node1 node2 =
   ImpDag.node_rel dag node1.auto_prec node2.auto_prec = LessThan

let auto_tac tac = tac.auto_tac

let sort_nodes = Sort.list compare

let successT i s = funT (fun p ->
   if !i = 0 then
      eprintf " -> succeded on -------------\n%s%t" s eflush;
   incr i;
   eprintf "-------- and got subgoal %i ---------\n%s\n----------------%t" (!i) (string_of_term (get_mode_base top_bookmark "prl" null_shortener) (Sequent.goal p)) eflush;
   idT)

(*
 * Debugging firstT.
 *)
let debugT auto_tac =
   { auto_tac with
     auto_tac = funT (fun p ->
        let s = string_of_term (get_mode_base top_bookmark "prl" null_shortener) (Sequent.goal p) in
        eprintf "Auto: trying %s%t" auto_tac.auto_name eflush;
        (progressT auto_tac.auto_tac) thenT successT (ref 0) s)
   }

let map_progressT tac =
   { tac with auto_tac = progressT tac.auto_tac }

let trivialP  tac = (tac.auto_type == AutoTrivial)
let normalP   tac = (tac.auto_type == AutoNormal)
let completeP tac = (tac.auto_type == AutoComplete)

(*
 * Build an auto tactic from all of the tactics given.
 * A list of tactics to try is constructed.
 * The earlier tactics should be tried first.
 *)
let make_progressT goals tac =
   funT (fun p ->
      let goal = Sequent.goal p in
      if List.exists (alpha_equal goal) goals then idT
      else tac (goal::goals))

let extract tactics =
   let tactics =
      if !debug_auto then List.map debugT tactics
      else List.map map_progressT tactics
   in
   let trivial = sort_nodes (List.filter trivialP tactics) in
   let normal = sort_nodes (List.filter normalP tactics) in
   let complete = sort_nodes (List.filter completeP tactics) in
   if !debug_auto then begin
      let names tacs = String.concat "; " (List.map (fun t -> t.auto_name) tacs) in
      eprintf "Auto tactics:\n\tTrivial: %s\n\tNormal: %s\n\tComplete: %s%t"
         (names trivial) (names normal) (names complete) eflush;
   end;
   let make_progress_first reset next =
      let rec prog_first tacs goals =
         match tacs with
            [] ->
               next goals
          | tac :: tacs ->
               (tac.auto_tac thenT (make_progressT goals prog_reset)) orelseT
                  (prog_first tacs goals)
      and prog_reset goals = prog_first reset goals
      in
         prog_first
   in
   let next_idT _ = idT in
   let gen_trivT next = make_progress_first trivial next trivial [] in
   let trivT = gen_trivT next_idT in
   let gen_normT next = gen_trivT (make_progress_first (trivial @ normal) next normal) in
   let all_tacs = trivial @ normal @ complete in
   let try_complete goals = tryT (completeT (make_progress_first all_tacs next_idT complete goals)) in
   let autoT = gen_normT try_complete in
   let strongAutoT = make_progress_first all_tacs next_idT all_tacs [] in
      (trivT, autoT, strongAutoT)

let improve_resource data info = info::data

(*
 * Resource.
 *)
let resource (auto_info, tactic * tactic * tactic) auto =
   Functional {
      fp_empty = [];
      fp_add = improve_resource;
      fp_retr = extract
   }

(*
 * Create a precedence.
 *)
let create_auto_prec before after =
   let node = ImpDag.insert dag () in
      List.iter (fun p -> ImpDag.add_edge dag p node) before;
      List.iter (fun p -> ImpDag.add_edge dag node p) after;
      node

(*
 * Use the tactic as long as progress is being made.
 *)
let rec check_progress goal = function
   goal' :: goals ->
      if alpha_equal goal goal' then
         true
      else
         check_progress goal goals
 | [] ->
      false

(*
 * Actual tactics.
 *)
let trivialT =
   funT (fun p -> let trivT, _, _ = get_resource_arg p get_auto_resource in trivT)

let autoT =
   funT (fun p -> let _, autoT, _ = get_resource_arg p get_auto_resource in autoT)

let strongAutoT =
   funT (fun p -> let _, _, sAutoT = get_resource_arg p get_auto_resource in sAutoT)

let tcaT = tryT (completeT strongAutoT)

let prefix_ttca tac =
   tac thenT tcaT

suffix ttca

let tcaT = tryT (completeT strongAutoT)
let tcwaT = tryT (completeT autoT)

let prefix_tatca tac =
   tac thenAT tcwaT

suffix tatca

let prefix_twtca tac =
   tac thenWT tcwaT

suffix twtca

let prefix_taa tac =
   tac thenAT autoT

suffix taa

let prefix_twa tac =
   tac thenWT autoT

suffix twa

let make_defT conv = rwhAllAll (conv thenC reduceC) (* BUG? : Should be reduceTopC ? *)
let byDefT conv = make_defT conv thenT autoT
let byDefsT convs = seqT (List.map make_defT convs) thenT autoT

let repeatWithRwsT convs tac  = repeatT (firstT (List.map (fun conv-> progressT (rwh conv 0) thenT tac ) convs))

(*
 * Trivial is in auto tactic.
 *)
let trivial_prec = create_auto_prec [] []
let nth_hyp_prec = create_auto_prec [trivial_prec] []

(*
 * Some trivial tactics.
 *)
let resource auto += {
   auto_name = "nthHypT/nthAssumT";
   auto_prec = nth_hyp_prec;
   auto_tac = someNthHypT orelseT onSomeAssumT nthAssumT;
   auto_type = AutoTrivial;
}

(*
 * Add autoT to the browser.
 *)
let resource menubar +=
    [<< menuitem["refine", "autoT", "Command('refine autoT')"] >>, refine_is_enabled]

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
