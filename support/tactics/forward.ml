(*
 * Forward-chaining tactic.  This is very much like an elimination
 * tactic, but we have a check to ensure that progress is being made.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
extends Top_tacticals
extends Dtactic

open Lm_debug
open Lm_printf
open Lm_int_set

open Term_sig
open Rewrite_sig
open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermSubst
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError
open Term_match_table
open Term_hash_code
open Simple_print

open Tactic_type
open Tactic_type.Tactic
open Tactic_type.Tacticals

open Dtactic

let debug_forward =
   create_debug (**)
      { debug_name = "forward";
        debug_description = "display forward-chaining operations";
        debug_value = false
      }

(*
 * Build a term table from the sequent.
 * This is a term set indexed by hash code.
 *)
module type TermTableSig =
sig
   type t

   val empty : t
   val add : t -> term -> t
   val mem : t -> term -> bool
end;;

module TermTable =
struct
   type t

   let empty = IntMTable.empty

   let add table t =
      IntMTable.add table (hash_term t) t

   let mem table t =
      try
         let tl = IntMTable.find_all table (hash_term t) in
            List.exists (fun t' -> alpha_equal t' t) tl
      with
         Not_found ->
            false
end;;

(*
 * Build a table from the hyps.
 *)
let term_table_of_hyps hyps =
   SeqHyp.fold (fun table _ h ->
         match h with
            Hypothesis (_, t) ->
               TermTable.add table t
          | Context _ ->
               table) TermTable.empty hyps

(*
 * Merge the new hyps into the list.
 * Fails if there are no new hyps and the concl has not changed.
 *)
let progress_check orig_hyps orig_concl p =
   let { sequent_hyps = hyps;
         sequent_concl = concl
       } = explode_sequent (Sequent.goal p)
   in
   let hyps, changed =
      SeqHyp.fold (fun (new_hyps, changed) _ h ->
            match h with
               Hypothesis (_, t) ->
                  if TermTable.mem new_hyps t then
                     new_hyps, changed
                  else
                     TermTable.add new_hyps t, true
             | Context _ ->
                  new_hyps, changed) (orig_hyps, false) hyps
   in
   let changed = changed || not (alpha_equal concl orig_concl) in
      hyps, concl, changed

(*
 * Extract the elimination tactic from the table.
 *)
let extract_forward_data =
   let rec firstiT i = function
      [] ->
         raise (Invalid_argument "extract_elim_data: internal error")
    | [tac] ->
         tac i
    | tac :: tacs ->
         tac i orelseT firstiT i tacs
   in
   let step tbl =
      argfunT (fun i p ->
            let t = Sequent.nth_hyp p i in
            let () =
               if !debug_forward then
                  eprintf "forwardT: elim: lookup %s%t" (SimplePrint.short_string_of_term t) eflush
            in
            let tacs =
               try
                  lookup_bucket tbl select_all t
               with
                  Not_found ->
                     raise (RefineError ("extract_elim_data", StringTermError ("forwardT doesn't know about", t)))
            in
               firstiT i tacs)
   in
      step

(*
 * Get explicit arguments to the elimination rule.
 *)
let rec get_elim_args_arg = function
   ElimArgsOption (f, arg) :: t ->
      Some (f, arg)
 | _ :: t ->
      get_elim_args_arg t
 | [] ->
      None

let one_rw_arg i =
   { arg_ints = [| i |]; arg_addrs = [||] }

(*
 * Process a forward-chaining rule.
 *)
let process_forward_resource_annotation name args term_args statement (pre_tactic, options) =
   if args.spec_addrs <> [||] then
      raise (Invalid_argument (sprintf "elim annotation: %s: context arguments not supported yet" name));

   let assums, goal = unzip_mfunction statement in
      match SeqHyp.to_list (explode_sequent goal).sequent_hyps with
         [Context _; Hypothesis(v, t); Context _] ->
            (*
             * Define a function (term_args i p) that returns the actual
             * term arguments during rule application.
             *)
            let term_args =
               match term_args with
                  [] ->
                     (fun _ _ -> [])
                | _ ->
                     match get_elim_args_arg options with
                        Some (f, arg) ->
                           (* There are some explicit elimination arguments *)
                           let get_arg =
                              match arg with
                                 None ->
                                    (fun p i -> Sequent.nth_hyp p i)
                               | Some arg ->
                                    (match find_subterm t (fun t _ -> alpha_equal t arg) with
                                        addr :: _ ->
                                           (fun p i -> term_subterm (Sequent.nth_hyp p i) addr)
                                      | [] ->
                                           raise (RefineError("intro annotation", (**)
                                                                 StringTermError("term not found in the conclusion", arg))))
                           in
                              (fun i p -> f p (get_arg p i))
                      | None ->
                           (* Get the term arguments from the rule statement *)
                           let length = List.length term_args in
                              (fun _ p ->
                                    let args =
                                       try get_with_args p with
                                          RefineError _ ->
                                             raise (RefineError (name, StringIntError ("arguments required", length)))
                                    in
                                    let length' = List.length args in
                                       if length' != length then
                                          raise (RefineError (name, StringIntError ("wrong number of arguments", length')));
                                       args)
            in

            (*
             * Define the tactic for forward chaining.
             *)
            let tac =
               match args.spec_ints with
                  [| _ |] ->
                     argfunT (fun i p ->
                           if !debug_forward then
                              eprintf "forwardT elim: trying %s%t" name eflush;
                           Tactic_type.Tactic.tactic_of_rule pre_tactic (one_rw_arg i) (term_args i p))
                | _ ->
                     raise (Invalid_argument (sprintf "forwardT: %s: not an elimination rule" name))
            in
               [t, tac]
       | _ ->
            raise (Invalid_argument (sprintf "forwardT.improve_elim: %s: must be an elimination rule" name))

let resource (term * (int -> tactic), int -> tactic) forward =
   table_resource_info extract_forward_data

let forward_proof p =
   Sequent.get_resource_arg p get_forward_resource

(*
 * Forward-chain, then repeat on all hyps if something has
 * changed.
 *)
let rec step tac hyps concl depth bound i =
   if depth = bound then
      idT
   else
      funT (fun p ->
            tac i thenMT progress tac hyps concl (succ depth) bound)

and progress tac orig_hyps orig_concl depth bound =
   funT (fun p ->
         let new_hyps, new_concl, changed = progress_check orig_hyps orig_concl p in
            if changed then
               onAnyHypT (step tac new_hyps new_concl depth bound)
            else
               raise (RefineError ("Forward.progress", StringError "no progress")))

(*
 * Describe the original sequent.
 *)
let start_info p =
   let { sequent_hyps = hyps;
         sequent_concl = concl
       } = explode_sequent (Sequent.goal p)
   in
   let hyps = term_table_of_hyps hyps in
      hyps, concl

let forwardT i =
   funT (fun p ->
         let tac = forward_proof p in
         let hyps, concl = start_info p in
         let i = Sequent.get_pos_hyp_num p i in
            step tac hyps concl 0 1 i)

let forwardChainBoundT bound =
   funT (fun p ->
         let tac = forward_proof p in
         let hyps, concl = start_info p in
            onAnyHypT (step tac hyps concl 0 bound))

let forwardChainT =
   forwardChainBoundT max_int

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)