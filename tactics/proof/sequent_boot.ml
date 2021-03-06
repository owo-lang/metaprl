(*
 * Utilities for tactics.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2006 MetaPRL Group, Cornell University and
 * California Institute of Technology
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
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open Lm_debug

open Term_sig
open Refiner.Refiner
open Refiner.Refiner.TermType

open Tactic_boot

(*
 * Debug statement.
 *)
let _ =
   show_loading "Loading Sequent%t"

module Sequent =
struct
   module SequentTypes = TacticInternalType

   (*
    * Two tactic_arguments are equal when they have
    * equal msequent parts.  Their labels, etc, are
    * not compared.
    *)
   let tactic_arg_alpha_equal = TacticInternal.tactic_arg_alpha_equal
   let tactic_arg_alpha_equal_concl = TacticInternal.tactic_arg_alpha_equal_concl

   (*
    * Addressing.
    *)
   let goal = TacticInternal.goal
   let msequent = TacticInternal.msequent
   let concl = TacticInternal.concl
   let label = TacticInternal.label

   let args p =
      let { sequent_args = args; _ } = TermMan.explode_sequent (goal p) in
         TermMan.dest_xlist args

   let num_assums = TacticInternal.num_assums
   let nth_assum = TacticInternal.nth_assum
   let all_assums = TacticInternal.all_assums

   let get_pos_assum_num arg i =
      if i < 0 then
         num_assums arg + i + 1
      else
         i

   (*
    * Sequent parts.
    *)
   let hyp_count arg =
      TermMan.num_hyps (goal arg)

   let get_pos_hyp_num arg i =
      if i < 0 then
         (hyp_count arg) + i + 1
      else
         i

   let assum_hyp_count arg i =
      TermMan.num_hyps (nth_assum arg i)

   let nth_hyp p i = TacticInternal.nth_hyp p (get_pos_hyp_num p i)
   let nth_binding p i = TacticInternal.nth_binding p (get_pos_hyp_num p i)

   let get_decl_number arg v =
      TermMan.get_decl_number (goal arg) v

   let get_hyp_number arg t =
      TermMan.get_hyp_number (goal arg) t

   let explode_sequent_arg arg =
      TermMan.explode_sequent (goal arg)

   let rec all_hyps_aux hyps l i =
      if i = 0 then l else
      let i = pred i in
         match Term.SeqHyp.get hyps i with
            Hypothesis (_, t) ->
               all_hyps_aux hyps (t::l) i
          | Context _ ->
               all_hyps_aux hyps l i

   let all_hyps arg =
      let hyps = (explode_sequent_arg arg).sequent_hyps in
         all_hyps_aux hyps [] (Term.SeqHyp.length hyps)

   (*
    * Argument functions.
    *)
   let get_term_arg       = TacticInternal.get_term
   let get_term_list_arg  = TacticInternal.get_term_list
   let get_type_arg       = TacticInternal.get_type
   let get_int_arg        = TacticInternal.get_int
   let get_bool_arg       = TacticInternal.get_bool
   let get_string_arg     = TacticInternal.get_string
   let get_resource_arg   = TacticInternal.get_resource
   let mem_string_arg     = TacticInternal.mem_string
   let get_string_args    = TacticInternal.get_strings
   let get_option_args    = TacticInternal.get_options
   let set_option_args    = TacticInternal.set_options
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
