(*
 * Minimal term module: Basic term operations.
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
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Author: Alexey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_symbol

(*
 * We use read-only arrays for sequents.
 *)
module type MinLinSet = sig
   type elt
   type t
   type index = int
   val length : t -> int
   val init : int -> (index -> elt) -> t
   val get : t -> index -> elt
   val of_list : elt list -> t
   val to_list : t -> elt list
end

module type TermBaseMinimalSig =
sig

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   module TermTypes : Term_sig.TermSig
   open TermTypes

   module SeqHyp : MinLinSet with type elt = hypothesis with type t = seq_hyps

   (************************************************************************
    * De/Constructors                                                      *
    ************************************************************************)

   (*
    * General interface.
    *)
   val make_term : term' -> term
   val dest_term : term -> term'
   val make_op : operator' -> operator
   val dest_op : operator -> operator'
   val make_bterm : bound_term' -> bound_term
   val dest_bterm : bound_term -> bound_term'
   val make_param : param' -> param
   val dest_param : param -> param'
   val dest_params : param list -> param' list
   val make_level : level_exp' -> level_exp
   val dest_level : level_exp -> level_exp'
   val make_level_var : level_exp_var' -> level_exp_var
   val dest_level_var : level_exp_var -> level_exp_var'

   val is_var_term : term -> bool
   val mk_var_term : var -> term
   val dest_var : term -> var

end

(*
 * We use read-only arrays for sequents.
 *)
module type TermBaseInternalSig =
sig
   include TermBaseMinimalSig
   open TermTypes

   (*
    * This function is not exported by the refiner,
    * and it may not even be implemented.
    *)
   val mk_descriptor_term : term Weak_memo.TheWeakMemo.descriptor -> term
   val dest_descriptor : term -> term Weak_memo.TheWeakMemo.descriptor option
end
