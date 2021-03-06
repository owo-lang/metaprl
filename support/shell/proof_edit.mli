(*
 * The proof editor constructs a proof interactively.
 * We provide a notion of a "current" address into the
 * proof, which is the point in the proof that is displayed
 * on the screen.
 *
 * At the base level, this data structure just adds undo capability
 * to proofs, and in doing so, the operations become imperative.
 *
 * Also add display capability.
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
 * Copyright (C) 1998-2004 MetaPRL Group
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
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

open Opname
open Refiner.Refiner.TermType
open Refiner.Refiner

open Tactic_type
open Tactic_type.Tactic
open Shell_sig

(*
 * The is the state of the current proof.
 *)
type ped

(*
 * Constructors.
 *)
val create : tactic_arg -> ped
val ped_of_proof : Proof.proof -> ped
val set_goal : ped -> Refine.msequent -> unit
val initialize_goal : ped -> Refine.msequent -> (tactic_arg -> tactic_arg) -> unit

val edit_info_of_ped : ped -> Proof.address -> edit_info

(*
 * Destructors.
 *)
val proof_of_ped : ped -> Proof.proof
val status_of_ped : ped -> Proof.address -> Proof.status
val node_count_of_ped : ped -> int * int

(*
 * Refinement, and undo lists.
 * A finite number of undo's are allowed.
 * The (string, Ast.expr, tactic) are all different
 * representations of the same thing.
 *
 * After a refine_ped or nop_ped, the undo stack gets reset.
 * The nop_ped does nothing but reset the undo stack.
 *)
val undo_ped : ped -> Proof.address -> Proof.address
val redo_ped : ped -> Proof.address -> Proof.address

(*
 * Test whether some methods are enabled.
 *)
val is_enabled_ped : ped -> Proof.address -> method_name -> bool

(*
 * Navigation.
 *)
val check_addr_ped : ped -> Proof.address -> unit

(*
 * Get the status of the proof.
 *)
val ped_status : ped Filter_summary_type.proof_type -> obj_status

(************************************************************************
 * Display.
 *)
type incomplete_ped =
   Primitive of tactic_arg
 | Incomplete of tactic_arg
 | Derived of tactic_arg * MLast.expr

val interpret : display_fun -> ped -> Proof.address -> proof_command -> was_modified

(*
 * Check the proof and return its extract.
 * Two versions for handling refinement errors:
 *    check_proof: expand until first error, exceptions propagate
 *       On failure, the ped is modified to point to the error
 *    expand_proof: check as much of the proof as possible,
 *       no exceptions are raised
 *)
val check_ped              : display_fun -> Refine.refiner -> opname -> ped -> was_modified * ref_status
val refiner_extract_of_ped : display_fun -> ped -> was_modified * Refine.extract

(*
 * Display utilities
 *)
val display_term : display_method -> term -> unit
val display_term_newline : display_method -> term -> unit

(*
 * Display the goals.
 *)
val format_incomplete : display_fun -> incomplete_ped -> unit
val format : display_fun -> ped -> Proof.address -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
