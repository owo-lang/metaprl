(*
 * Here are rules for the Void base type.
 * Void has no elements.  Its propositional
 * interpretation is "False".
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *
 *)

include Tacticals
include Itt_equal
include Itt_subtype

open Refiner.Refiner.Term
open Tacticals

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

declare void

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

(*
 * H >- Ui ext Void
 * by voidFormation
 *)
axiom voidFormation 'H : sequent ['ext] { 'H >- univ[@i:l] }

(*
 * H >- Void = Void in Ui ext Ax
 * by voidEquality
 *)
axiom voidEquality 'H : sequent ['ext] { 'H >- void = void in univ[@i:l] }

(*
 * Typehood.
 *)
axiom voidType 'H : sequent ['ext] { 'H >- "type"{void} }

(*
 * H; i:x:Void; J >- C
 * by voidElimination i
 *)
axiom voidElimination 'H 'J : sequent ['ext] { 'H; x: void; 'J['x] >- 'C['x] }

(*
 * Squash elimination.
 *)
axiom void_squashElimination 'H :
   sequent [squash] { 'H >- void } -->
   sequent ['ext] { 'H >- void }

(*
 * Subtyping.
 *)
axiom void_subtype 'H :
   sequent ['ext] { 'H >- subtype{void; 'T} }

(************************************************************************
 * TACTICS                                                              *
 ************************************************************************)

val d_voidT : int -> tactic
val eqcd_voidT : tactic

val void_term : term
val is_void_term : term -> bool

topval squash_voidT : tactic

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
