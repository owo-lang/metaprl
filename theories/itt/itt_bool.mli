(*
 * Boolean operations.
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
 *)

include Itt_equal
include Itt_logic
include Itt_struct

open Refiner.Refiner.Term
open Tacticals
open Conversionals

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

declare "bool"

declare "btrue"
declare "bfalse"
declare bor{'a; 'b}
declare band{'a; 'b}
declare bimplies{'a; 'b}
declare bnot{'a; 'b}

declare "assert"{'t}

declare ifthenelse{'e1; 'e2; 'e3}

prec prec_bimplies
prec prec_bor
prec prec_band
prec prec_bnot
prec prec_assert

(*
 * Definition of bool.
 *)
rewrite unfold_bool : bool <--> (unit + unit)
rewrite unfold_btrue : btrue <--> inl{it}
rewrite unfold_bfalse : bfalse <--> inr{it}

(*
 * Reduction.
 *)
rewrite reduce_ifthenelse_true : ifthenelse{btrue; 'e1; 'e2} <--> 'e1
rewrite reduce_ifthenelse_false : ifthenelse{bfalse; 'e1; 'e2} <--> 'e2
rewrite unfold_bor : bor{'a; 'b} <--> ifthenelse{'a; btrue; 'b}
rewrite unfold_band : band{'a; 'b} <--> ifthenelse{'a; 'b; bfalse}
rewrite unfold_bimplies : bimplies{'a; 'b} <--> ifthenelse{'a; 'b; btrue}
rewrite unfold_bnot : bnot{'a} <--> ifthenelse{'a; bfalse; btrue}
rewrite unfold_assert : "assert"{'t} <--> ('t = btrue in bool)

topval fold_bool : conv
topval fold_btrue : conv
topval fold_bfalse : conv
topval fold_bor : conv
topval fold_band : conv
topval fold_bimplies : conv
topval fold_bnot : conv
topval fold_assert : conv

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

(*
 * H >- Ui ext Unit
 * by boolFormation
 *)
rule boolFormation 'H : sequent ['ext] { 'H >- univ[@i:l] }

(*
 * H >- Bool = Bool in Ui ext Ax
 * by boolEquality
 *)
rule boolEquality 'H : sequent ['ext] { 'H >- "bool" = "bool" in univ[@i:l] }

(*
 * H >- Bool ext btrue
 * by bool_*Formation
 *)
rule bool_trueFormation 'H : sequent ['ext] { 'H >- "bool" }
rule bool_falseFormation 'H : sequent ['ext] { 'H >- "bool" }

(*
 * H >- Unit = Unit in Ui ext Ax
 * by boolEquality
 *)
rule bool_trueEquality 'H : sequent ['ext] { 'H >- btrue = btrue in "bool" }
rule bool_falseEquality 'H : sequent ['ext] { 'H >- bfalse = bfalse in "bool" }

(*
 * H; i:x:Unit; J >- C
 * by boolElimination i
 * H; i:x:Unit; J[it / x] >- C[it / x]
 *)
rule boolElimination2 'H 'J 'x :
   sequent['ext] { 'H; 'J[btrue] >- 'C[btrue] } -->
   sequent['ext] { 'H; 'J[bfalse] >- 'C[bfalse] } -->
   sequent ['ext] { 'H; x: "bool"; 'J['x] >- 'C['x] }

(*
 * Squash elimination on assert.
 *)
rule assertSquashElim 'H :
   sequent [squash] { 'H >- "assert"{'t} } -->
   sequent ['ext] { 'H >- "assert"{'t} }

(************************************************************************
 * TACTICS                                                              *
 ************************************************************************)

val is_assert_term : term -> bool
val mk_assert_term : term -> term
val dest_assert : term -> term

val d_boolT : int -> tactic
val eqcd_boolT : tactic
val eqcd_btrueT : tactic
val eqcd_bfalseT : tactic
val bool_term : term
val btrue_term : term
val bfalse_term : term

topval extBoolT : tactic
topval magicT : tactic
topval splitBoolT : term -> int -> tactic
topval splitITE : int -> tactic
topval squash_assertT : tactic

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
