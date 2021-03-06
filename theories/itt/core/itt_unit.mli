(*
 * Although unit is not strictly necessary,
 * we define it anyway, so we can use it before numbers
 * are defined.
 *
 * Type unit contains one element, it.
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
 * Copyright (C) 1997-2006 MetaPRL Group, Cornell University and
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)
extends Itt_squiggle

open Refiner.Refiner.TermType

declare const unit

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

(*
 * Squiggle equality.
 *)
rule unitSqequal :
   sequent { <H> >- 'x = 'y in unit } -->
   sequent { <H> >- 'x ~ 'y }

(*
 * H >- Ui ext Unit
 * by unitFormation
 *)
rule unitFormation : sequent { <H> >- univ[i:l] }

(*
 * H >- Unit = Unit in Ui ext Ax
 * by unitEquality
 *)
rule unitEquality : sequent { <H> >- unit in univ[i:l] }

(*
 * Is a type.
 *)
rule unitType : sequent { <H> >- "type"{unit} }

(*
 * H >- Ui ext Unit
 * by unitFormation
 *)
rule unit_memberFormation : sequent { <H> >- unit }

(*
 * H >- Unit = Unit in Ui ext Ax
 * by unitEquality
 *)
rule unit_memberEquality : sequent { <H> >- it in unit }

(*
 * H; i:x:Unit; J >- C
 * by unitElimination i
 * H; i:x:Unit; J[it / x] >- C[it / x]
 *)
rule unitElimination 'H :
   sequent{ <H>; <J[it]> >- 'C[it] } -->
   sequent { <H>; x: unit; <J['x]> >- 'C['x] }

(************************************************************************
 * TACTICS                                                              *
 ************************************************************************)

val unit_term : term
val is_unit_term : term -> bool

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
