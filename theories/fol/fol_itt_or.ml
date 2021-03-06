(*
 * Derive the constant true.
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

extends Itt_theory
extends Fol_itt_type

derive Fol_or

open Basic_tactics

prim_rw unfold_or : Fol_or!"or"{'A; 'B} <--> Itt_union!union{'A; 'B}
prim_rw unfold_inl : Fol_or!inl{'a} <--> Itt_union!inl{'a}
prim_rw unfold_inr : Fol_or!inr{'a} <--> Itt_union!inr{'a}
prim_rw unfold_decide : Fol_or!decide{'a; x. 'b['x]; y. 'c['y]} <--> Itt_union!decide{'a; x. 'b['x]; y. 'c['y]}

let fold_or = makeFoldC << Fol_or!"or"{'A; 'B} >> unfold_or
let fold_inl = makeFoldC << Fol_or!"inl"{'a} >> unfold_inl
let fold_inr = makeFoldC << Fol_or!"inr"{'a} >> unfold_inr
let fold_decide = makeFoldC << Fol_or!"decide"{'a; x. 'b['x]; y. 'c['y]} >> unfold_decide

(************************************************************************
 * COMPUTATION                                                          *
 ************************************************************************)

derived_rw reduce_decide_inl : Fol_or!decide{.Fol_or!inl{'x}; y. 'body1['y]; z. 'body2['z]} <--> 'body1['x]

derived_rw reduce_decide_inr : Fol_or!decide{.Fol_or!inr{'x}; y. 'body1['y]; z. 'body2['z]} <--> 'body2['x]

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

derived or_type :
   [wf] sequent { <H> >- "type"{'A} } -->
   [wf] sequent { <H> >- "type"{'B} } -->
   sequent { <H> >- "type"{.Fol_or!"or"{'A; 'B}} }

derived or_intro_left :
   [wf] sequent { <H> >- "type"{'B} } -->
   [main] ('a : sequent { <H> >- 'A }) -->
   sequent { <H> >- Fol_or!"or"{'A; 'B} }

derived or_intro_right :
   [wf] sequent { <H> >- "type"{'A} } -->
   [main] ('b : sequent { <H> >- 'B } ) -->
   sequent { <H> >- Fol_or!"or"{'A; 'B} }

derived or_elim 'H :
   [wf] ('a['x] : sequent { <H>; x: 'A; <J[Fol_or!inl{'x}]> >- 'C[Fol_or!inl{'x}] }) -->
   [wf] ('b['x] : sequent { <H>; x: 'B; <J[Fol_or!inr{'x}]> >- 'C[Fol_or!inr{'x}] }) -->
   sequent { <H>; x: Fol_or!"or"{'A; 'B}; <J['x]> >- 'C['x] }

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
