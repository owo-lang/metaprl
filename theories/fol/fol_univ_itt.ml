(*
 * Interpretation of universe is a decidable type in U1.
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
extends Fol_type_itt
extends Fol_univ

open Basic_tactics

(************************************************************************
 * SYNTAX                                                               *
 ************************************************************************)

declare ufalse{'t}
declare utrue{'t}

(************************************************************************
 * INTERPRETATION                                                       *
 ************************************************************************)

prim_rw unfold_univ : univ <--> (T: Itt_equal!univ[1:l] * "type"{'T})
prim_rw unfold_prop : prop{'t} <--> fst{'t}

prim_rw unfold_ufalse : ufalse{'t} <--> pair{'t; inl{it}}
prim_rw unfold_utrue : utrue{'t} <--> pair{'t; inr{it}}

interactive_rw reduce_prop_ufalse : prop{ufalse{'t}} <--> 't
interactive_rw reduce_prop_utrue : prop{utrue{'t}} <--> 't

(************************************************************************
 * DISPLAY                                                              *
 ************************************************************************)

dform ufalse_df : ufalse{'t} = `"ufalse(" slot{'t} `")"
dform utrue_df : utrue{'t} = `"utrue(" slot{'t} `")"

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

interactive univ_type 'H :
   sequent { <H>; x: univ; <J['x]> >- "type"{prop{'x}} }

interactive univ_type2 :
   sequent { <H> >- Itt_equal!"type"{univ} }

interactive ufalse_univ :
   sequent { <H> >- 't1 = 't2 in Itt_equal!"univ"[1:l] } -->
   sequent { <H>; x: 't1 >- Itt_logic!"false" } -->
   sequent { <H> >- ufalse{'t1} = ufalse{'t2} in univ }

interactive utrue_univ :
   sequent { <H> >- 't1 = 't2 in Itt_equal!"univ"[1:l] } -->
   sequent { <H> >- it = it in 't1 } -->
   sequent { <H> >- utrue{'t1} = utrue{'t2} in univ }

(************************************************************************
 * TACTICS                                                              *
 ************************************************************************)

let d_univ_typeT i p =
   if i = 0 then
      univ_type2 (Sequent.hyp_count_addr p) p
   else
      raise (RefineError ("d_unit_typeT", StringError "no elimination form"))

let univ_type_term = << Itt_equal!"type"{univ} >>

let resource d += (univ_type_term, d_univ_typeT)

(*
 * Equality goals.
 *)
let d_utrueT i p =
   if i = 0 then
      (utrue_univ (Sequent.hyp_count_addr p)
       thenT addHiddenLabelT "wf") p
   else
      raise (RefineError ("d_utrue", StringError "no elimination form"))

let utrue_term = << utrue{'t1} = utrue{'t2} in univ >>

let resource d += (utrue_term, d_utrueT)

let d_ufalseT i p =
   if i = 0 then
      (ufalse_univ (Sequent.hyp_count_addr p)
       thenT addHiddenLabelT "wf") p
   else
      raise (RefineError ("d_ufalse", StringError "no elimination form"))

let ufalse_term = << ufalse{'t1} = ufalse{'t2} in univ >>

let resource d += (ufalse_term, d_ufalseT)

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
