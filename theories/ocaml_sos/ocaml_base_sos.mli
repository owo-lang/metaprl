(*
 * Basic definition for the operational semantics of
 * ocaml.  We define states as maps from "addresses", which
 * are just strings, to values.
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
extends Base_theory
extends Ocaml

(*
 * Extract term for equivalences.
 *)
declare it

(*
 * Term:
 * The address of a value.
 *)
declare address[name:s]

(*
 * Type:
 * A function belongs to type functional{'t1; 't2} if it
 * belongs to fun{'t1; 't2} and for any argument in 't1,
 * the application of the function does not modify the state.
 *)
declare functional{'t1 : TyOCaml; 't2 : TyOCaml} : TyOCaml

(*
 * Judgment:
 * Two expressions are equivalent if their evaluation from the same
 * state produces the same value and the same state.
 *    S: the state
 *    e1, e2: the expressions being compared
 *    t: the type of the comparison
 *)
declare equiv{'S; 'e1 : TyOCaml; 'e2 : TyOCaml; 't : TyOCaml}
declare member{'S; 'e : TyOCaml; 't : TyOCaml}

rewrite member_unfold :
   member{'S; 'e; 't} <--> equiv{'S; 'e; 'e; 't}

(*
 * Judgment:
 * Two expressions are functionally equivalent if they are equivalent
 * and they are both values.
 *)
declare value_equiv{'S; 'e1 : TyOCaml; 'e2 : TyOCaml; 't : TyOCaml}
declare value_member{'S; 'e : TyOCaml; 't : TyOCaml}

rewrite value_member_unfold :
   value_member{'S; 'e; 't} <--> value_equiv{'S; 'e; 'e; 't}

(*
 * Judgment:
 * Untyped value judgment.
 *)
declare is_value{'S; 'e : TyOCaml}

(*
 * Judgment:
 * t is a valid type.
 *)
declare is_type{'t : TyOCaml}

(*
 * Equivalence of names.
 *)
declare name_equiv{'S; 'n1; 'n2}

(*
 * Forms:
 * process: a run of a program in a particular state
 * value: is a process, but its evaluation does not modify the state
 * state: projects the state component of a process
 * expr: projects the program component
 * :expr_value: projects the program if it is a value
 *)
declare process{'S; 'e : TyOCaml}
declare "value"{'S; 'e : TyOCaml}
declare spread{'process; e, S. 'body['e; 'S]}
declare spread_value{'process; v, S. 'body['v; 'S]}
declare state{'S; 'e : TyOCaml}
declare expr{'S; 'e : TyOCaml}
declare expr_value{'S; 'e : TyOCaml}

(*
 * Operations on states.
 * lookup: get the value of name 'n in the state 'S
 * replace: replace the value of the name 'n in the state 'S with value 'v
 * allocate: allocate a new name in the state 'S with value 'v
 *)
declare lookup{'S; 'n}
declare replace{'S; 'n; 'v}
declare allocate{'S; 'v}

rewrite state_unfold :
   state{'S; 'e} <--> spread{process{'S; 'e}; v, S2. 'S2}

rewrite expr_unfold :
   expr{'S; 'e} <--> spread{process{'S; 'e}; v, S2. 'v}

(************************************************************************
 * BASIC FACTS                                                          *
 ************************************************************************)

(*
 * Two equivalent values are equivalent.
 *)
rule value_equiv_is_equiv :
   sequent { <H> >- value_equiv{'S; 'e1; 'e2; 't} } -->
   sequent { <H> >- equiv{'S; 'e1; 'e2; 't} }

(*
 * A functional function application to a value is a value.
 *)
rule functional_apply_value ('t1 :> TyOCaml) :
   sequent { <H> >- value_equiv{'S; 'a1; 'a2; 't2} } -->
   sequent { <H> >- value_equiv{'S; 'f1; 'f2; functional{'t1; 't2}} } -->
   sequent { <H> >- value_equiv{'S; apply{'f1; 'a1}; apply{'f2; 'a2}; 't2} }

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
