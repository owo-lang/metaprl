doc <:doc<
   @module[Czf_itt_axioms]

   The @tt[Czf_itt_axioms] defines the remaining axioms of
   the set theory as axioms.  This includes the set induction
   scheme, and the strong collection.

   @docoff
   ----------------------------------------------------------------

   @begin[license]
   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/htmlman/default.html or visit http://metaprl.org/
   for more information.

   Copyright (C) 1998 Jason Hickey, Cornell University

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Jason Hickey
   @email{jyh@cs.cornell.edu}
   @end[license]
>>

doc <:doc<
   @parents

   The @tt[Czf_itt_axiom] module includes the
   entire logical part of the theory, as well as the
   definition of the @hrefterm[rel] (for use in the definition
   of the strong collection theorem).
>>
extends Czf_itt_true
extends Czf_itt_false
extends Czf_itt_and
extends Czf_itt_or
extends Czf_itt_implies
extends Czf_itt_all
extends Czf_itt_exists
extends Czf_itt_sall
extends Czf_itt_sexists
extends Czf_itt_dall
extends Czf_itt_dexists
extends Czf_itt_rel
doc docoff

open Lm_debug
open Lm_printf

doc <:doc<
   @rules
   @modsubsection{Set induction}

   The set induction rule formalizes the induction scheme.  A goal
   $P[z]$ can be proven for a set $z$ if it can be proven for
   an arbitrary set $x$, given that it holds on all the elements.

   The proof of induction follows directly from $W$-type
   induction.
>>
interactive set_induction :
   sequent { <H>; x: set >- "type"{'P['x]} } -->
   sequent { <H> >- fun_prop{z. 'P['z]} } -->
   sequent { <H>; x: set; w: dall{'x; z. 'P['z]} >- 'P['x] } -->
   sequent { <H> >- sall{z. 'P['z]} }
doc docoff

let setInduction1 = set_induction

interactive set_induction2 'H :
   sequent { <H>; x: set; <J['x]>; y: set >- "type"{'C['y]} } -->
   sequent { <H>; x: set; <J['x]> >- fun_prop{y. 'C['y]} } -->
   sequent { <H>; x: set; <J['x]>; y: set; z: dall{'y; w. 'C['w]} >- 'C['y] }-->
   sequent { <H>; x: set; <J['x]> >- 'C['x] }

let setInduction = set_induction2

doc <:doc<
   @modsubsection{Strong Collection}

   The strong collection axiom states that for every proof
   of a @misspelled{forall}/exists formula $@dall{x; s_1; @sexists{s_2; P[x; y]}}$,
   there is a set $s_3$ that contains the collection of sets that
   were chosen by the existential.  The reason that the collection is
   not defined as a set constructor is that the proof of the @misspelled{forall}/exists
   formula is part of the construction.  If the set $s_1$ has canonical
   for $s_1 = @collect{x; T; f[x]}$, the proof provides a witness
   that inhabits the function space $T @rightarrow @set$.  The canonical
   form of the proof is a @tt[lambda]-function $@lambda{x; s_x}$,
   which can be used to form the set collection $@collect{x; T; s_x}$.
>>
interactive collection 's1 (bind{x. bind{y. 'P['x; 'y]}}) :
   sequent { <H> >- isset{'s1} } -->
   sequent { <H>; x: set; y: set >- "type"{'P['x; 'y]} } -->
   sequent { <H> >- dall{'s1; x. sexists{y. 'P['x; 'y]}} } -->
   sequent { <H>; s2: set; w: rel{x, y. 'P['x; 'y]; 's1; 's2} >- 'C } -->
   sequent { <H> >- 'C }

doc <:doc<
   @modsubsection{Subset collection}

   The @hrefmodule[Czf_itt_power] module defines the subset collection
   set constructor $@power{s_1; s_2}$.  For completeness, we reprove the
   axiom form of the subset collection.
>>
interactive subset_collection 'a 'b bind{u. bind{x. bind{y. 'P['u; 'x; 'y]}}} :
   sequent { <H> >- isset{'a} } -->
   sequent { <H> >- isset{'b} } -->
   sequent { <H>; u: set; x: set; y: set >- "type"{'P['u; 'x; 'y]} } -->
   sequent { <H>; u: set; x: set >- fun_prop{y. 'P['u; 'x; 'y]} } -->
   sequent { <H>; w: sexists{c. sall{u. dall{'a; x. dexists{'b; y. 'P['u; 'x; 'y]}} => dexists{'c; z. rel{x, y. 'P['u; 'x; 'y]; 'a; 'z}}}} >- 'C } -->
   sequent { <H> >- 'C }
doc docoff

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
