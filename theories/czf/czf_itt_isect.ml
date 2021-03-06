doc <:doc<
   @module[Czf_itt_isect]

   The @tt[Czf_itt_isect] module gives defines a binary
   and general intersection.  The intersection is a @emph{derived} form,
   the binary intersection is defined with separation:

   $$@isect{s_1; s_2} @equiv @sep{x; s_1; @mem{x; s_2}}.$$

   The general intersection is defined over the union type.
   The elements in the intersection $@isect{s}$ are the elements
   of the union $@union{s}$ that are also members of all
   the elements of $s$.

   $$@isect{s} @equiv @sep{x; @union{s}; @dall{y; s; @mem{x; y}}}$$

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

doc <:doc< @parents >>
extends Czf_itt_union
doc docoff

open Lm_debug
open Lm_printf

open Dtactic

open Itt_logic

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

doc terms
declare "isect"{'s1; 's2}
declare "isect"{'s1}

(************************************************************************
 * REWRITES                                                             *
 ************************************************************************)

doc <:doc<
   @rewrites

   The intersections are derived from the separation
   set constructor in the @hrefmodule[Czf_itt_sep] module,
   and the union in the @hrefmodule[Czf_itt_union] module.
>>
prim_rw unfold_bisect : "isect"{'s1; 's2} <--> sep{'s1; x. mem{'x; 's2}}
prim_rw unfold_isect : "isect"{'s} <--> sep{union{'s}; x. dall{'s; y. mem{'x; 'y}}}
doc docoff

(************************************************************************
 * DISPLAY                                                              *
 ************************************************************************)

dform isect_df1 : parens :: "prec"[prec_and] :: "isect"{'s1; 's2} =
   slot{'s1} " " cap " " slot{'s2}

dform isect_df2 : parens :: "prec"[prec_and] :: "isect"{'s} =
   cap " " slot{'s}

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

doc <:doc<
   @rules
   @modsubsection{Well-formedness}

   Both forms of intersection are well-formed if their arguments are sets.
>>
interactive bisect_isset {| intro [] |} :
   ["wf"] sequent { <H> >- isset{'s1} } -->
   ["wf"] sequent { <H> >- isset{'s2} } -->
   sequent { <H> >- isset{."isect"{'s1; 's2}} }

interactive isect_isset {| intro [] |} :
   ["wf"] sequent { <H> >- isset{'s1} } -->
   sequent { <H> >- isset{."isect"{'s1}} }

doc <:doc<
   @modsubsection{Introduction}

   The binary intersection $@isect{s_1; s_2}$ requires membership
   in both sets $s_1$ and $s_2$.
>>
interactive bisect_member_intro {| intro [] |} :
   ["wf"] sequent { <H> >- isset{'x} } -->
   ["wf"] sequent { <H> >- isset{'s1} } -->
   ["wf"] sequent { <H> >- isset{'s2} } -->
   sequent { <H> >- mem{'x; 's1} } -->
   sequent { <H> >- mem{'x; 's2} } -->
   sequent { <H> >- mem{'x; ."isect"{'s1; 's2}} }

doc <:doc<
   A set $x$ is in the general intersection $@isect{s}$ if
   $@mem{x; y}$ for all $@mem{y; s}$.
>>
interactive isect_member_intro {| intro [] |} :
   ["wf"] sequent { <H> >- isset{'x} } -->
   ["wf"] sequent { <H> >- isset{'s} } -->
   sequent { <H> >- mem{'x; union{'s}} } -->
   sequent { <H>; y: set; w: mem{'y; 's} >- mem{'x; 'y} } -->
   sequent { <H> >- mem{'x; ."isect"{'s}} }

doc <:doc<
   @modsubsection{Elimination}

   The elimination form for membership in the binary intersection
   produces the proofs for membership in both types.
>>
interactive bisect_member_elim {| elim [] |} 'H :
   ["wf"] sequent { <H>; x: mem{'y; ."isect"{'s1; 's2}}; <J['x]> >- isset{'y} } -->
   ["wf"] sequent { <H>; x: mem{'y; ."isect"{'s1; 's2}}; <J['x]> >- isset{'s1} } -->
   ["wf"] sequent { <H>; x: mem{'y; ."isect"{'s1; 's2}}; <J['x]> >- isset{'s2} } -->
   sequent { <H>; x: mem{'y; ."isect"{'s1; 's2}}; <J['x]>; z: mem{'y; 's1} >- 'T['x] } -->
   sequent { <H>; x: mem{'y; ."isect"{'s1; 's2}}; <J['x]>; z: mem{'y; 's2} >- 'T['x] } -->
   sequent { <H>; x: mem{'y; ."isect"{'s1; 's2}}; <J['x]> >- 'T['x] }

doc <:doc<
   The elimination form for the general intersection $@mem{x; @isect{s}}$ performs
   instantiation of the assumption on a particular set $@mem{z; 's}$.
>>
interactive isect_member_elim {| elim [] |} 'H 'z :
   ["wf"] sequent { <H>; x: mem{'y; ."isect"{'s}}; <J['x]> >- isset{'z} } -->
   ["wf"] sequent { <H>; x: mem{'y; ."isect"{'s}}; <J['x]> >- isset{'y} } -->
   ["wf"] sequent { <H>; x: mem{'y; ."isect"{'s}}; <J['x]> >- isset{'s} } -->
   sequent { <H>; x: mem{'y; ."isect"{'s}}; <J['x]> >- mem{'z; 's} } -->
   sequent { <H>; x: mem{'y; ."isect"{'s}}; <J['x]>; w: mem{'y; 'z} >- 'T['x] } -->
   sequent { <H>; x: mem{'y; ."isect"{'s}}; <J['x]> >- 'T['x] }

doc <:doc<
   The intersection types are both functional in their arguments.
>>
interactive bisect_fun {| intro [] |} :
   sequent { <H> >- fun_set{z. 's1['z]} } -->
   sequent { <H> >- fun_set{z. 's2['z]} } -->
   sequent { <H> >- fun_set{z. "isect"{'s1['z]; 's2['z]}} }

interactive isect_fun {| intro [] |} :
   sequent { <H> >- fun_set{z. 's['z]} } -->
   sequent { <H> >- fun_set{z. "isect"{'s['z]}} }
doc docoff

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
