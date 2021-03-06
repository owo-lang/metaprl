doc <:doc<
   @module[Czf_itt_abel_group]

   The @tt[Czf_itt_abel_group] module defines abelian groups.
   A group is @emph{abelian} if its binary operation is
   commutative.


   @docoff
   ----------------------------------------------------------------

   @begin[license]
   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/htmlman/default.html or visit http://metaprl.org/
   for more information.

   Copyright (C) 2002 Xin Yu, Caltech

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

   Author: Xin Yu
   @email{xiny@cs.caltech.edu}
   @end[license]
>>

doc <:doc< @parents >>
extends Czf_itt_group
doc docoff

open Lm_debug
open Lm_printf

open Dtactic

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

doc terms
declare abel{'g}
doc docoff

(************************************************************************
 * REWRITES                                                             *
 ************************************************************************)

doc <:doc<
   @rewrites

   A group $g$ is abelian if its operation is commutative.
>>
prim_rw unfold_abel: abel{'g} <-->
   (group{'g} & (all a: set. all b: set. (mem{'a; car{'g}} => mem{'b; car{'g}} => eq{op{'g; 'a; 'b}; op{'g; 'b; 'a}})))
doc docoff

(************************************************************************
 * DISPLAY FORMS                                                        *
 ************************************************************************)

dform abel_df : except_mode[src] :: abel{'g} =
   `"abel(" slot{'g} `")"

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

doc <:doc<
   @rules
   @modsubsection{Typehood}

   The @tt[abel] judgment is well-formed if its
   argument is a label.
>>
interactive abel_type {| intro [] |} :
   sequent { <H> >- 'g IN label } -->
   sequent { <H> >- "type"{abel{'g}} }

doc <:doc<
   @modsubsection{Introduction}

   The proposition $@abel{g}$ is true if it is well-formed, $g$
   is a group, and @tt[op] is commutative.
>>
interactive abel_intro {| intro[] |} :
   sequent { <H> >- 'g IN label } -->
   sequent { <H> >- group{'g} } -->
   sequent { <H>; a: set; b: set; x: mem{'a; car{'g}}; y: mem{'b; car{'g}} >- eq{op{'g; 'a; 'b}; op{'g; 'b; 'a}} } -->
   sequent { <H> >- abel{'g} }
doc docoff

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
