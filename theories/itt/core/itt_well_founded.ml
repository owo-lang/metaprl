doc <:doc<
   @module[Itt_well_founded]

   The @tt{Itt_well_founded} module provides a more convenient
   description of well-foundness than the @hrefterm[well_founded_prop]
   term formalized in the @hrefmodule[Itt_rfun] module.  The definition
   of well-foundness requires the derivation of an induction
   principle.

   @docoff
   ----------------------------------------------------------------

   @begin[license]
   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/htmlman/default.html or visit http://metaprl.org/
   for more information.

   Copyright (C) 1999 Jason Hickey, Cornell University

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
>>
extends Itt_dfun
extends Itt_logic

open Dtactic

(************************************************************************
 * SYNTAX                                                               *
 ************************************************************************)

doc <:doc<
   @terms

   The @tt{partial_order} term specifies that $R$ is a partial
   order on type $A$.  The @tt{well_founded} term specifies that,
   in addition, $R$ is a well-founded order on $A$.

   The definition of @tt{partial_order} is that the order is
   anti-reflexive, anti-symmetric, and transitive.

   The definition of @tt{well_founded} requires that $R$ be a partial
   order, and that there is an @emph{induction} principle that can be
   used to prove any predicate $P$ on $A$.  This is different from the
   classical definition (that there are no infinite descending chains),
   but the induction principle implies that classical property.
>>
define unfold_partial_order : partial_order{'A; x, y. 'R['x; 'y]} <-->
   ((all x: 'A. "not"{'R['x; 'x]})
    & (all x: 'A. all y: 'A. ('R['x; 'y] => "not"{'R['y; 'x]}))
    & (all x: 'A. all y: 'A. all z: 'A. ('R['x; 'y] => ('R['y; 'z] => 'R['x; 'z]))))

define unfold_well_founded : well_founded[i:l]{'A; x, y. 'R['x; 'y]} <-->
   (partial_order{'A; x, y. 'R['x; 'y]}
    & (all P: ('A -> univ[i:l]).
       ((all x: 'A. ((all y: 'A. ('R['y; 'x] => ('P 'y))) => ('P 'x))) =>
        (all x: 'A. 'P 'x))))
doc docoff

(************************************************************************
 * DISPLAY                                                              *
 ************************************************************************)

dform partial_order_df : except_mode[src] :: partial_order{'A; x, y. 'R} =
   szone `"partial_order(" pushm[3] slot{'x} `"," slot{'y} `":" slot{'A} `"." hspace slot{'R} `")" popm ezone

dform well_founded_df : except_mode[src] :: well_founded[i:l]{'A; x, y. 'R} =
   szone `"well_founded[" slot[i:l] `"](" pushm[3] slot{'x} `"," slot{'y} `":" slot{'A} `"." hspace slot{'R} `")" popm ezone

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

doc <:doc<
   @rules
   @modsubsection{Well-formedness}

   The @tt{partial_order} and @tt{well_founded} predicates are
   both well-formed if their domain $A$ is a type, and their
   relation $R$ is a binary relation.
>>
interactive partial_order_type {| intro [] |} :
   [wf] sequent { <H> >- "type"{'A} } -->
   [wf] sequent { <H>; a: 'A; b: 'A >- "type"{'R['a; 'b]} } -->
   sequent { <H> >- "type"{partial_order{'A; x, y. 'R['x; 'y]}} }

interactive well_founded_type {| intro [] |} :
   [wf] sequent { <H> >- "type"{'A} } -->
   [wf] sequent { <H>; a: 'A; b: 'A >- "type"{'R['a; 'b]} } -->
   sequent { <H> >- "type"{well_founded[i:l]{'A; x, y. 'R['x; 'y]}} }

doc docoff

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
