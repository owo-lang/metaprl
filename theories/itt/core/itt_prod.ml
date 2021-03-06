doc <:doc<
   @module[Itt_prod]

   The product type $@prod{A; B}$ is @emph{derived} from the
   dependent production module @hrefmodule[Itt_dprod].  The
   non-dependent product $@prod{A; B}$ is equivalent to
   $@prod{x; A; B}$, where $x$ is not free in $B$.

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
>>
extends Itt_equal
extends Itt_dprod
extends Itt_struct
doc docoff

open Dtactic

open Itt_equal
open Itt_subtype
open Itt_dprod

doc <:doc<
   @rewrites
   The @tt{unfold_prod} rewrite unfolds the non-dependent
   product to a dependent-product with a @emph{new} variable
   $x$.
>>
prim_rw unfold_prod : ('A * 'B) <--> (x: 'A * 'B)

doc <:doc<
   @rules
   @modsubsection{Typehood and equality}

   The product space $@prod{A; B}$ is well-formed if
   both $A$ and $B$ are types.
>>
interactive independentProductEquality {| intro [] |} :
   [wf] sequent { <H> >- 'A1 = 'A2 in univ[i:l] } -->
   [wf] sequent { <H> >- 'B1 = 'B2 in univ[i:l] } -->
   sequent { <H> >- 'A1 * 'B1 = 'A2 * 'B2 in univ[i:l] }

(*
 * Typehood.
 *)
interactive independentProductType {| intro [] |} :
   [wf] sequent { <H> >- "type"{'A1} } -->
   [wf] sequent { <H> >- "type"{'A2} } -->
   sequent { <H> >- "type"{'A1 * 'A2} }

(*
 * H >- Ui ext A * B
 * by independentProductFormation
 * H >- Ui ext A
 * H >- Ui ext B
 *)
interactive independentProductFormation :
   ('A : sequent { <H> >- univ[i:l] }) -->
   ('B : sequent { <H> >- univ[i:l] }) -->
   sequent { <H> >- univ[i:l] }

doc <:doc<
   @modsubsection{Elimination}

   The elimination form splits the hypothesis $z@colon @prod{A; B}$ into
   its parts $u@colon A$ and $v@colon B$.
>>
interactive independentProductElimination {| elim[AutoOK] |} 'H :
   sequent { <H>; u: 'A; v: 'B; <J['u, 'v]> >- 'T['u, 'v] } -->
   sequent { <H>; z: 'A * 'B; <J['z]> >- 'T['z] }

interactive independentProductEqElimination {| elim [AutoOK] |} 'H :
   sequent { <H>; 'x1 = 'x2 in 'A; 'y1= 'y2 in 'B;  <J[it]> >- 'T[it] } -->
   sequent { <H>; u: ('x1,'y1) = ('x2,'y2) in 'A * 'B; <J['u]> >- 'T['u] }


doc <:doc<
   @modsubsection{Membership}

   The members of the non-dependent product $@prod{A; B}$
   are the pairs $@pair{a; b}$, where $a @in A$ and $b @in B$.
>>
interactive independentPairEquality {| intro [] |} :
   [wf] sequent { <H> >- 'a1 = 'a2 in 'A } -->
   [wf] sequent { <H> >- 'b1 = 'b2 in 'B } -->
   sequent { <H> >- ('a1, 'b1) = ('a2, 'b2) in 'A * 'B }

doc <:doc<
   @modsubsection{Introduction}

   The propositional interpretation of the
   non-dependent product space $@prod{A; B}$ is the
   conjunction $@and{A; B}$.  The proposition is
   true if both $A$ and $B$ are true.
>>
interactive independentPairFormation {| intro [] |} :
   [wf] ('a : sequent { <H> >- 'A }) -->
   [wf] ('b : sequent { <H> >- 'B }) -->
   sequent { <H> >- 'A * 'B }

doc <:doc<
   @modsubsection{Subtyping}

   The product space is covariant in both parts.
>>
interactive independentProductSubtype {| intro [] |} :
   ["subtype"] sequent { <H> >- 'A1 subtype 'A2 } -->
   ["subtype"] sequent { <H> >- 'B1 subtype 'B2 } -->
   sequent { <H> >- ('A1 * 'B1) subtype ('A2 * 'B2) }
doc docoff

(************************************************************************
 * TYPE INFERENCE                                                       *
 ************************************************************************)

let resource typeinf += (prod_term, infer_univ_dep0_dep0 dest_prod)

(************************************************************************
 * SUBTYPING                                                            *
 ************************************************************************)

(*
 * Subtyping of two product types.
 *)
let resource sub +=
   (DSubtype ([<< 'A1 * 'B1 >>, << 'A2 * 'B2 >>;
               << 'A1 >>, << 'A2 >>;
               << 'B1 >>, << 'B2 >>],
               independentProductSubtype))

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
