doc <:doc< 
   @spelling{unitElimination}
  
   @begin[doc]
   @module[Itt_unit]
  
   The @tt{Itt_unit} module defines a term containing exactly
   one element, $@it$.  The element is the same term that inhabits
   the equality (Section @refmodule[Itt_equal]) and subtype
   (Section @refmodule[Itt_subtype]) judgments.
   @end[doc]
  
   ----------------------------------------------------------------
  
   @begin[license]
  
   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.
  
   See the file doc/index.html for information on Nuprl,
   OCaml, and more information about this system.
  
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
   @email{jyh@cs.caltech.edu}
  
   @end[license]
>>

doc <:doc< 
   @begin[doc]
   @parents
   @end[doc]
>>
extends Itt_equal
extends Itt_squash
extends Itt_struct
extends Itt_squiggle
doc <:doc< @docoff >>

open Printf
open Mp_debug
open Tactic_type.Sequent
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError
open Mp_resource

open Tactic_type
open Tactic_type.Tacticals

open Base_dtactic

open Itt_equal
open Itt_struct
open Itt_squiggle
open Itt_squash

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Itt_unit%t"

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

doc <:doc< @doc{@terms} >>
declare unit
doc <:doc< @docoff >>

(*
 * Standard term.
 *)
let unit_term = << unit >>
let unit_opname = opname_of_term unit_term
let is_unit_term = is_no_subterms_term unit_opname

(************************************************************************
 * DISPLAY FORMS                                                        *
 ************************************************************************)

dform unit_df1 : except_mode[src] :: unit = `"Unit"

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

doc <:doc< 
   @begin[doc]
   @rules
   @modsubsection{Typehood and equality}
  
   The $@unit$ type is a member of every universe, and it
   is also a type.
   @end[doc]
>>
prim unitEquality {| intro []; eqcd |} :
   sequent ['ext] { 'H >- unit in univ[i:l] } =
   it

(*
 * H >- Ui ext Unit
 * by unitFormation
 *)
interactive unitFormation :
   sequent ['ext] { 'H >- univ[i:l] }

(*
 * Is a type.
 *)
prim unitType {| intro [] |} :
   sequent ['ext] { 'H >- "type"{unit} } =
   it

doc <:doc< 
   @begin[doc]
   @modsubsection{Membership}
   The unique inhabitant of the $@unit$ type is the term $@it$.
   @end[doc]
>>
prim unit_memberEquality {| intro []; eqcd; squash |} :
   sequent ['ext] { 'H >- it in unit } =
   it

doc <:doc< 
   @begin[doc]
   @modsubsection{Introduction}
  
   The $@unit$ type is always provable.  The proof is the unique term
   $@it$.
   @end[doc]
>>
interactive unit_memberFormation {| intro [] |} :
   sequent ['ext] { 'H >- unit }

doc <:doc< 
   @begin[doc]
   @modsubsection{Elimination}
   The elimination rule @tt{unitElimination} performs a case analysis
   on $x@colon @unit$.  The witness is replaced with the term $@it$.
   @end[doc]
>>
prim unitElimination {| elim [ThinOption thinT] |} 'H :
   ('t : sequent['ext] { 'H; x: unit; 'J[it] >- 'C[it] }) -->
   sequent ['ext] { 'H; x: unit; 'J['x] >- 'C['x] } =
   't

doc <:doc< 
   @begin[doc]
   @modsubsection{Rewriting}
   Two terms in $@unit$ are always computationally equivalent.
   @end[doc]
>>
prim unitSqequal :
   sequent [squash] { 'H >- 'x = 'y in unit } -->
   sequent ['ext] { 'H >- 'x ~ 'y } = it
doc <:doc< @docoff >>

(************************************************************************
 * TYPE INFERENCE                                                       *
 ************************************************************************)

(*
 * Type of unit.
 *)
let resource typeinf += (unit_term, infer_univ1)

(*
 * Type of a unit object is unit.
 *)
let resource typeinf += (it_term, Typeinf.infer_const unit_term)

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
