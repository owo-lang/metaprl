doc <:doc< 
   @spelling{sall}
  
   @begin[doc]
   @module[Czf_itt_sall]
  
   The @tt{Czf_itt_sall} module defines the @emph{unrestricted} universal
   quantification $@sall{x; P[x]}$ over all sets $x$.  The proposition
   $P[x]$ must be well-formed for all sets $x$.
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
   @email{jyh@cs.cornell.edu}
   @end[license]
>>

doc <:doc< @doc{@parents} >>
extends Czf_itt_set
doc <:doc< @docoff >>

open Printf
open Lm_debug

open Refiner.Refiner.RefineError

open Tactic_type.Tacticals
open Tactic_type.Conversionals
open Tactic_type.Sequent
open Var

open Dtactic

open Itt_logic
open Itt_rfun

let _ =
   show_loading "Loading Czf_itt_sall%t"

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

doc <:doc< 
   @begin[doc]
   @terms
  
   The quantification $@sall{x; P[x]}$ is defined using the universal
   quantifier @hrefterm[all] from the @hrefmodule[Itt_logic] module.
   @end[doc]
>>
define unfold_sall : "sall"{x. 'A['x]} <--> (all x: set. 'A['x])
doc docoff

let fold_sall = makeFoldC << "sall"{x. 'A['x]} >> unfold_sall

(************************************************************************
 * DISPLAY FORMS                                                        *
 ************************************************************************)

dform sall_df : except_mode[src] :: parens :: "prec"[prec_lambda] :: "sall"{x. 'A} =
   math_sall{'x; 'A}

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

doc <:doc< 
   @begin[doc]
   @rules
   @modsubsection{Well-formedness}
  
   The quantification $@sall{x; A[x]}$ is well-formed if
   $A[x]$ is a proposition for any set $x$.
   @end[doc]
>>
interactive sall_type {| intro [] |} :
   sequent { <H>; y: set >- "type"{'A['y]} } -->
   sequent { <H> >- "type"{."sall"{x. 'A['x]} } }

doc <:doc< 
   @begin[doc]
   @modsubsection{Introduction}
  
   The quantification $@sall{x; A[x]}$ is true if it is well-formed,
   and if $A[x]$ is true for every set $x$.
   @end[doc]
>>
interactive sall_intro {| intro [] |} :
   sequent { <H>; a: set >- 'A['a] } -->
   sequent { <H> >- "sall"{x. 'A['x]} }

doc <:doc< 
   @begin[doc]
   @modsubsection{Elimination}
  
   The elimination form instantiates the universal assumption
   on a particular set argument $z$.
   @end[doc]
>>
interactive sall_elim {| elim [] |} 'H 'z :
   ["wf"]   sequent { <H>; x: "sall"{y. 'A['y]}; <J['x]> >- isset{'z} } -->
   ["main"] sequent { <H>; x: "sall"{y. 'A['y]}; <J['x]>; w: 'A['z] >- 'T['x] } -->
   sequent { <H>; x: "sall"{y. 'A['y]}; <J['x]> >- 'T['x] }
doc <:doc< @docoff >>

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
