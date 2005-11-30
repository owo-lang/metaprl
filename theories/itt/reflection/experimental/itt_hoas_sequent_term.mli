doc <:doc<
   Native sequent representation.  This representation of sequents
   is not a BTerm itself.  If you want to work in a theory where
   sequents are not part of your language, then you should probably
   use this representation, because it is easier to use.

   ----------------------------------------------------------------

   @begin[license]
   Copyright (C) 2005 Mojave Group, Caltech

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

   @parents
>>
extends Meta_extensions_theory
extends Itt_hoas_sequent

open Basic_tactics

(*
 * Sequent well-formedness (no argument).
 *)
declare sequent [sequent_wf] { Term : Term >- Term } : Term

(*
 * BTerm sequents.
 *)
declare sequent [bsequent{'arg}] { Term : Term >- Term } : Term

(*
 * ML values.
 *)
topval fold_is_std_sequent : conv
topval fold_flatten_sequent : conv

(************************************************************************
 * Private values.
 *)

(* Type of standard sequents (hyp/concl terms) *)
declare StdSequent{'d}

(* Type of sequent pairs, without the argument *)
declare PreSequent{'d}

(* Conversion from a normal sequent to a standard sequent *)
declare sequent [std_sequent] { Term : Term >- Term } : Term

(* Full conversion to a sequent triple *)
declare sequent [pre_sequent] { Term : Term >- Term } : Term

(* The terms in a standard sequent *)
declare sconcl{'e}
declare shyp{'e1; x. 'e2['x]}

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
