doc <:doc<
   @theory{@Nuprl-style Computational Type Theory}
   @module[Itt_theory]

   The @tt[Itt_theory] module collects all of the modules in our
   computational type theory.  This is the basic module to use when
   stating and proving theorems in the type theory.

   @docoff
   ----------------------------------------------------------------

   @begin[license]
   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/htmlman/default.html or visit http://metaprl.org/
   for more information.

   Copyright (C) 1998-2006 MetaPRL, Cornell University and California
   Institute of Technology

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
extends Base_theory
extends Itt_comment
extends Itt_labels
extends Itt_equal
extends Itt_struct
extends Itt_squash
extends Itt_squiggle
extends Itt_subtype
extends Itt_void
extends Itt_unit
extends Itt_atom
extends Itt_set
extends Itt_union
extends Itt_dprod
extends Itt_prod
extends Itt_dfun
extends Itt_esquash
extends Itt_logic
extends Itt_sqsimple
extends Itt_nequal
extends Itt_well_founded
extends Itt_decidable
extends Itt_bool
extends Itt_list
extends Itt_ext_equal
extends Itt_pairwise
extends Itt_struct2
extends Itt_struct3
extends Itt_match
extends Itt_int_base
extends Itt_int_ext
extends Itt_int_arith
extends Itt_omega
extends Itt_atom_bool
extends Itt_nat
extends Itt_isect
extends Itt_tsquash
extends Itt_disect
extends Itt_singleton
extends Itt_subset
extends Itt_image
extends Itt_tunion
extends Itt_bisect
extends Itt_bunion
extends Itt_inv_typing
extends Itt_w
extends Itt_prec
extends Itt_srec
extends Itt_quotient
extends Itt_list2
extends Itt_list_set
extends Itt_fun2
extends Itt_union2
extends Itt_prop_decide
extends Itt_subset2
extends Itt_pairwise2
extends Itt_record_label0
extends Itt_record_label
extends Itt_record0
extends Itt_record
extends Itt_record_exm
extends Itt_record_renaming

doc docoff

open Itt_equal
open Itt_dfun
open Itt_logic
open Itt_w
open Itt_list
open Itt_list2
open Itt_tunion
open Itt_bisect
open Itt_bunion
open Itt_atom_bool

(*
 * Combine the precedences.
 *)
prec prec_assoc < prec_equal
prec prec_equal < prec_apply
prec prec_type = prec_apply
prec prec_not < prec_apply
prec prec_w = prec_quant
prec prec_tree_ind < prec_quant
prec prec_list = prec_apply
prec prec_tunion = prec_apply
prec prec_bisect = prec_and
prec prec_bunion = prec_or
prec prec_eq_atom = prec_equal

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)

