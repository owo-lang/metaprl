(*
 * Group-like Objects.
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
 * Author: Xin Yu
 * Email : xiny@cs.caltech.edu
 *)

extends Itt_record
extends Itt_set
extends Itt_dfun
extends Itt_disect

open Tactic_type.Tactic

(************************************************************************
 * SYNTAX                                                               *
 ************************************************************************)

declare groupoid[i:l]
declare isSemigroup{'g}
declare semigroup[i:l]
declare premonoid[i:l]
declare isMonoid{'g}
declare monoid[i:l]
declare isCommutative{'g}
declare csemigroup[i:l]
declare cmonoid[i:l]
declare subStructure{'s; 'g}

(************************************************************************
 * DISPLAY                                                              *
 ************************************************************************)

prec prec_mul

(************************************************************************
 * TACTICS                                                              *
 ************************************************************************)

topval unfold_isSemigroup : conv
topval unfold_semigroup : conv
topval unfold_premonoid : conv
topval unfold_isMonoid : conv
topval unfold_monoid : conv
topval unfold_subStructure : conv
topval unfold_isCommutative : conv

topval fold_groupoid : conv
topval fold_isSemigroup : conv
topval fold_semigroup1 : conv
topval fold_semigroup : conv
topval fold_premonoid1 : conv
topval fold_premonoid : conv
topval fold_isMonoid1 : conv
topval fold_isMonoid : conv
topval fold_monoid1 : conv
topval fold_monoid : conv
topval fold_isCommutative : conv
topval fold_csemigroup : conv
topval fold_cmonoid : conv
topval fold_subStructure : conv

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
