(*
 * Fields with decidable equality.
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
 * Copyright (C) 1997-2004 MetaPRL Group
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

extends Itt_field2
extends Itt_intdomain_e

open Tactic_type.Tactic

(************************************************************************
 * SYNTAX                                                               *
 ************************************************************************)

declare prefieldE[i:l]
declare isFieldE{'f}
declare fieldE[i:l]

(************************************************************************
 * TACTICS                                                              *
 ************************************************************************)

topval unfold_prefieldE : conv
topval unfold_isFieldE : conv
topval unfold_fieldE : conv

topval fold_prefieldE1 : conv
topval fold_prefieldE : conv
topval fold_isFieldE1 : conv
topval fold_isFieldE : conv
topval fold_fieldE1 : conv
topval fold_fieldE : conv

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
