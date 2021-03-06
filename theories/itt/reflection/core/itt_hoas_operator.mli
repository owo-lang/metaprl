doc <:doc<
   @module[Itt_hoas_operator]
   The @tt[Itt_hoas_operator] module defines a type << Operator >> of abstract
   operators; it also estabishes the connection between abstract operator type
   and the internal notion of syntax that is exposed by the computational bterms
   theory (@hrefmodule[Base_operator]).

   ----------------------------------------------------------------

   @begin[license]
   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/htmlman/default.html or visit http://metaprl.org/
   for more information.

   Copyright (C) 2005, MetaPRL Group

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

   Authors: Aleksey Nogin @email{nogin@cs.caltech.edu}
            Alexei Kopylov @email{kopylov@cs.caltech.edu}
            Xin Yu @email{xiny@cs.caltech.edu}

   @end[license]
>>

extends Itt_nat
extends Itt_list2

open Basic_tactics

declare const Operator
declare operator[op:op]
declare shape{'op}
declare is_same_op{'op_1;'op_2}

define iform unfold_arity : arity{'op} <--> length{shape{'op}}

topval opCaseT : term -> tactic

(************************************************************************
 * Term operations.
 *)
val is_operator_term : term -> bool
val dest_operator_term : term -> param op_param

(************************************************************************
 * Grammar.
 *)
declare iform parsed_operator{'t}

production xterm_term{parsed_operator{'t}} <--
   tok_dollar; xterm_term{'t}

iform unfold_parsed_operator :
   parsed_operator{'t}
   <-->
   operator[t:op]

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
