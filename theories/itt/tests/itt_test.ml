(*
 * Display all the elements in a particular theory.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

extends Itt_theory

open Top_conversionals

open Itt_dfun
open Itt_bool
open Itt_int_base
open Itt_int_ext

declare fact{'i}
prim_rw reduceFact {| reduce |} : fact{'i} <--> ifthenelse{beq_int{'i; 0}; 1; .'i *@ fact{'i -@ 1}}

dform fact_df : except_mode[src] :: parens :: "prec"[prec_apply] :: fact{'i} =
   `"fact" " " slot{'i}

let redex1C =
   firstC [reduce_beta;
           reduce_eq_int;
           reduce_ifthenelse_true;
           reduce_ifthenelse_false;
           reduce_add;
           reduce_sub;
           reduce_mul;
           reduce_div]

let redex2C =
   reduceFact

let redexC = (repeatC (higherC redex1C) thenC (higherC redex2C))

interactive fact100 :
   sequent { <H> >- fact{100} }

interactive fact250 :
   sequent { <H> >- fact{250} }

interactive fact400 :
   sequent { <H> >- fact{400} }

interactive fact650 :
   sequent { <H> >- fact{650} }

let factT = rw (repeatC redexC) 0

interactive extraction_test :
   sequent { <H>; "type"{'A}; "type"{'B}; "type"{'C} >- (('A and 'B) => 'C) => ('A => ('B => 'C)) }

interactive foo:
   sequent { <H> >- lambda{x.'x +@ 0} }

declare sequent [boo] { Term : Term >- Term } : Term

prim bug175:
   sequent { <H>; <J> >- 'A } -->
   sequent { <H> >- sequent [boo] { <J> >- 'A }} = it

interactive_rw rw_table_test {| reduce |} :
   sequent { 'A; <H>; 'B >- 'C }
   <-->
   sequent { 'A; <H>; 'B >- 'C }

interactive_rw context_rw 'C:
   'C[[let v = 'e1<||> in 'e2['v]]] :> Term <--> (let v = 'e1 in 'C[['e2['v]]])

interactive context_rw_tests:
   sequent{ lambda{x.let v = 'x in ('v 'x)}; lambda{x.let v = 1 in ('v 'x)}; lambda{x.let v = 'x in ('v 1)}; lambda{x.let v = 1 in ('v 1)} >- it }

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
