(*
 * Convert between camlp4 terms and NL terms.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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

open MLast

open Opname
open Refiner.Refiner.Term

(*
 * Location is a pair of bignums.
 *)
type loc = Mp_num.num * Mp_num.num

(*
 * A comment function takes a term,
 * and it location and type, and returns
 * another term.
 *)
type term_type =
   ExprTerm
 | PattTerm
 | TypeTerm
 | SigItemTerm
 | StrItemTerm
 | ModuleTypeTerm
 | ModuleExprTerm
 | ClassTerm
 | ClassFieldTerm
 | WithClauseTerm

type comment = term_type -> loc -> term -> term

(*
 * Terms to MLAst.
 * FormatError (reason, term that failed)
 *)
exception FormatError of string * term

val expr_of_term : term -> expr
val patt_of_term : term -> patt * term
val type_of_term : term -> ctyp
val sig_item_of_term : term -> sig_item
val str_item_of_term : term -> str_item
val module_type_of_term : term -> module_type
val module_expr_of_term : term -> module_expr
val class_type_infos_of_term : term -> class_type class_infos
val class_expr_infos_of_term : term -> class_expr class_infos
val class_type_of_term : term -> class_type
val class_expr_of_term : term -> class_expr
val class_field_of_term : term -> class_field
val class_type_field_of_term : term -> class_type_field

(*
 * MLast to term.
 *
 * The extra (loc -> term -> term) argument is used to annoate the results.
 * It is mapped over all terms postfix order.
 *)
val term_of_expr : string list -> comment -> expr -> term
val term_of_patt : string list -> comment -> patt -> (string list -> term) -> term
val term_of_type : comment -> ctyp -> term
val term_of_sig_item : comment -> sig_item -> term
val term_of_str_item : string list -> comment -> str_item -> term
val term_of_module_type : comment -> module_type -> term
val term_of_module_expr : string list -> comment -> module_expr -> term
val term_of_class_type_infos : comment -> class_type class_infos -> term
val term_of_class_expr_infos : string list -> comment -> class_expr class_infos -> term
val term_of_class_type : comment -> class_type -> term
val term_of_class_expr : string list -> comment -> class_expr -> term
val term_of_class_type_field : comment -> class_type_field -> term
val term_of_class_field : string list -> comment -> class_field -> term

(*
 * Specific values useful for writing
 * term interpreters.
 *)
val some_op : opname
val none_op : opname
val true_op : opname
val false_op : opname

(*
 * Common destructors.
 *)
val dest_loc : term -> int * int
val dest_loc_string : term -> (int * int) * string
val dest_loc_int : term -> (int * int) * string
val dest_opt : (term -> 'a) -> term -> 'a option
val dest_string : term -> string
val dest_string_opt : term -> string option
val dest_var : term -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
