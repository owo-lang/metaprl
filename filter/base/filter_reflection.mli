(*
 * Managing reflected terms.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Opname
open Term_sig
open Term_ty_sig
open Term_shape_sig
open Refiner.Refiner.TermType
open Refiner.Refiner.TermShape
open Filter_shape
open Lm_symbol

(*
 * For expanding quotations.
 *)
type parse_state =
   { parse_quotation : string -> string -> term;
     parse_opname    : op_kind -> string list -> shape_param list -> int list -> Opname.opname;
     parse_shape     : shape -> shape_class;
     parse_param     : term -> param
   }

(*
 * Hooks.
 *)
val is_xquote_term    : term -> bool
val is_xquote0_term   : term -> bool

(*
 * Reflection processing.
 *)
val dest_xquote_term  : parse_state -> term -> term
val dest_xquote0_term : parse_state -> term -> term

(*
 * Constructing rules and theorems.
 *)
type parse_info

type var_info = var * var list * int

type socvars_info =
   { cvars_info  : var_info list;
     sovars_info : var_info list
   }

val create_parse_info : parse_state -> parse_info

(*
 * Term processing.
 *)
val quote_term        : parse_info -> ?depth: term -> term -> term

(*
 * Rule generation.
 *)
val mk_rule_term      : parse_info -> meta_term -> term
val mk_rule_wf_thm    : parse_info -> term -> meta_term
val mk_logic_wf_thm   : parse_info -> term -> meta_term
val mk_intro_thm      : parse_info -> term -> meta_term -> socvars_info * meta_term
val mk_type_check_thm : parse_info -> (term, term) poly_ty_term -> meta_term
val mk_mem_logic_thm  : parse_info -> term -> term -> meta_term
val mk_elim_thm       : parse_info -> term -> meta_term list -> var * meta_term

(*
 * Multi-part elimination.
 *)
val mk_elim_start_thm       : parse_info -> term -> var * meta_term
val mk_simple_step_elim_thm : parse_info -> term -> term list -> var * meta_term

(*
 * Various terms.
 *)
val mk_reflect_df1_term : parse_info -> term -> term
val mk_reflect_df2_term : parse_info -> term -> term -> term

val mk_empty_logic_term : parse_info -> term
val mk_rules_logic_term : parse_info -> term list -> term -> term
val mk_union_logic_term : parse_info -> term -> term -> term

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
