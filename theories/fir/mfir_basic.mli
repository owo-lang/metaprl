(*
 * The Mfir_basic module declares basic terms needed to
 * support the MetaPRL representation of the FIR.
 *
 * ------------------------------------------------------------------------
 *
 * @begin[license]
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.  Additional
 * information about the system is available at
 * http://www.metaprl.org/
 *
 * Copyright (C) 2002 Brian Emre Aydemir, Caltech
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
 * Author: Brian Emre Aydemir
 * @email{emre@cs.caltech.edu}
 * @end[license]
 *)

extends Base_theory

open Tactic_type.Conversionals

(**************************************************************************
 * Declarations.
 **************************************************************************)

(*
 * Booleans.
 *)

declare "true"
declare "false"
declare "or"{ 'bool1; 'bool2 }
declare "and"{ 'bool1; 'bool2 }
declare "not"{ 'boolean }
declare ifthenelse{ 'test; 'true_case; 'false_case }

(*
 * Integers.
 *)

declare number[i:n]
declare numeral{ 'num }
declare add{ 'num1; 'num2 }
declare sub{ 'num1; 'num2 }
declare mul{ 'num1; 'num2 }
declare div{ 'num1; 'num2 }
declare rem{ 'num1; 'num2 }
declare minus{ 'num }

declare int_eq{ 'num1; 'num2 }
declare int_neq{ 'num1; 'num2 }
declare int_lt{ 'num1; 'num2 }
declare int_le{ 'num1; 'num2 }
declare int_gt{ 'num1; 'num2 }
declare int_ge{ 'num1; 'num2 }

(*
 * Lists.
 *)

declare nil
declare cons{ 'elt; 'tail }

(*
 * Integer sets.
 *)

declare interval{ 'left; 'right }
declare intset{ 'interval_list }
declare rawintset[precision:n, sign:s]{ 'interval_list }
declare member{ 'num; 'set }
declare intset_max

(**************************************************************************
 * Rewrites.
 **************************************************************************)

(*
 * Booleans.
 *)

val reduce_and : conv
val reduce_or : conv
val reduce_not : conv
val reduce_ifthenelse_true : conv
val reduce_ifthenelse_false : conv

(*
 * Integers.
 *)

val reduce_add : conv
val reduce_sub : conv
val reduce_mul : conv
val reduce_div : conv
val reduce_rem : conv
val reduce_minus : conv
val reduce_numeral : conv

val reduce_int_eq : conv
val reduce_int_neq : conv
val reduce_int_lt : conv
val reduce_int_le : conv
val reduce_int_gt : conv
val reduce_int_ge : conv

(*
 * Lists.
 *)

val reduce_member_interval : conv
val reduce_member_intset_ind : conv
val reduce_member_intset_base : conv
val reduce_member_rawintset_ind : conv
val reduce_member_rawintset_base : conv
