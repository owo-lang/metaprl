(*
 * Function Intermediate Representation formalized in MetaPRL.
 *
 * Defines the types in the FIR.
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
 * Email:  emre@its.caltech.edu
 *)

include Base_theory

(*************************************************************************
 * Declarations.
 *************************************************************************)

(*
 * Integer and floating point precision.
 * The floating point precision terms are here for completeness.
 *    It'll be a while before we do any work with floating point numbers.
 *)
declare int8
declare int16
declare int32
declare int64
declare floatSingle
declare floatDouble
declare floatLongDouble

(* Integer type. *)
declare tyInt

(*
 * Enumeration type.
 * Represents a set of integers from 0 to ('num - 1).
 *)
declare tyEnum{ 'num }

(*
 * Native data types.
 * 'precision is the precision (number of bits).
 * 'sign should be val_true or val_false if this is, respectively,
 *    a signed or unsigned integral type.
 *)
declare tyRawInt{ 'precision; 'sign }
declare tyFloat{ 'precision }

(*
 *  Function type.
 * 'ty_list is a list of the types of the function's arguments.
 * 'ty is the type of the return value of the function.
 *)
declare tyFun{ 'ty_list; 'ty }

(*
 * Tuples.
 * 'ty_list in tyTuple is a list of the types in the tuple.
 * 'ty in tyArray is the type of the elements in the array.
 *)
declare tyUnion{ 'ty_var; 'ty_list; 'int_set }
declare tyTuple{ 'ty_list }
declare tyArray{ 'ty }
declare tyRawData

(* Polymorphism. *)
declare tyVar{ 'ty_var }
declare tyApply{ 'ty_var; 'ty_list }
declare tyExists{ 'ty_var_list; 'ty }
declare tyAll{ 'ty_var_list; 'ty }
declare tyProject{ 'ty_var; 'num }

(*
 * Delayed type.
 * Type should be inferred later.
 *)
declare tyDelayed

(*
 * Union tags.
 * normalUnion : all the fields are known and ordered.
 * exnUnion : not all the fields are known, nor are they ordered.
 *)
declare normalUnion
declare exnUnion

(* Defining types. *)
declare unionElt{ 'ty; 'bool }
declare tyDefUnion{ 'ty_var_list; 'union_ty; 'elts }
declare tyDefLambda{ 'ty_var_list; 'ty }
