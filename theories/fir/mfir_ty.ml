(*!
 * @begin[doc]
 * @module[Mfir_ty]
 *
 * The @tt[Mfir_ty] module declares terms to represent the FIR type system.
 * @end[doc]
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

(*!
 * @begin[doc]
 * @parents
 * @end[doc]
 *)

extends Base_theory
extends Mfir_basic

open Top_conversionals

(**************************************************************************
 * Declarations.
 **************************************************************************)

(*
 * DROPPED: TyPointer.        Not worrying about this optimization.
 * TODO:    TyFrame.          I'm confused.
 * DROPPED: TyCase.           Part of the (unsound) FIR object system.
 * DROPPED: TyObject.         Part of the (unsound) FIR object system.
 * DROPPED: TyDelayed.        Not doing type inference.
 * TODO:    frame.            I'm confused.
 * TODO:    TyFun*            Need a type for functions of zero arguments.
 *)

(*!
 * @begin[doc]
 * @terms
 * @modsubsection{Numbers}
 *
 * The type @tt[tyInt] refers to signed, 31-bit integers.  The type
 * @tt{tyEnum[i:n]} includes the integers $@{0,@ldots,i-1@}$.  The raw integer
 * type @tt[tyRawInt] includes integers of varying bit precisions (8, 16, 32,
 * and 64) and signedness (``signed'' or ``unsigned'').  The type @tt[tyFloat]
 * includes floating-point values of a specified bit-precision (32, 64, or
 * 80).
 * @end[doc]
 *)

declare tyInt
declare tyEnum[i:n]
declare tyRawInt[precision:n, sign:s]
declare tyFloat[precision:n]

(*!
 * @begin[doc]
 * @modsubsection{Functions}
 *
 * The type @tt[tyFun] represents functions that take an argument of
 * type @tt[arg_type], and return a result of type @tt[res_type].  Strictly
 * speaking, all function calls in the FIR are tailcalls, so functions never
 * return.  They usually have a return type of @tt{tyEnum[0]}.  The @MetaPRL
 * representation, though, represents functions in curried form.  Thus,
 * @tt[res_type] maybe another @tt[tyFun] term.
 * @end[doc]
 *)

declare tyFun{ 'arg_type; 'res_type }

(*!
 * @begin[doc]
 * @modsubsection{Aggregate data}
 *
 * The type @tt[tyUnion] represents values in a polymorphic union type.  The
 * @tt[ty_var] subterm refers to a polymorphic union definition (see
 * @hrefterm[tyDefUnion]), which is instantiated at the types in @tt[ty_list].
 * The third subterm is an integer set that selects a subset of the union
 * cases.  Union cases are indexed starting at zero.
 * @end[doc]
 *)

declare tyUnion{ 'ty_var; 'ty_list; 'intset }

(*!
 * @begin[doc]
 *
 * The type @tt[tyTuple] represents tuples with arity $n$ if @tt[ty_list]
 * is a list of $n$ types. The parameter @tt[tc] is a tuple class, which
 * can either be ``normal'' or ``box''.  Box tuples must always have
 * arity one, and are used pass arbitrary values (such as floating-point
 * values or raw integers) as polymorphic values.
 * @end[doc]
 *)

declare tyTuple[tc:s]{ 'ty_list }

(*!
 * @begin[doc]
 *
 * Arrays are similar to tuples, except all the elements of an array have the
 * same type, and arrays may have arbitrary, non-negative dimension.
 * @end[doc]
 *)

declare tyArray{ 'ty }

(*!
 * @begin[doc]
 *
 * The unsafe type @tt[tyRawData] represents arbitrary data.  It is commonly
 * used to represent data aggregates in imperative programming languages, such
 * as C, that allow assignment of values to a data area without regard for the
 * type.
 * @end[doc]
 *) declare tyRawData

(*!
 * @begin[doc]
 * @modsubsection{Polymorphism}
 *
 * The term @tt[tyVar] represents a type variable.
 * @end[doc]
 *)

declare tyVar{ 'ty_var }

(*!
 * @begin[doc]
 *
 * The term @tt[tyApply] applies the types in the list @tt[ty_list] to a
 * parametrized type given by @tt[ty_var].  The application should be
 * complete; that is, the resulting type should not be a parametrized type.
 * @end[doc]
 *)

declare tyApply{ 'ty_var; 'ty_list }

(*!
 * @begin[doc]
 *
 * The existential type @tt[tyExists] defines a type @tt[ty] abstracted over a
 * type variable @tt[t].  The term @tt[tyAll] defines a polymorphic type,
 * where @tt[ty] is restricted to be either @tt[tyFun] or another @tt[tyAll].
 * This corresponds to value restriction @cite["ullman:sml"].
 * @end[doc]
 *)

declare tyExists{ t. 'ty['t] }
declare tyAll{ t. 'ty['t] }

(*!
 * @begin[doc]
 *
 * If @tt[var] is a ``packed'' existential value (see @hrefterm[atomTyPack]),
 * then the term @tt[tyProject] is the $i$th type in the packing, where
 * indexing starts at zero.
 * @end[doc]
 *)

declare tyProject[i:n]{ 'var }

(*!
 * @begin[doc]
 *
 * If @tt[poly_ty] is a parametrized type definition (see
 * @hrefterm[tyDefPoly]), then @tt[do_tyApply] instantiates the type
 * definition at the types in the list @tt[ty_list].
 * @end[doc]
 *)

declare do_tyApply{ 'poly_ty; 'ty_list }

(*!
 * @begin[doc]
 *
 * The term @tt[num_params] counts the number of parameters in an
 * existential or universal type @tt[ty].
 * @end[doc]
 *)

declare num_params{ 'ty }

(*!
 * @begin[doc]
 * @modsubsection{Type definitions}
 *
 * Type definitions defined parameterized types and unions.  The term
 * @tt[tyDefPoly] abstracts a type @tt[ty] over @tt[t].
 * @end[doc]
 *)

declare tyDefPoly{ t. 'ty['t] }

(*!
 * @begin[doc]
 *
 * The term @tt[tyDefUnion] is used to define a disjoint union.  The parameter
 * @tt[str] should either be ``normal'' or ``exn''.  Unions are tagged with
 * ``exn'' when they have more than 100 cases.  The subterm @tt[cases] should
 * be a list of @tt[unionCase] terms, and each @tt[unionCase] term should have
 * a list of @tt[unionCaseElt] terms.  A union case can be viewed as a tuple
 * space in which each field is tagged with a boolean indicating whether or
 * not it is mutable.
 * @end[doc]
 *)

declare unionCaseElt{ 'ty; 'boolean }
declare unionCase{ 'elts }
declare tyDefUnion[str:s]{ 'cases }

(*!
 * @begin[doc]
 *
 * The term @tt[nth_unionCase] returns the $n$th tuple space of a union
 * definition.
 * @end[doc]
 *)

declare nth_unionCase{ 'n; 'union_def }

(**************************************************************************
 * Rewrites.
 **************************************************************************)

(*!
 * @begin[doc]
 * @rewrites
 *
 * Instantiating a parameterized type at a given list of types is
 * straightforward.
 * @end[doc]
 *)

prim_rw reduce_do_tyApply_base :
   do_tyApply{ 'poly_ty; nil } <-->
   'poly_ty

prim_rw reduce_do_tyApply_ind :
   do_tyApply{ tyDefPoly{ t. 'ty['t] }; cons{ 'a; 'b } } <-->
   do_tyApply{ 'ty['a]; 'b }

(*!
 * @docoff
 *)

let resource reduce += [
   << do_tyApply{ 'poly_ty; nil } >>,
      reduce_do_tyApply_base;
   << do_tyApply{ tyDefPoly{ t. 'ty['t] }; cons{ 'a; 'b } } >>,
      reduce_do_tyApply_ind
]

(*!
 * @begin[doc]
 *
 * Counting the number of parameters in a type $<< tyExists{t. 'ty['t]} >>$
 * or $<< tyAll{ t. 'ty['t]} >>$ is also straightforward. Note the
 * bogus instantiation at $<< tyInt >>$ to address the problem of
 * free variables.
 * @end[doc]
 *)

prim_rw reduce_num_params_exists :
   num_params{ tyExists{ t. 'ty['t] } } <-->
   (1 +@ num_params{ 'ty[tyInt] })

prim_rw reduce_num_params_all :
   num_params{ tyAll{ t. 'ty['t] } } <-->
   (1 +@ num_params{ 'ty[tyInt] })

prim_rw reduce_num_params_any :
   num_params{ 'ty } <-->
   0

(*!
 * @docoff
 *)

let resource reduce += [
   << num_params{ 'ty } >>,
      (reduce_num_params_exists orelseC
       reduce_num_params_all orelseC
       reduce_num_params_any)
]

(*!
 * @begin[doc]
 *
 * Computing the $n$th tuple space of a union definition is straightforward,
 * given that the cases are given as a list.
 * @end[doc]
 *)

prim_rw reduce_nth_unionCase :
   nth_unionCase{ number[n:n]; tyDefUnion[str:s]{ 'cases } } <-->
   nth_elt{ number[n:n]; 'cases }

(*!
 * @docoff
 *)

let resource reduce += [
   << nth_unionCase{ number[i:n]; tyDefUnion[str:s]{ 'cases } } >>,
      reduce_nth_unionCase
]

(**************************************************************************
 * Display forms.
 **************************************************************************)

(*
 * Numbers.
 *)

dform tyInt_df : except_mode[src] ::
   tyInt =
   mathbbZ sub{31}

dform tyEnum_df : except_mode[src] ::
   tyEnum[i:n] =
   bf["enum"] sub{slot[i:n]}

dform tyRawInt_df1 : except_mode[src] ::
   tyRawInt[precision:n, sign:s] =
   mathbbZ sub{slot[precision:n]} sup{it[sign:s]}

dform tyRawInt_df2 : except_mode[src] ::
   tyRawInt[precision:n, "signed"] =
   mathbbZ sub{slot[precision:n]} sup{bf["signed"]}

dform tyRawInt_df3 : except_mode[src] ::
   tyRawInt[precision:n, "unsigned"] =
   mathbbZ sub{slot[precision:n]} sup{bf["unsigned"]}

dform tyFloat_df : except_mode[src] ::
   tyFloat[precision:n] =
   mathbbR sub{slot[precision:n]}

(*
 * Functions.
 *)

dform tyFun_df : except_mode[src] ::
   tyFun{ 'arg_type; 'res_type } =
   `"(" slot{'arg_type} rightarrow slot{'res_type} `")"

(*
 * Aggregate data.
 *)

dform tyUnion_df : except_mode[src] ::
   tyUnion{ 'ty_var; 'ty_list; 'intset } =
   bf["union"] `"(" slot{'ty_var} `"," slot{'ty_list} `"," slot{'intset} `")"

dform tyTuple_df : except_mode[src] ::
   tyTuple[tc:s]{ 'ty_list } =
   bf["tuple"] sub{bf[tc:s]} slot{'ty_list}

dform tyArray_df : except_mode[src] ::
   tyArray{ 'ty } =
   `"(" slot{'ty} `" " bf["array"] `")"

dform tyRawData_df : except_mode[src] ::
   tyRawData =
   bf["data"]

(*
 * Polymorphism.
 *)

dform tyVar_df : except_mode[src] ::
   tyVar{ 'ty_var } =
   bf["tvar"] `"(" slot{'ty_var} `")"

dform tyApply_df : except_mode[src] ::
   tyApply{ 'ty_var; 'ty_list } =
   slot{'ty_var} `"(" slot{'ty_list} `")"

dform tyExists_df : except_mode[src] ::
   tyExists{ t. 'ty } =
   exists slot{'t} `". " slot{'ty}

dform tyAll_df : except_mode[src] ::
   tyAll{ t. 'ty } =
   forall slot{'t} `". " slot{'ty}

dform tyProject_df : except_mode[src] ::
   tyProject[i:n]{ 'var } =
   slot{'var} `"." slot[i:n]

dform do_tyApply_df : except_mode[src] ::
   do_tyApply{ 'poly_ty; 'ty_list } =
   `"((" slot{'poly_ty} `") " slot{'ty_list} `")"

dform num_params_df : except_mode[src] ::
   num_params{ 'ty } =
   bf["num_params"] `"(" slot{'ty} `")"

(*
 * Type definitions.
 *)

dform tyDefPoly_df : except_mode[src] :: except_mode[tex] ::
   tyDefPoly{ t. 'ty } =
   lambda uparrow slot{'t} `". " slot{'ty}

dform tyDefPoly_df : mode[tex] ::
   tyDefPoly{ t. 'ty } =
   izone `"\\Lambda " ezone slot{'t} `". " slot{'ty}

dform unionCaseElt : except_mode[src] ::
   unionCaseElt{ 'ty; 'boolean } =
   `"(" slot{'ty} `"," slot{'boolean} `")"

dform unionCase_df : except_mode[src] ::
   unionCase{ 'elts } =
   `"+" slot{'elts}

dform tyDefUnion_df1 : except_mode[src] ::
   tyDefUnion[str:s]{ 'cases } =
   bf["union"] sub{it[str:s]} `"(" slot{'cases} `")"

dform tyDefUnion_df2 : except_mode[src] ::
   tyDefUnion["normal"]{ 'cases } =
   bf["union"] sub{bf["normal"]} `"(" slot{'cases} `")"

dform tyDefUnion_df3: except_mode[src] ::
   tyDefUnion["exn"]{ 'cases } =
   bf["exn"] sub{bf["normal"]} `"(" slot{'cases} `")"

dform nth_unionCase_df : except_mode[src] ::
   nth_unionCase{ 'i; 'union } =
   bf["nth"] `"(" slot{'i} `"," slot{'union} `")"
