(*!
 * @spelling{conversional Mojave}
 *
 * @begin[doc]
 * @module[Mfir_sequent]
 *
 * The @tt[Mfir_sequent] module declares terms used in FIR theory sequents.
 * We take the following interpretation of sequents in the FIR theory.  If a
 * sequent is not well-formed, then it holds trivially.  In order for a
 * sequent to be well-formed, the list of hypotheses, also called the
 * @em{context}, must be well-formed.
 *
 * Contexts may contain declarations and definitions for variables, type
 * variables, and global labels (global values in FIR programs).  A variable
 * must be declared before it is defined, since variables may be defined in a
 * mutually recursive fashion (e.g.~functions).  No variable may be declared
 * or defined more than once.  Similar requirements hold for type variables
 * and global labels.
 * @end[doc]
 *
 * ------------------------------------------------------------------------
 *
 * @begin[license] This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming environment for OCaml
 * and other languages.  Additional information about the system is available
 * at http://www.metaprl.org/
 *
 * Copyright (C) 2002 Brian Emre Aydemir, Caltech
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc., 675
 * Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Brian Emre Aydemir @email{emre@cs.caltech.edu} @end[license]
 *)

(*!
 * @begin[doc]
 * @parents
 * @end[doc]
 *)

extends Mfir_int


(**************************************************************************
 * Declarations.
 **************************************************************************)

(*!
 * @begin[doc]
 * @terms
 * @modsubsection{Sequent tags}
 *
 * The term @tt[mfir] is used to tag FIR theory sequents.  The term @tt[hack]
 * is used to declare (explicitly) that some ``hacking'' is being done to make
 * things work out correctly. The term @tt[it] is used in rules to express
 * (the lack of) computational content of a proof.
 * @end[doc]
 *)

declare mfir
declare hack
declare it

(*!
 * @begin[doc]
 * @modsubsection{Kinds}
 *
 * Kinds are used to classify FIR types and type definitions.  Types may be
 * @em[small] or @em[large].  The raw integers and floating point values are
 * large; all other types are small.  The distinction between small and large
 * types is necessary to assist the garbage collector in the Mojave compiler.
 * Values of a small type can be tagged to distinguish them from pointers.
 * @end[doc]
 *)

declare small_type
declare large_type

(*!
 * @begin[doc]
 *
 * Union definitions (see @hrefterm[tyDefUnion]) belong to the @tt[union_type]
 * kind, where the parameter $i$ indiciates the number of cases in the union.
 * @end[doc]
 *)

declare union_type[i:n]

(*!
 * @begin[doc]
 *
 * All types, including parametrized types, belong to the @tt[polyKind] kind.
 * The parameter @tt[i] is the number of parameters in the definition, and
 * the subterm @tt[k] is the kind of the type once all the parameters are
 * instantiated.  We allow the case $i = 0$.  In practice, it assumed
 * that the ``simplest'' kind is used; this is made clear by the rules
 * in @hrefmodule[Mfir_tr_base].
 * @end[doc]
 *)

declare polyKind{ 'i; 'k }

(*!
 * @begin[doc]
 * @modsubsection{Contexts}
 *
 * The terms @tt[ty_def], @tt[var_def], and @tt[global_def] are used for
 * definitions in the context.  If the subterm @tt[def] is @tt[no_def], then
 * the definition is considered to be a declaration only.  A declaration is
 * well-formed if the first subterm is a well-formed kind/type, and the second
 * subterm is @tt[no_def].  A definition is well-formed if the corresponding
 * declaration is well-formed, and if the value/type in the definition has the
 * specified kind or type.
 * @end[doc]
 *)

declare ty_def{ 'k; 'def }
declare var_def{ 'ty; 'def }
declare global_def{ 'ty; 'def }
declare no_def

(*!
 * @begin[doc]
 * @modsubsection{Store values}
 *
 * Variables can be defined with an atom, or one of values below.  The term
 * @tt[polyFun] is a polymorphic function that takes one type argument.  The
 * term @tt[lambda] is a non-polymorphic function that takes one argument.
 * (Note that functions of multiple arguments are represented in curried
 * form.) The term @tt[union_val] is a value of case $i$ of some (polymorphic)
 * union type @tt[ty_var], initialized with the atoms in the list
 * @tt[atom_list].  The term @tt[raw_data] is an opaque representation of raw
 * data (see @hrefterm[tyRawData]).
 * @end[doc]
 *)

declare polyFun{ t. 'f['t] }
declare lambda{ v. 'f['v] }
declare union_val[i:n]{ 'ty_var; 'atom_list }
declare raw_data

(*!
 * @begin[doc]
 * @modsubsection{Judgments}
 *
 * The judgment @tt[wf_kind] says that kind @tt[k] is well-formed.
 * @end[doc]
 *)

declare wf_kind{ 'k }

(*!
 * @begin[doc]
 *
 * A proof of @tt[type_eq] says that two types (or type definitions)
 * @tt[ty1] and @tt[ty2] are equal in the kind @tt[k], and that @tt[k]
 * is well-formed.  A proof of @tt[type_eq_list] says that two lists of types
 * (or type definitions) are pointwise equal in the specified kind, and that
 * the kind is well-formed.
 * @end[doc]
 *)

declare type_eq{ 'ty1; 'ty2; 'k }
declare type_eq_list{ 'tyl1; 'tyl2; 'k }

(*!
 * @begin[doc]
 *
 * A proof of @tt[has_type] proves that a term @tt[t] has type @tt[ty],
 * and that type @tt[ty] is a well-formed.  The string parameter is an
 * annotation that is intended to describe some aspect of @tt[t] or
 * the typing relation.
 * @end[doc]
 *)

declare has_type[str:s]{ 't; 'ty }

(*!
 * @docoff
 *)


(**************************************************************************
 * Display forms.
 **************************************************************************)

(*
 * Sequent tags.
 *)

dform mfir_df : except_mode[src] ::
   mfir =
   it["mfir"]

dform hack_df : except_mode[src] ::
   hack =
   it["hack"]

dform it_df1 : except_mode[src] :: except_mode[tex] ::
   it =
   cdot

dform it_df2 : mode[tex] ::
   it =
   izone `"\\bullet" ezone

(*
 * Kinds.
 *)

dform small_type_df : except_mode[src] ::
   small_type =
   omega

dform large_type_df : except_mode[src] :: except_mode[tex] ::
   large_type  =
   `"(big " omega `")"

dform largeType_df : mode[tex] ::
   large_type  =
   izone `"\\Omega" ezone

dform union_type_df : except_mode[src] ::
   union_type[i:n] =
   bf["union"] `"[" slot[i:n] `"]"

dform polyKind_df : except_mode[src] ::
   polyKind{ 'i; 'k } =
   small_type sup{slot{'i}} rightarrow slot{'k}

(*
 * Store values.
 *)

dform polyFun_df1 : except_mode[src] :: except_mode[tex] ::
   polyFun{ t. 'f } =
   lambda uparrow slot{'t} `". " slot{'f}

dform polyFun_df2 : mode[tex] ::
   polyFun{ t. 'f } =
   izone `"\\Lambda " ezone slot{'t} `". " slot{'f}

dform lambda_df : except_mode[src] ::
   lambda{ v. 'f } =
   lambda slot{'v} `". " slot{'f}

dform union_val_df : except_mode[src] ::
   union_val[i:n]{ 'ty_var; 'atom_list } =
   slot{'ty_var} `"[" slot[i:n] `"](" slot{'atom_list} `")"

dform raw_data_df : except_mode[src] ::
   raw_data =
   bf["raw_data"]

(*
 * Contexts.
 *)

dform ty_def_df1 : except_mode[src] ::
   ty_def{ 'k; 'def } =
   slot{'k} `"=" slot{'def}

dform ty_def_df2 : except_mode[src] ::
   ty_def{ 'k; no_def } =
   slot{'k}

dform var_def_df1 : except_mode[src] ::
   var_def{ 'ty; 'def } =
   slot{'ty} `"=" slot{'def}

dform var_def_df2 : except_mode[src] ::
   var_def{ 'ty; no_def } =
   slot{'ty}

dform global_def_df1 : except_mode[src] ::
   global_def{ 'ty; 'def } =
   slot{'ty} `"=" slot{'def}

dform global_def_df2 : except_mode[src] ::
   global_def{ 'ty; no_def } =
   slot{'ty}

dform no_def_df1 : except_mode[src] :: except_mode[tex] ::
   no_def =
   cdot

dform no_def_df2 : mode[tex] ::
   no_def =
   izone `"\\bullet" ezone

(*
 * Judgments.
 *)

dform wf_kind_df : except_mode[src] ::
   wf_kind{ 'k } =
   bf["wf_kind"] `"(" slot{'k} `")"

dform type_eq_df : except_mode[src] ::
   type_eq{ 'ty1; 'ty2; 'k } =
   slot{'ty1} `"=" slot{'ty2} `":" slot{'k}

dform type_eq_list_df : except_mode[src] ::
   type_eq_list{ 'tyl1; 'tyl2; 'k } =
   slot{'tyl1} `"=" sub{it["list"]} slot{'tyl2} `":" slot{'k}

dform has_type_df : except_mode[src] ::
   has_type[str:s]{ 't; 'ty } =
   slot{'t} `":" slot{'ty} `" [" it[str:s] `"]"
