(*
 * Standard operations on terms.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

INCLUDE "refine_error.mlh"

open Term_sig
open Refine_error_sig
open Opname
open Term_std_sig
open Term_std

module TermOp
   (Term : TermStdSig with module TermTypes = TermType)
   (RefineError : RefineErrorSig with module Types = TermType) =
struct
   open RefineError
   open TermType
   module OpTypes = TermType

   (*
    * Helper functins for simple terms.
    *)
   let is_simple_bterm bterm =
      bterm.bvars = []

   let mk_simple_bterm t =
      { bvars = []; bterm = t }

   let dest_simple_bterm t = function
      { bvars = []; bterm = t } ->
         t
    | _ ->
         REF_RAISE(RefineError ("dest_simple_bterm", TermMatchError (t, "not a simple term")))

   let is_simple_bterms terms =
      List.for_all is_simple_bterm terms

   let mk_simple_bterms terms =
      List.map mk_simple_bterm terms

   let dest_simple_bterms t bterms =
      List.map (dest_simple_bterm t) bterms

   (*
    * Terms with no subterms.
    *)
   let is_no_subterms_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = []
      } ->
         Opname.eq opname' opname
    | _ ->
         false

   (*
    * Terms with one subterm
    *)
   let is_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; _ }]
      } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_term opname t =
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = []; bterm = t }]
      }

   let dest_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname' opname -> t
    | t -> REF_RAISE(RefineError ("dest_dep0_term", TermMatchError (t, "not a dep0 term")))

   let one_subterm = function
      ({ term_terms = [{ bvars = []; bterm = t }]; _ } : term) -> t
    | t -> REF_RAISE(RefineError ("one_subterm", TermMatchError (t, "not a single subterm")))

   let one_subterm_opname opname = function
      ({ term_op = { op_name = opname'; _ }; term_terms = [{ bvars = []; bterm = t }]} : term)
      when Opname.eq opname' opname -> t
    | t -> REF_RAISE(RefineError ("one_subterm", TermMatchError (t, "not a single subterm")))

   (*
    * Terms with two subterms.
    *)
   let is_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; _ }; { bvars = []; _ }]
      } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep0_term opname = fun
      t1 t2 ->
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [{ bvars = []; bterm = t1 };
                         { bvars = []; bterm = t2 }]
         }

   let dest_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = []; bterm = t2 }]
      } when Opname.eq opname' opname -> t1, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let two_subterms = function
      ({ term_terms = [{ bvars = []; bterm = a };
                       { bvars = []; bterm = b }];
         _
       } : term) -> a, b
    | t -> REF_RAISE(RefineError ("two_subterms", TermMatchError (t, "bad arity")))

   let two_subterms_opname opname  = function
      ({ term_op = { op_name = opname'; _ };
         term_terms = [{ bvars = []; bterm = a };
                       { bvars = []; bterm = b }]} : term)
      when Opname.eq opname' opname -> a, b
    | t -> REF_RAISE(RefineError ("two_subterms", TermMatchError (t, "bad arity")))

   (*
    * Terms with three subterms.
    *)
   let is_dep0_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; _ }; { bvars = []; _ }; { bvars = []; _ }]
      } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep0_dep0_term opname = fun
      t1 t2 t3  ->
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [{ bvars = []; bterm = t1 };
                         { bvars = []; bterm = t2 };
                         { bvars = []; bterm = t3 }]
         }

   let dest_dep0_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = []; bterm = t2 };
                      { bvars = []; bterm = t3 }]
      } when Opname.eq opname' opname -> t1, t2, t3
    | t -> REF_RAISE(RefineError ("dest_dep0_dep0_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * Terms with four subterms.
    *)
   let is_dep0_dep0_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; _ }; { bvars = []; _ }; { bvars = []; _ }; { bvars = []; _ }]
      } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep0_dep0_dep0_term opname = fun
      t1 t2 t3 t4 ->
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [{ bvars = []; bterm = t1 };
                         { bvars = []; bterm = t2 };
                         { bvars = []; bterm = t3 };
                         { bvars = []; bterm = t4 }]
         }

   let dest_dep0_dep0_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = []; bterm = t2 };
                      { bvars = []; bterm = t3 };
                      { bvars = []; bterm = t4 }]
      } when Opname.eq opname' opname -> t1, t2, t3, t4
    | t -> REF_RAISE(RefineError ("dest_dep0_dep0_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let is_two_subterm opname = function
      { term_op = { op_name = opname'; _ };
        term_terms = [{ bvars = []; _ }; { bvars = []; _ }]
      } -> Opname.eq opname' opname
    | _ -> false

   let is_three_subterm opname = function
      { term_op = { op_name = opname'; _ };
        term_terms = [{ bvars = []; _ }; { bvars = []; _ }; { bvars = []; _ }]
      } -> Opname.eq opname' opname
    | _ -> false

(* unused
   let is_four_subterm opname = function
      { term_op = { op_name = opname' };
        term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [] }; { bvars = [] }]
      } -> Opname.eq opname' opname
    | _ -> false
*)

   let is_five_subterm opname = function
      { term_op = { op_name = opname'; _ };
        term_terms = [{ bvars = []; _ }; { bvars = []; _ }; { bvars = []; _ }; { bvars = []; _ }; { bvars = []; _ }]
      } -> Opname.eq opname' opname
    | _ -> false

   let three_subterms = function
      { term_terms = [{ bvars = []; bterm = a };
                      { bvars = []; bterm = b };
                      { bvars = []; bterm = c }];
        _
      } ->
         a, b, c
    | t -> REF_RAISE(RefineError ("three_subterms", TermMatchError (t, "bad arity")))

   let four_subterms = function
      { term_terms = [{ bvars = []; bterm = a };
                      { bvars = []; bterm = b };
                      { bvars = []; bterm = c };
                      { bvars = []; bterm = d }];
        _
      } ->
         a, b, c, d
    | t ->
         REF_RAISE(RefineError ("four_subterms", TermMatchError (t, "bad arity")))

   let five_subterms = function
      { term_terms = [{ bvars = []; bterm = a };
                      { bvars = []; bterm = b };
                      { bvars = []; bterm = c };
                      { bvars = []; bterm = d };
                      { bvars = []; bterm = e }];
        _
      } ->
         a, b, c, d, e
    | t ->
         REF_RAISE(RefineError ("five_subterms", TermMatchError (t, "bad arity")))

   let six_subterms = function
      { term_terms = [{ bvars = []; bterm = a };
                      { bvars = []; bterm = b };
                      { bvars = []; bterm = c };
                      { bvars = []; bterm = d };
                      { bvars = []; bterm = e };
                      { bvars = []; bterm = f }];
        _
      } ->
         a, b, c, d, e, f
    | t ->
         REF_RAISE(RefineError ("six_subterms", TermMatchError (t, "bad arity")))

   (************************************************************************
    * Nonsimple but useful forms                                           *
    ************************************************************************)

   (*
    * One string param.
    *)
   let is_string_term opname = function
      { term_op = { op_name = opname'; op_params = [String _] };
        term_terms = []
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_string_term opname = function
      { term_op = { op_name = opname'; op_params = [String s] };
        term_terms = []
      } when Opname.eq opname opname' ->
         s
    | t ->
         REF_RAISE(RefineError ("dest_string_term", TermMatchError (t, "not a string term")))

   let dest_string_param = function
      { term_op = { op_params = String s :: _; _ }; _ } ->
         s
    | t ->
         REF_RAISE(RefineError ("dest_string_param", TermMatchError (t, "no string parameter")))

   let mk_string_term opname s =
      { term_op = { op_name = opname; op_params = [String s] }; term_terms = [] }

   (*
    * Two string params.
    *)
   let is_string_string_term opname = function
      { term_op = { op_name = opname'; op_params = [String _; String _] };
        term_terms = []
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_string_string_term opname = function
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = []
      } when Opname.eq opname opname' ->
         s1, s2
    | t ->
         REF_RAISE(RefineError ("dest_string_string_term", TermMatchError (t, "not a string-pair term")))

   let mk_string_string_term opname s1 s2 =
      { term_op = { op_name = opname; op_params = [String s1; String s2] }; term_terms = [] }

   (*
    * One variable param.
    *)
   let is_var_param_term opname = function
      { term_op = { op_name = opname'; op_params = [Var _] };
        term_terms = []
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_var_param_term opname = function
      { term_op = { op_name = opname'; op_params = [Var v] };
        term_terms = []
      } when Opname.eq opname opname' ->
         v
    | t ->
         REF_RAISE(RefineError ("dest_var_param_term", TermMatchError (t, "not a var param term")))

   let mk_var_param_term opname v =
      { term_op = { op_name = opname; op_params = [Var v] }; term_terms = [] }

   (*
    * One variable param, and two simple subterms.
    *)
   let is_var_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Var _] };
        term_terms = [{ bvars = []; _ }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_var_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Var v] };
        term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname opname' ->
         v, t
    | t ->
         REF_RAISE(RefineError ("dest_var_dep0_term", TermMatchError (t, "not a var param term")))

   let dest_var_dep0_any_term = function
      { term_op = { op_params = [Var v]; _ };
        term_terms = [{ bvars = []; bterm = t }]
      } ->
         v, t
    | t ->
         REF_RAISE(RefineError ("dest_var_dep0_term", TermMatchError (t, "not a var param term")))

   let mk_var_dep0_term opname v t =
      { term_op = { op_name = opname; op_params = [Var v] };
        term_terms = [{ bvars = []; bterm = t }]
      }

   let is_var_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Var _] };
        term_terms = [{ bvars = []; _ }; { bvars = []; _ }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_var_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Var v] };
        term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
      } when Opname.eq opname opname' ->
         v, t1, t2
    | t ->
         REF_RAISE(RefineError ("dest_var_dep0_dep0_term", TermMatchError (t, "not a var param term")))

   let mk_var_dep0_dep0_term opname v t1 t2 =
      { term_op = { op_name = opname; op_params = [Var v] };
        term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
      }

   (*
    * One string parameter, and one simple subterm.
    *)
   let is_string_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String _] };
        term_terms = [{ bvars = []; _ }]
      } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_string_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String s] };
        term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname opname' -> s, t
    | t -> REF_RAISE(RefineError ("dest_string_dep0_term", TermMatchError (t, "bad arity")))

   let mk_string_dep0_term opname = fun
      s t ->
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = [{ bvars = []; bterm = t }]
         }

   (*
    * Two string parameters, and one simple subterm.
    *)
   let is_string_string_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String _; String _] };
        term_terms = [{ bvars = []; _ }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_string_string_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname opname' ->
         s1, s2, t
    | t ->
         REF_RAISE(RefineError ("dest_string_string_dep0_term", TermMatchError (t, "bad arity")))

   let dest_string_string_dep0_any_term = function
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [{ bvars = []; bterm = t }]
      } ->
         s1, s2, t
    | t ->
         REF_RAISE(RefineError ("dest_string_string_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_string_string_dep0_term opname = fun
      s1 s2 t ->
         { term_op = { op_name = opname; op_params = [String s1; String s2] };
           term_terms = [{ bvars = []; bterm = t }]
         }

   (*
    * One number parameter and one subterm.
    *)
   let is_number_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number _] };
        term_terms = [{ bvars = []; _ }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_number_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number s1] };
        term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname opname' ->
         s1, t
    | t ->
         REF_RAISE(RefineError ("dest_number_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_dep0_any_term = function
      { term_op = { op_name = opname'; op_params = [Number s1] };
        term_terms = [{ bvars = []; bterm = t }]
      } ->
         s1, t
    | t ->
         REF_RAISE(RefineError ("dest_number_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_dep0_term opname = fun
      s1 t ->
         { term_op = { op_name = opname; op_params = [Number s1] };
           term_terms = [{ bvars = []; bterm = t }]
         }

   let is_number_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [Number _] };
        term_terms = [{ bvars = [_]; _ }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_number_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [Number s1] };
        term_terms = [{ bvars = [v]; bterm = t }]
      } when Opname.eq opname opname' ->
         s1, v, t
    | t ->
         REF_RAISE(RefineError ("dest_number_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_dep1_any_term = function
      { term_op = { op_name = opname'; op_params = [Number s1] };
        term_terms = [{ bvars = [v]; bterm = t }]
      } ->
         s1, v, t
    | t ->
         REF_RAISE(RefineError ("dest_number_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_dep1_term opname = fun
      s1 v t ->
         { term_op = { op_name = opname; op_params = [Number s1] };
           term_terms = [{ bvars = [v]; bterm = t }]
         }

   (*
    * Two number parameters and one subterm.
    *)
   let is_number_number_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number _; Number _] };
        term_terms = [{ bvars = []; _ }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_number_number_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2] };
        term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname opname' ->
         s1, s2, t
    | t ->
         REF_RAISE(RefineError ("dest_number_number_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_number_dep0_any_term = function
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2] };
        term_terms = [{ bvars = []; bterm = t }]
      } ->
         s1, s2, t
    | t ->
         REF_RAISE(RefineError ("dest_number_number_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_number_dep0_term opname = fun
      s1 s2 t ->
         { term_op = { op_name = opname; op_params = [Number s1; Number s2] };
           term_terms = [{ bvars = []; bterm = t }]
         }

   (*
    * Two number parameters, a string, and one subterm.
    *)
   let is_number_number_string_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number _; Number _; String _] };
        term_terms = [{ bvars = []; _ }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_number_number_string_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2; String s3] };
        term_terms = [{ bvars = []; bterm = t }]
      } when Opname.eq opname opname' ->
         s1, s2, s3, t
    | t ->
         REF_RAISE(RefineError ("dest_number_number_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_number_string_dep0_any_term = function
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2; String s3] };
        term_terms = [{ bvars = []; bterm = t }]
      } ->
         s1, s2, s3, t
    | t ->
         REF_RAISE(RefineError ("dest_number_number_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_number_string_dep0_term opname = fun
      s1 s2 s3 t ->
         { term_op = { op_name = opname; op_params = [Number s1; Number s2; String s3] };
           term_terms = [{ bvars = []; bterm = t }]
         }

   (*
    * Two number parameters, a string, and two subterms.
    *)
   let is_number_number_string_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number _; Number _; String _] };
        term_terms = [{ bvars = []; _ }; { bvars = []; _ }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_number_number_string_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2; String s3] };
        term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
      } when Opname.eq opname opname' ->
         s1, s2, s3, t1, t2
    | t ->
         REF_RAISE(RefineError ("dest_number_number_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_number_string_dep0_dep0_any_term = function
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2; String s3] };
        term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
      } ->
         s1, s2, s3, t1, t2
    | t ->
         REF_RAISE(RefineError ("dest_number_number_dep0_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_number_string_dep0_dep0_term opname = fun
      s1 s2 s3 t1 t2 ->
         { term_op = { op_name = opname; op_params = [Number s1; Number s2; String s3] };
           term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
         }

   (*
    * One string parameter, two subterms.
    *)
   let is_string_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String _] };
        term_terms = [{ bvars = []; _ }; { bvars = []; _ }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_string_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String s] };
        term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
      } when Opname.eq opname opname' ->
         s, t1, t2
    | t ->
         REF_RAISE(RefineError ("dest_string_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let dest_string_dep0_dep0_any_term = function
      { term_op = { op_name = opname'; op_params = [String s] };
        term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
      } ->
         s, t1, t2
    | t ->
         REF_RAISE(RefineError ("dest_string_dep0_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_string_dep0_dep0_term opname = fun
      s t1 t2 ->
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
         }

   let mk_string_dep0_dep0_dep0_term opname = fun
      s t1 t2 t3 ->
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }; { bvars = []; bterm = t3 }]
         }

   (*
    * Two string parameters, two subterms.
    *)
   let is_string_string_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String _; String _] };
        term_terms = [{ bvars = []; _ }; { bvars = []; _ }]
      } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_string_string_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
      } when Opname.eq opname opname' ->
         s1, s2, t1, t2
    | t ->
         REF_RAISE(RefineError ("dest_string_string_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let dest_string_string_dep0_dep0_any_term = function
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
      } ->
         s1, s2, t1, t2
    | t ->
         REF_RAISE(RefineError ("dest_string_string_dep0_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_string_string_dep0_dep0_term opname = fun
      s1 s2 t1 t2 ->
         { term_op = { op_name = opname; op_params = [String s1; String s2] };
           term_terms = [{ bvars = []; bterm = t1 }; { bvars = []; bterm = t2 }]
         }

   (*
    * One number param.
    *)
   let is_number_term opname = function
      { term_op = { op_name = opname'; op_params = [Number _] };
        term_terms = []
      } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_number_term opname = function
      { term_op = { op_name = opname'; op_params = [Number n] };
        term_terms = []
      } when Opname.eq opname opname' -> n
    | t -> REF_RAISE(RefineError ("dest_number_term", TermMatchError (t, "bad arity")))

   let dest_number_any_term = function
      { term_op = { op_params = [Number n]; _ };
        term_terms = []
      } ->
         n
    | t ->
         REF_RAISE(RefineError ("dest_number_any_term", TermMatchError (t, "bad arity")))

   let mk_number_term opname = function
      n ->
         { term_op = { op_name = opname; op_params = [Number n] };
           term_terms = []
         }

   (*
    * One universe param.
    *)
   let is_univ_term opname = function
      { term_op = { op_name = opname'; op_params = [MLevel _] };
        term_terms = []
      } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_univ_term opname = function
      { term_op = { op_name = opname'; op_params = [MLevel n] };
        term_terms = []
      } when Opname.eq opname opname' -> n
    | t -> REF_RAISE(RefineError ("dest_univ_term", TermMatchError (t, "bad arity")))

   let mk_univ_term opname = function
      n ->
         { term_op = { op_name = opname; op_params = [MLevel n] };
           term_terms = []
         }

   (*
    * One quote param.
    *)
   let is_quoted_term = function
      { term_op = { op_params = Quote::_; _ }; _ } -> true
    | _ -> false

   let unquote_term = function
      { term_op = { op_name = opname; op_params = Quote::params }; term_terms = bterms } ->
         { term_op = { op_name = opname; op_params = params }; term_terms = bterms }
    | t -> REF_RAISE(RefineError ("unquote_term", TermMatchError (t, "not a quoted term")))

   let quote_term = function
      { term_op = { op_name = opname; op_params = params }; term_terms = bterms } ->
         { term_op = { op_name = opname; op_params = Quote::params }; term_terms = bterms }

   (*
    * One token param.
    *)
   let is_token_term opname = function
      { term_op = { op_name = opname'; op_params = [Token _] };
        term_terms = []
      } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_token_term opname = function
      { term_op = { op_name = opname'; op_params = [Token n] };
        term_terms = []
      } when Opname.eq opname opname' -> n
    | t -> REF_RAISE(RefineError ("dest_token_term", TermMatchError (t, "bad arity")))

   let dest_token_param = function
      { term_op = { op_params = [Token n]; _ }; _ } -> n
    | t -> REF_RAISE(RefineError ("dest_token_param", TermMatchError (t, "bad arity")))

   let mk_token_term opname = function
      n ->
         { term_op = { op_name = opname; op_params = [Token n] };
           term_terms = []
         }

   (*
    * One token param, and no bound subterms.
    *)
   let is_token_simple_term opname = function
      { term_op = { op_name = opname'; op_params = [Token _] };
        term_terms = terms
      } when Opname.eq opname opname' ->
         is_simple_bterms terms
    | _ ->
         false

   let dest_token_simple_term opname = function
      { term_op = { op_name = opname'; op_params = [Token n] };
        term_terms = terms
      } as t when Opname.eq opname opname' ->
         n, dest_simple_bterms t terms
    | t ->
         REF_RAISE(RefineError ("dest_token_term", TermMatchError (t, "bad arity")))

   let mk_token_simple_term opname n terms =
      { term_op = { op_name = opname; op_params = [Token n] };
        term_terms = mk_simple_bterms terms
      }

   (*
    * Bound term.
    *)
   let is_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [_]; _ }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep1_term opname = fun
      v t -> { term_op = { op_name = opname; op_params = [] };
               term_terms = [{ bvars = [v]; bterm = t }]
             }

   let dest_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v]; bterm = t }]
      } when Opname.eq opname' opname -> v, t
    | t -> REF_RAISE(RefineError ("dest_dep1_term", TermMatchError (t, "bad arity")))

   let dest_dep1_any_term = function
      { term_op = { op_params = []; _ };
        term_terms = [{ bvars = [v]; bterm = t }]
      } -> v, t
    | t -> REF_RAISE(RefineError ("dest_dep1_any_term", TermMatchError (t, "bad arity")))

   let is_dep1_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [_]; _ }; { bvars = [_]; _ }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep1_dep1_term opname v1 t1 v2 t2 =
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = [v1]; bterm = t1 };
                      { bvars = [v2]; bterm = t2 }]
      }

   let dest_dep1_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v1]; bterm = t1 };
                      { bvars = [v2]; bterm = t2 }]
      } when Opname.eq opname' opname -> v1, t1, v2, t2
    | t -> REF_RAISE(RefineError ("dest_dep1_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [_; _]; _ }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep2_term opname = fun
      v1 v2 t -> { term_op = { op_name = opname; op_params = [] };
                   term_terms = [{ bvars = [v1;v2]; bterm = t }]
                 }

   let dest_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v1;v2]; bterm = t }]
      } when Opname.eq opname' opname -> v1, v2, t
    | t -> REF_RAISE(RefineError ("dest_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; _ }; { bvars = [_]; _ }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let is_dep0_dep1_any_term = function
      { term_op = { op_params = []; _ };
        term_terms = [{ bvars = []; _ }; { bvars = [_]; _ }]
      } -> true
    | _ -> false

   let mk_dep0_dep1_term opname = fun
      v t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                   term_terms = [{ bvars = []; bterm = t1 };
                                 { bvars = [v]; bterm = t2 }]
                 }

   let mk_dep0_dep1_any_term op = fun
      v t1 t2 -> { term_op = op;
                   term_terms = [{ bvars = []; bterm = t1 };
                                 { bvars = [v]; bterm = t2 }]
                 }

   let dest_dep0_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = [v]; bterm = t2 }]
      } when Opname.eq opname' opname -> v, t1, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep1_term", TermMatchError (t, "bad arity")))

   let dest_dep0_dep1_any_term = function
      { term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = [v]; bterm = t2 }];
        _
      } -> v, t1, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep1_any_term", TermMatchError (t, "bad arity")))

   let is_dep1_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [_]; _ }; { bvars = []; _ }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep1_dep0_term opname = fun
      v t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                   term_terms = [{ bvars = [v]; bterm = t1 };
                                 { bvars = []; bterm = t2 }]
                 }

   let dest_dep1_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v]; bterm = t1 };
                      { bvars = []; bterm = t2 }]
      } when Opname.eq opname' opname -> v, t1, t2
    | t -> REF_RAISE(RefineError ("dest_dep1_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * First subterm of arity 2.
    *)
   let is_dep2_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [_; _]; _ }; { bvars = []; _ }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep2_dep0_term opname = fun
      v1 v2 t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                   term_terms = [{ bvars = [v1; v2]; bterm = t1 };
                                 { bvars = []; bterm = t2 }]
                 }

   let dest_dep2_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v1; v2]; bterm = t1 };
                      { bvars = []; bterm = t2 }]
      } when Opname.eq opname' opname -> v1, v2, t1, t2
    | t -> REF_RAISE(RefineError ("dest_dep2_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * Second subterm of arity 2.
    *)
   let is_dep0_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; _ }; { bvars = [_; _]; _ }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep0_dep2_term opname = fun
      v1 v2 t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                   term_terms = [{ bvars = []; bterm = t1 };
                                 { bvars = [v1; v2]; bterm = t2 }]
                 }

   let dest_dep0_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = [v1; v2]; bterm = t2 }]
      } when Opname.eq opname' opname -> v1, v2, t1, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep2_term", TermMatchError (t, "bad arity")))

   (*
    * Second subterm of arity 3.
    *)
   let is_dep0_dep3_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; _ }; { bvars = [_; _; _]; _ }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep0_dep3_term opname = fun
      v1 v2 v3 t1 t2 -> { term_op = { op_name = opname; op_params = [] };
                          term_terms = [{ bvars = []; bterm = t1 };
                                        { bvars = [v1; v2; v3]; bterm = t2 }]
                        }

   let dest_dep0_dep3_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = [v1; v2; v3]; bterm = t2 }]
      } when Opname.eq opname' opname -> v1, v2, v3, t1, t2
    | t ->
         REF_RAISE(RefineError ("dest_dep0_dep3_term", TermMatchError (t, "bad arity")))

   (*
    * Four subterms.
    *)
   let is_dep0_dep2_dep0_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; _ }; { bvars = [_; _]; _ }; { bvars = []; _ }; { bvars = [_; _]; _ }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep0_dep2_dep0_dep2_term opname = fun
      t0 v11 v12 t1 base v21 v22 t2 -> { term_op = { op_name = opname; op_params = [] };
                                         term_terms = [{ bvars = []; bterm = t0 };
                                                       { bvars = [v11; v12]; bterm = t1 };
                                                       { bvars = []; bterm = base };
                                                       { bvars = [v21; v22]; bterm = t2 }]
                                       }

   let dest_dep0_dep2_dep0_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = [v11; v12]; bterm = t1 };
                      { bvars = []; bterm = base };
                      { bvars = [v21; v22]; bterm = t2 }]
      } when Opname.eq opname' opname -> t0, v11, v12, t1, base, v21, v22, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep2_dep0_dep2_term", TermMatchError (t, "bad arity")))

   let is_dep2_dep2_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [_; _]; _ }; { bvars = [_; _]; _ }; { bvars = []; _ }; { bvars = []; _ }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep2_dep2_dep0_dep0_term opname v11 v12 t1 v21 v22 t2 t3 t4 =
      { term_op = { op_name = opname; op_params = [] };
        term_terms = [{ bvars = [v11; v12]; bterm = t1 };
                      { bvars = [v21; v22]; bterm = t2 };
                      { bvars = []; bterm = t3 };
                      { bvars = []; bterm = t4 }]
      }

   let dest_dep2_dep2_dep0_dep0_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v11; v12]; bterm = t1 };
                      { bvars = [v21; v22]; bterm = t2 };
                      { bvars = []; bterm = t3 };
                      { bvars = []; bterm = t4 }];
      } when Opname.eq opname' opname -> v11, v12, t1, v21, v22, t2, t3, t4
    | t -> REF_RAISE(RefineError ("dest_dep2_dep2_dep0_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * Three subterms.
    *)
   let is_dep0_dep0_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; _ }; { bvars = []; _ }; { bvars = [_]; _ }]
      } when Opname.eq opname' opname ->
         true
    | _ ->
         false

   let mk_dep0_dep0_dep1_term opname = fun
      t0 t1 v2 t2 -> { term_op = { op_name = opname; op_params = [] };
                          term_terms = [{ bvars = []; bterm = t0 };
                                        { bvars = []; bterm = t1 };
                                        { bvars = [v2]; bterm = t2 }]
                        }

   let dest_dep0_dep0_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = []; bterm = t1 };
                      { bvars = [v2]; bterm = t2 }]
      } when Opname.eq opname' opname ->
         t0, t1, v2, t2
    | t ->
         REF_RAISE(RefineError ("dest_dep0_dep0_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep0_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; _ }; { bvars = []; _ }; { bvars = [_; _]; _ }]
      } when Opname.eq opname' opname ->
         true
    | _ ->
         false

   let mk_dep0_dep0_dep2_term opname = fun
      t0 t1 v21 v22 t2 -> 	{ term_op = { op_name = opname; op_params = [] };
									  term_terms = [{ bvars = []; bterm = t0 };
                                        { bvars = []; bterm = t1 };
                                        { bvars = [v21;v22]; bterm = t2 }]
									}

   let dest_dep0_dep0_dep2_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = []; bterm = t1 };
                      { bvars = [v21;v22]; bterm = t2 }]
      } when Opname.eq opname' opname ->
         t0, t1, v21, v22, t2
    | t ->
         REF_RAISE(RefineError ("dest_dep0_dep0_dep2_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep0_dep1_any_term = function
      { term_terms = [{ bvars = []; _ }; { bvars = []; _ }; { bvars = [_]; _ }]; _ } ->
         true
    | _ ->
         false

   let mk_dep0_dep0_dep1_any_term op = fun
      t0 t1 v2 t2 -> { term_op = op;
                       term_terms = [{ bvars = []; bterm = t0 };
                                     { bvars = []; bterm = t1 };
                                     { bvars = [v2]; bterm = t2 }]
                     }

   let dest_dep0_dep0_dep1_any_term = function
      { term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = []; bterm = t1 };
                      { bvars = [v2]; bterm = t2 }];
        _
      } ->
         t0, t1, v2, t2
    | t ->
         REF_RAISE(RefineError ("dest_dep0_dep0_dep1_any_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep1_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; _ }; { bvars = [_]; _ }; { bvars = [_]; _ }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep0_dep1_dep1_term opname = fun
      t0 v1 t1 v2 t2 -> { term_op = { op_name = opname; op_params = [] };
                          term_terms = [{ bvars = []; bterm = t0 };
                                        { bvars = [v1]; bterm = t1 };
                                        { bvars = [v2]; bterm = t2 }]
                        }

   let dest_dep0_dep1_dep1_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = [v1]; bterm = t1 };
                      { bvars = [v2]; bterm = t2 }]
      } when Opname.eq opname' opname -> t0, v1, t1, v2, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep1_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep0_dep3_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; _ }; { bvars = []; _ }; { bvars = [_; _; _]; _ }]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let mk_dep0_dep0_dep3_term opname = fun
      t0 t1 v1 v2 v3 t2 -> { term_op = { op_name = opname; op_params = [] };
                             term_terms = [{ bvars = []; bterm = t0 };
                                           { bvars = []; bterm = t1 };
                                           { bvars = [v1; v2; v3]; bterm = t2 }]
                           }

   let dest_dep0_dep0_dep3_term opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = []; bterm = t1 };
                      { bvars = [v1; v2; v3]; bterm = t2 }]
      } when Opname.eq opname' opname -> t0, t1, v1, v2, v3, t2
    | t -> REF_RAISE(RefineError ("dest_dep0_dep0_dep3_term", TermMatchError (t, "bad arity")))

   (*
    * One subterm with opname.
    *)
(* unused
   let is_one_bsubterm opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [_]
      } when Opname.eq opname' opname -> true
    | _ -> false

   let dest_one_bsubterm opname = function
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bterm]
      } when Opname.eq opname' opname -> bterm
    | t -> REF_RAISE(RefineError ("dest_one_bsubterm", TermMatchError (t, "bad arity")))

   let mk_one_bsubterm opname = fun
      bterm -> { term_op = { op_name = opname; op_params = [] }; term_terms = [bterm] }
*)

   (*
    * Iterate through the term.
    *)

   (*
    * XXX: NASTY HACK: Nogin: skip the "special" terms used by Term_man_gen to encode
    * sequents.
    *)
   let normal_term =
      let hyp_opname = mk_opname "hyp" xperv in
      let concl_opname = mk_opname "concl" xperv in
      fun t -> not (is_dep0_dep1_term hyp_opname t || is_dep0_term concl_opname t)

   let rec iter_down f t =
      if normal_term t then f t;
      List.iter (fun bterm -> iter_down f bterm.bterm) t.term_terms

   let rec iter_up f t =
      List.iter (fun bterm -> iter_up f bterm.bterm) t.term_terms;
      if normal_term t then f t

   (*
    * Sweep a function down through the term.
    *)
   let rec map_down f t =
      let { term_op = op; term_terms = bterms } = if normal_term t then f t else t in
      let apply { bvars = vars; bterm = t } =
         { bvars = vars; bterm = map_down f t }
      in
      let bterms =
         if (Opname.eq op.op_name var_opname or Opname.eq op.op_name context_opname) && bterms != [] then
            let bterms, last = Lm_list_util.split_last bterms in
               (List.map apply bterms) @ [last]
         else
            List.map apply bterms
      in
         { term_op = op; term_terms = bterms }

   let rec map_up f { term_op = op; term_terms = bterms } =
      let apply { bvars = vars; bterm = t } =
         { bvars = vars; bterm = map_up f t }
      in
      let bterms =
         if (Opname.eq op.op_name var_opname or Opname.eq op.op_name context_opname) && bterms != [] then
            let bterms, last = Lm_list_util.split_last bterms in
               (List.map apply bterms) @ [last]
         else
            List.map apply bterms
      in
      let t = { term_op = op; term_terms = bterms } in
         if normal_term t then f t else t
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
