(*
 * Standard operations on terms.
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Authors: Alexey Nogin
 *)

INCLUDE "refine_error.mlh"

open Refine_error_sig
open Term_ds_sig

open String_set
open Term_ds

module TermOp
   (Term : TermDsSig
    with type level_exp_var = TermType.level_exp_var
    with type level_exp = TermType.level_exp
    with type param = TermType.param
    with type operator = TermType.operator
    with type term = TermType.term
    with type term_core = TermType.term_core
    with type bound_term = TermType.bound_term

    with type level_exp_var' = TermType.level_exp_var'
    with type level_exp' = TermType.level_exp'
    with type object_id = TermType.object_id
    with type param' = TermType.param'
    with type operator' = TermType.operator'
    with type term' = TermType.term'
    with type bound_term' = TermType.bound_term')
   (RefineError : RefineErrorSig
    with type level_exp = TermType.level_exp
    with type param = TermType.param
    with type term = TermType.term
    with type bound_term = TermType.bound_term)
=
struct
   open RefineError
   open TermType
   open Term

   type term = TermType.term
   type operator = TermType.operator
   type level_exp = TermType.level_exp

   (*
    * Terms with no subterms.
    *)
   let is_no_subterms_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = []
           } when Opname.eq opname' opname -> true
    | _ -> false

   (*
    * Terms with one subterm
    *)
   let is_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [bt]
           } when Opname.eq opname' opname -> bt.bvars = []
    | _ -> false

   let mk_dep0_term opname t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [mk_simple_bterm t]}}

   let dest_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt]
      } when Opname.eq opname' opname -> dest_simple_bterm bt
    | _ -> REF_RAISE(RefineError ("dest_dep0_term", TermMatchError (t, "bad arity")))

   let one_subterm t = match dest_term t with
      { term_terms = [bt]}  -> dest_simple_bterm bt
    | _ -> REF_RAISE(RefineError ("one_subterm", TermMatchError (t, "bad arity")))

   let one_subterm_opname opname t = match dest_term t with
      { term_op = { op_name = opname' }; term_terms = [bt] }
        when Opname.eq opname' opname  ->
         dest_simple_bterm bt
    | _ -> REF_RAISE(RefineError ("one_subterm", TermMatchError (t, "bad arity")))

   (*
    * Terms with two subterms.
    *)
   let is_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [bt1; bt2]
           } when Opname.eq opname' opname ->
         bt1.bvars = [] && bt2.bvars = []
    | _ -> false

   let mk_dep0_dep0_term opname t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [mk_simple_bterm t1; mk_simple_bterm t2]}}

   let dest_dep0_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1 ; bt2 ]
      } when Opname.eq opname' opname ->
         dest_simple_bterm bt1, dest_simple_bterm bt2
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let two_subterms t = match dest_term t with
      { term_terms = [bt1; bt2]} ->
         dest_simple_bterm bt1, dest_simple_bterm bt2
    | _ -> REF_RAISE(RefineError ("two_subterms", TermMatchError (t, "bad arity")))

   let two_subterms_opname opname t = match dest_term t with
      { term_op = { op_name = opname' }; term_terms = [bt1; bt2]}
        when Opname.eq opname' opname ->
         dest_simple_bterm bt1, dest_simple_bterm bt2
    | _ -> REF_RAISE(RefineError ("two_subterms", TermMatchError (t, "bad arity")))

   (*
    * Terms with three subterms.
    *)
   let is_dep0_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = ([_; _; _] as bterms)
           } when Opname.eq opname' opname -> no_bvars bterms
    | _ -> false

   let mk_dep0_dep0_dep0_term opname t1 t2 t3 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [mk_simple_bterm t1; mk_simple_bterm t2; mk_simple_bterm t3]}}

   let dest_dep0_dep0_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3]
      } when Opname.eq opname' opname ->
         dest_simple_bterm bt1, dest_simple_bterm bt2, dest_simple_bterm bt3
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep0_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * Terms with four subterms.
    *)
   let is_dep0_dep0_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [bt1; bt2; bt3; bt4]
           } when Opname.eq opname' opname ->
         bt1.bvars = [] && bt2.bvars = [] && bt3.bvars = [] && bt4.bvars = []
    | _ -> false

   let mk_dep0_dep0_dep0_dep0_term opname t1 t2 t3 t4 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [mk_simple_bterm t1; mk_simple_bterm t2; mk_simple_bterm t3; mk_simple_bterm t4]}}

   let dest_dep0_dep0_dep0_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2; bt3; bt4]
      } when Opname.eq opname' opname ->
         dest_simple_bterm bt1, dest_simple_bterm bt2, dest_simple_bterm bt3, dest_simple_bterm bt4
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep0_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let is_two_subterm opname t = match get_core t with
      Term { term_op = { op_name = opname' };
             term_terms = ([_; _] as bterms)
           } when Opname.eq opname' opname -> no_bvars bterms
    | _ -> false

   let is_three_subterm opname t = match get_core t with
      Term { term_op = { op_name = opname' };
             term_terms = ([_; _; _] as bterms)
           } when Opname.eq opname' opname -> no_bvars bterms
    | _ -> false

   let is_four_subterm opname t = match get_core t with
      Term { term_op = { op_name = opname' };
             term_terms = ([_; _; _; _] as bterms)
           } when Opname.eq opname' opname -> no_bvars bterms
    | _ -> false

   let is_five_subterm opname t = match get_core t with
      Term { term_op = { op_name = opname' };
             term_terms = ([_; _; _; _; _] as bterms)
           } when Opname.eq opname' opname -> no_bvars bterms
    | _ -> false

   let three_subterms t = match dest_term t with
      { term_op = { op_name = opname' };
        term_terms = [bt1; bt2; bt3]
      } ->
         dest_simple_bterm bt1, dest_simple_bterm bt2, dest_simple_bterm bt3
    | _ -> REF_RAISE(RefineError ("three_subterms", TermMatchError (t, "bad arity")))

   let four_subterms t = match dest_term t with
      { term_op = { op_name = opname' };
        term_terms = [bt1; bt2; bt3; bt4]
      } ->
         dest_simple_bterm bt1, dest_simple_bterm bt2, dest_simple_bterm bt3, dest_simple_bterm bt4
    | _ -> REF_RAISE(RefineError ("four_subterms", TermMatchError (t, "bad arity")))

   let five_subterms t = match dest_term t with
      { term_op = { op_name = opname' };
        term_terms = [bt1; bt2; bt3; bt4; bt5]
      } ->
          dest_simple_bterm bt1, dest_simple_bterm bt2, dest_simple_bterm bt3, dest_simple_bterm bt4, dest_simple_bterm bt5
    | _ -> REF_RAISE(RefineError ("five_subterms", TermMatchError (t, "bad arity")))

   let six_subterms t = match dest_term t with
      { term_op = { op_name = opname' };
        term_terms = [bt1; bt2; bt3; bt4; bt5; bt6]
      } ->
          dest_simple_bterm bt1, dest_simple_bterm bt2, dest_simple_bterm bt3, dest_simple_bterm bt4, dest_simple_bterm bt5, dest_simple_bterm bt6
    | _ -> REF_RAISE(RefineError ("six_subterms", TermMatchError (t, "bad arity")))

   (************************************************************************
    * Nonsimple but useful forms                                           *
    ************************************************************************)

   (*
    * One string param.
    *)
   let is_string_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String _] };
             term_terms = []
           } when Opname.eq opname opname' ->
         true
    | _ ->
         false

   let dest_string_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String s] };
        term_terms = []
      } when Opname.eq opname opname' ->
         s
    | _ ->
         REF_RAISE(RefineError ("dest_string_term", TermMatchError (t, "not a string term")))

   let dest_string_param t = match dest_term t with
      { term_op = { op_params = String s :: _ } } ->
         s
    | _ ->
         REF_RAISE(RefineError ("dest_string_param", TermMatchError (t, "no string parameter")))

   let mk_string_term opname s =
      { free_vars = Vars StringSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [String s] }; term_terms = [] }}

   (*
    * One string parameter, and one simple subterm.
    *)
   let is_string_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String _] };
             term_terms = [{bvars=[]}]
           } -> Opname.eq opname opname'
    | _ -> false

   let dest_string_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String s] };
        term_terms = [bt]
      } when Opname.eq opname opname' -> s, dest_simple_bterm bt
    | _ -> REF_RAISE(RefineError ("dest_string_dep0_term", TermMatchError (t, "bad arity")))

   let mk_string_dep0_term opname s t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = [mk_simple_bterm t] }}

   (*
    * Two string parameters, and one simple subterm.
    *)
   let is_string_string_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String _; String _] };
             term_terms = [{bvars=[]}]
           } -> Opname.eq opname opname'
    | _ ->
         false

   let dest_string_string_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [bt]
      } when Opname.eq opname opname' ->
         s1, s2, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_string_string_dep0_term", TermMatchError (t, "bad arity")))

   let dest_string_string_dep0_any_term t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [bt]
      } ->
         s1, s2, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_string_string_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_string_string_dep0_term opname s1 s2 t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [String s1; String s2] };
           term_terms = [mk_simple_bterm t] }}

   (*
    * Two number parameters and one subterm.
    *)
   let is_number_number_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number _; Number _] };
             term_terms = [ {bvars=[]} ]
           } -> Opname.eq opname opname'
    | _ ->
         false

   let dest_number_number_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2] };
        term_terms = [bt]
      } when Opname.eq opname opname' ->
         s1, s2, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_number_number_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_number_dep0_any_term t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2] };
        term_terms = [bt]
      } ->
         s1, s2, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_number_number_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_number_dep0_term opname s1 s2 t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [Number s1; Number s2] };
           term_terms = [mk_simple_bterm t]}}

   (*
    * Two number parameters, a string, and one subterm.
    *)
   let is_number_number_string_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number _; Number _; String _] };
             term_terms = [ {bvars=[]} ]
           } -> Opname.eq opname opname'
    | _ ->
         false

   let dest_number_number_string_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2; String s3] };
        term_terms = [bt]
      } when Opname.eq opname opname' ->
         s1, s2, s3, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_number_number_dep0_term", TermMatchError (t, "bad arity")))

   let dest_number_number_string_dep0_any_term t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Number s1; Number s2; String s3] };
        term_terms = [bt]
      } ->
         s1, s2, s3, dest_simple_bterm bt
    | _ ->
         REF_RAISE(RefineError ("dest_number_number_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_number_number_string_dep0_term opname s1 s2 s3 t =
      { free_vars = t.free_vars;
        core = Term
         { term_op = { op_name = opname; op_params = [Number s1; Number s2; String s3] };
           term_terms = [mk_simple_bterm t]}}

   (*
    * Two string parameters, two subterms.
    *)
   let is_string_string_dep0_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [String _; String _] };
             term_terms = [{bvars=[]};{bvars=[]}]
           } when Opname.eq opname opname' -> true
    | _ ->
         false

   let dest_string_string_dep0_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [bt1;bt2]
      } when Opname.eq opname opname' ->
         let destr = dest_simple_bterm in
         s1, s2, destr bt1, destr bt2
    | _ ->
         REF_RAISE(RefineError ("dest_string_string_dep0_dep0_term", TermMatchError (t, "bad arity")))

   let dest_string_string_dep0_dep0_any_term t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [String s1; String s2] };
        term_terms = [bt1;bt2]
      } ->
         let destr = dest_simple_bterm in
         s1, s2, destr bt1, destr bt2
    | _ ->
         REF_RAISE(RefineError ("dest_string_string_dep0_dep0_any_term", TermMatchError (t, "bad arity")))

   let mk_string_string_dep0_dep0_term opname s1 s2 t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [String s1; String s2] };
           term_terms = [mk_simple_bterm t1; mk_simple_bterm t2]}}

   (*
    * One number param.
    *)
   let is_number_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Number _] };
             term_terms = []
           } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_number_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Number n] };
        term_terms = []
      } when Opname.eq opname opname' -> n
    | _ -> REF_RAISE(RefineError ("dest_number_term", TermMatchError (t, "bad arity")))

   let dest_number_any_term t = match dest_term t with
      { term_op = { op_params = [Number n] };
        term_terms = []
      } ->
         n
    | _ ->
         REF_RAISE(RefineError ("dest_number_any_term", TermMatchError (t, "bad arity")))

   let mk_number_term opname n =
      { free_vars = Vars StringSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [Number n] };
           term_terms = [] }}

   (*
    * One universe param.
    *)
   let is_univ_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [MLevel _] };
             term_terms = []
           } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_univ_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [MLevel n] };
        term_terms = []
      } when Opname.eq opname opname' -> n
    | _ -> REF_RAISE(RefineError ("dest_univ_term", TermMatchError (t, "")))

   let mk_univ_term opname n =
      { free_vars = Vars StringSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [MLevel n] };
           term_terms = [] }}

   (*
    * One token param.
    *)
   let is_token_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [Token _] };
             term_terms = []
           } when Opname.eq opname opname' -> true
    | _ -> false

   let dest_token_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [Token n] };
        term_terms = []
      } when Opname.eq opname opname' -> n
    | _ -> REF_RAISE(RefineError ("dest_token_term", TermMatchError (t, "bad arity")))

   let mk_token_term opname n =
      { free_vars = Vars StringSet.empty;
        core = Term
         { term_op = { op_name = opname; op_params = [Token n] };
           term_terms = [] }}

   (*
    * One token param.
    *)
   let is_token_simple_term opname t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [Token _] };
                term_terms = terms
              } when Opname.eq opname opname' ->
            no_bvars terms
       | _ ->
            false

   let dest_token_simple_term opname t =
      match dest_term t with
         { term_op = { op_name = opname'; op_params = [Token n] };
           term_terms = terms
         } when Opname.eq opname opname' ->
            n, List.map dest_simple_bterm terms
       | _ ->
            REF_RAISE(RefineError ("dest_token_term", TermMatchError (t, "bad arity")))

   let mk_token_simple_term opname n terms =
      { free_vars = VarsDelayed;
        core = Term { term_op = { op_name = opname; op_params = [Token n] };
                      term_terms = List.map mk_simple_bterm terms
               }
      }

   (*
    * Bound term.
    *)
   let is_dep1_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{bvars=[_]}]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep1_term opname v t =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [{ bvars = [v]; bterm = t }]}}

   let dest_dep1_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v]; bterm = t }]
      } when Opname.eq opname' opname -> v,t
    | _ -> REF_RAISE(RefineError ("dest_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep1_dep1_term opname t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [] };
                term_terms = [{ bvars = [_] }; { bvars = [_] }]
         } -> Opname.eq opname' opname
       | _ ->
            false

   let mk_dep1_dep1_term opname v1 t1 v2 t2 =
      { free_vars = VarsDelayed;
        core = Term { term_op = { op_name = opname; op_params = [] };
                      term_terms =
                         [{ bvars = [v1]; bterm = t1 };
                          { bvars = [v2]; bterm = t2 }]
               }
      }

   let dest_dep1_dep1_term opname t =
      match dest_term t with
         { term_op = { op_name = opname'; op_params = [] };
           term_terms = [{ bvars = [v1]; bterm = t1 };
                         { bvars = [v2]; bterm = t2 }]
         } when Opname.eq opname' opname -> v1, t1, v2, t2
       | _ ->
            REF_RAISE(RefineError ("dest_dep1_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep2_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{bvars=[_;_]}]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep2_term opname v1 v2 t =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [{ bvars = [v1;v2]; bterm = t }]}}

   let dest_dep2_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v1;v2]; bterm = t }]
      } when Opname.eq opname' opname -> v1,v2,t
    | _ -> REF_RAISE(RefineError ("dest_dep2_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep1_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{bvars=[]};{bvars=[_]}]
           } -> Opname.eq opname' opname
    | _ -> false

   let is_dep0_dep1_any_term t = match get_core t with
      Term { term_op = { op_params = [] };
             term_terms = [{bvars=[]};{bvars=[_]}] } -> true
    | _ -> false

   let mk_dep0_dep1_term opname v t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
               [ mk_simple_bterm t1;
                 { bvars = [v]; bterm = t2 }]}}

   let mk_dep0_dep1_any_term op v t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = op;
           term_terms =
               [ mk_simple_bterm t1;
                 { bvars = [v]; bterm = t2 }]}}

   let dest_dep0_dep1_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1;bt2]
      } when Opname.eq opname' opname ->
      begin match (bt1, bt2) with
         ({ bvars = []; bterm = t1 }, { bvars = [v]; bterm = t2 }) ->
            v, t1, t2
       | _ -> REF_RAISE(RefineError ("dest_dep0_dep1_term", TermMatchError (t, "bad arity")))
      end
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep1_term", TermMatchError (t, "bad arity")))

   let dest_dep0_dep1_any_term t = match dest_term t with
      { term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = [v]; bterm = t2 }] } -> v, t1, t2
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep1_any_term", TermMatchError (t, "bad arity")))

   let is_dep1_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{bvars=[_]};{bvars=[]}]
           } -> Opname.eq opname' opname
    | _ -> false

   let is_dep1_dep0_any_term t = match get_core t with
      Term { term_op = { op_params = [] };
             term_terms = [{bvars=[_]};{bvars=[]}] } -> true
    | _ -> false

   let mk_dep1_dep0_term opname v t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
               [ { bvars = [v]; bterm = t1 };
                 { bvars = []; bterm = t2 }]}}

   let mk_dep1_dep0_any_term op v t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = op;
           term_terms =
               [ { bvars = [v]; bterm = t1 };
                 { bvars = []; bterm = t2 }]}}

   let dest_dep1_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bt1; bt2]
      } when Opname.eq opname' opname ->
      begin match (bt1, bt2) with
         ({ bvars = [v]; bterm = t1 }, { bvars = []; bterm = t2 }) ->
            v, t1, t2
       | _ -> REF_RAISE(RefineError ("dest_dep1_dep0_term", TermMatchError (t, "bad arity")))
      end
    | _ -> REF_RAISE(RefineError ("dest_dep1_dep0_term", TermMatchError (t, "bad arity")))

   let dest_dep1_dep0_any_term t = match dest_term t with
      { term_terms = [{ bvars = [v]; bterm = t1 };
                      { bvars = []; bterm = t2 }] } -> v, t1, t2
    | _ -> REF_RAISE(RefineError ("dest_dep1_dep0_any_term", TermMatchError (t, "bad arity")))

   (*
    * First subterm of arity 2.
    *)
   let is_dep2_dep0_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{bvars=[_;_]};{bvars=[]}]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep2_dep0_term opname v1 v2 t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [{ bvars = [v1; v2]; bterm = t1 };
             mk_simple_bterm t2]}}

   let dest_dep2_dep0_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = [v1; v2]; bterm = t1 };
                      { bvars = []; bterm = t2 }]
      } when Opname.eq opname' opname -> v1, v2, t1, t2
    | _ -> REF_RAISE(RefineError ("dest_dep2_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * Second subterm of arity 2.
    *)
   let is_dep0_dep2_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{bvars=[]};{bvars=[_;_]}]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep2_term opname v1 v2 t1 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [mk_simple_bterm t1;
             { bvars = [v1; v2]; bterm = t2 }]}}

   let dest_dep0_dep2_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t1 };
                      { bvars = [v1; v2]; bterm = t2 }]
      } when Opname.eq opname' opname -> v1, v2, t1, t2
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep2_term", TermMatchError (t, "bad arity")))

   (*
    * Second subterm of arity 2.
    *)
   let is_dep0_dep3_term opname t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [] };
                term_terms = [{bvars=[]};{bvars=[_;_;_]}]
              } -> Opname.eq opname' opname
       | _ ->
            false

   let mk_dep0_dep3_term opname v1 v2 v3 t1 t2 =
         { free_vars = VarsDelayed;
           core = Term
                  { term_op = { op_name = opname; op_params = [] };
                    term_terms =
                       [mk_simple_bterm t1;
                        { bvars = [v1; v2; v3]; bterm = t2 }]}}

   let dest_dep0_dep3_term opname t =
      match dest_term t with
         { term_op = { op_name = opname'; op_params = [] };
           term_terms = [{ bvars = []; bterm = t1 };
                         { bvars = [v1; v2; v3]; bterm = t2 }]
         } when Opname.eq opname' opname ->
            v1, v2, v3, t1, t2
       | _ ->
            REF_RAISE(RefineError ("dest_dep0_dep3_term", TermMatchError (t, "bad arity")))

   (*
    * Three subterms.
    *)
   let is_dep0_dep2_dep2_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{bvars=[]};{bvars=[_;_]};{bvars=[_;_]}]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep2_dep2_term opname t0 v11 v12 t1 v21 v22 t2 =
      mk_term
         { op_name = opname; op_params = [] }
         [mk_simple_bterm t0;
          mk_bterm [v11; v12] t1;
          mk_bterm [v21; v22] t2]

   let dest_dep0_dep2_dep2_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = [v11; v12]; bterm = t1 };
                      { bvars = [v21; v22]; bterm = t2 }]
      } when Opname.eq opname' opname ->
          t0, v11, v12, t1, v21, v22, t2
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep2_dep2_term", TermMatchError (t, "bad arity")))

   (*
    * Four subterms.
    *)
   let is_dep0_dep2_dep0_dep2_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = [] }; { bvars = [_; _] };
                           { bvars = [] }; { bvars = [_; _] }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep2_dep0_dep2_term opname t0 v11 v12 t1 base v21 v22 t2 =
         mk_term
            { op_name = opname; op_params = [] }
            [mk_simple_bterm t0;
             mk_bterm [v11; v12] t1;
             mk_simple_bterm base;
             mk_bterm [v21; v22] t2]

   let dest_dep0_dep2_dep0_dep2_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = [v11; v12]; bterm = t1 };
                      { bvars = []; bterm = base };
                      { bvars = [v21; v22]; bterm = t2 }]
      } when Opname.eq opname' opname ->
         t0, v11, v12, t1, base, v21, v22, t2
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep2_dep0_dep2_term", TermMatchError (t, "bad arity")))

   (*
    * Three subterms.
    *)
   let is_dep0_dep0_dep1_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [_] }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep0_dep1_term opname t0 t1 v2 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [mk_simple_bterm t0;
             mk_simple_bterm t1;
             { bvars = [v2]; bterm = t2 }]}}

   let dest_dep0_dep0_dep1_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = []; bterm = t1 };
                      { bvars = [v2]; bterm = t2 }]
      } when Opname.eq opname' opname ->
         t0, t1, v2, t2
    | _ ->
         REF_RAISE(RefineError ("dest_dep0_dep0_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep0_dep1_any_term t = match get_core t with
      Term { term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [_] }] } -> true
    | _ -> false

   let mk_dep0_dep0_dep1_any_term op t0 t1 v2 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = op;
           term_terms =
            [mk_simple_bterm t0;
             mk_simple_bterm t1;
             { bvars = [v2]; bterm = t2 }]}}

   let dest_dep0_dep0_dep1_any_term t = match dest_term t with
      { term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = []; bterm = t1 };
                      { bvars = [v2]; bterm = t2 }] } -> t0, t1, v2, t2
    | _ ->
         REF_RAISE(RefineError ("dest_dep0_dep0_dep1_any_term", TermMatchError (t, "bad arity")))

   (*
    * Three subterms.
    *)
   let is_dep0_dep0_dep2_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [_; _] }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep0_dep2_term opname t0 t1 v1 v2 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms =
            [mk_simple_bterm t0;
             mk_simple_bterm t1;
             { bvars = [v1; v2]; bterm = t2 }]}}

   let dest_dep0_dep0_dep2_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = []; bterm = t1 };
                      { bvars = [v1; v2]; bterm = t2 }]
      } when Opname.eq opname' opname -> t0, t1, v1, v2, t2
    | _ ->
         REF_RAISE(RefineError ("dest_dep0_dep0_dep2_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep0_dep2_any_term t = match get_core t with
      Term { term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [_; _] }] } -> true
    | _ -> false

   let mk_dep0_dep0_dep2_any_term op t0 t1 v1 v2 t2 =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = op;
           term_terms =
            [mk_simple_bterm t0;
             mk_simple_bterm t1;
             { bvars = [v1; v2]; bterm = t2 }]}}

   let dest_dep0_dep0_dep2_any_term t = match dest_term t with
      { term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = []; bterm = t1 };
                      { bvars = [v1; v2]; bterm = t2 }] } -> t0, t1, v1, v2, t2
    | _ ->
         REF_RAISE(RefineError ("dest_dep0_dep0_dep2_any_term", TermMatchError (t, "bad arity")))

   let is_dep0_dep1_dep1_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = [] }; { bvars = [_] }; { bvars = [_] }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep1_dep1_term opname t0 v1 t1 v2 t2 =
      mk_term
         { op_name = opname; op_params = [] }
         [mk_simple_bterm t0;
          mk_bterm [v1] t1;
          mk_bterm [v2] t2]

   let dest_dep0_dep1_dep1_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = [v1]; bterm = t1 };
                      { bvars = [v2]; bterm = t2 }]
      } when Opname.eq opname' opname -> t0, v1, t1, v2, t2
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep1_dep1_term", TermMatchError (t, "bad arity")))

   let is_dep2_dep2_dep0_dep0_term opname t =
      match get_core t with
         Term { term_op = { op_name = opname'; op_params = [] };
                term_terms = [{ bvars = [_; _] };
                              { bvars = [_; _] };
                              { bvars = [] };
                              { bvars = [] }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep2_dep2_dep0_dep0_term opname v11 v12 t1 v21 v22 t2 t3 t4 =
      mk_term
         { op_name = opname; op_params = [] }
         [mk_bterm [v11; v12] t1;
          mk_bterm [v21; v22] t2;
          mk_simple_bterm t3;
          mk_simple_bterm t4]

   let dest_dep2_dep2_dep0_dep0_term opname t =
      match dest_term t with
         { term_op = { op_name = opname'; op_params = [] };
           term_terms = [{ bvars = [v11; v12]; bterm = t1 };
                         { bvars = [v21; v22]; bterm = t2 };
                         { bvars = []; bterm = t3 };
                         { bvars = []; bterm = t4 }]
      } when Opname.eq opname' opname -> v11, v12, t1, v21, v22, t2, t3, t4
    | _ -> REF_RAISE(RefineError ("dest_dep2_dep2_dep0_dep0_term", TermMatchError (t, "bad arity")))

   (*
    * Three subterms.
    *)
   let is_dep0_dep0_dep3_term opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [{ bvars = [] }; { bvars = [] }; { bvars = [_; _; _] }]
           } -> Opname.eq opname' opname
    | _ -> false

   let mk_dep0_dep0_dep3_term opname t0 t1 v1 v2 v3 t2 =
      mk_term
         { op_name = opname; op_params = [] }
         [mk_simple_bterm t0;
          mk_simple_bterm t1;
          mk_bterm [v1; v2; v3] t2]

   let dest_dep0_dep0_dep3_term opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [{ bvars = []; bterm = t0 };
                      { bvars = []; bterm = t1 };
                      { bvars = [v1; v2; v3]; bterm = t2 }]
      } when Opname.eq opname' opname -> t0, t1, v1, v2, v3, t2
    | _ -> REF_RAISE(RefineError ("dest_dep0_dep0_dep3_term", TermMatchError (t, "bad arity")))

   (*
    * One subterm with opname.
    *)
   let is_one_bsubterm opname t = match get_core t with
      Term { term_op = { op_name = opname'; op_params = [] };
             term_terms = [_]
           } when Opname.eq opname' opname -> true
    | _ -> false

   let dest_one_bsubterm opname t = match dest_term t with
      { term_op = { op_name = opname'; op_params = [] };
        term_terms = [bterm]
      } when Opname.eq opname' opname -> bterm
    | _ -> REF_RAISE(RefineError ("dest_one_bsubterm", TermMatchError (t, "bad arity")))

   let mk_one_bsubterm opname bt =
      { free_vars = VarsDelayed;
        core = Term
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [bt] }}

   (************************************************************************
    * TERM MAPS                                                            *
    ************************************************************************)

   (*
    * Sweep a function down through the term.
    *)
   let rec bterm_down f btrm =
      { bvars = btrm.bvars; bterm = map_down f btrm.bterm }

   and map_down f t =
      let t = f t in
      match get_core t with
         Term trm ->
            make_term { term_op = trm.term_op; term_terms = List.map (bterm_down f) trm.term_terms }
       | FOVar _ -> t
       | Sequent _ -> raise (Invalid_argument "Term_op_ds.map_down: sequent code is not implemented")
       | Subst _ | Hashed _ -> fail_core "Term_op_ds.map_down"

   let rec bterm_up f btrm =
      { bvars = btrm.bvars; bterm = map_up f btrm.bterm }

   and map_up f t =
      match get_core t with
         Term trm ->
            f (make_term { term_op = trm.term_op; term_terms = List.map (bterm_up f) trm.term_terms })
       | FOVar _ -> f t
       | Sequent _ -> raise (Invalid_argument "Term_op_ds.map_up: sequent code is not implemented")
       | Subst _ | Hashed _ -> fail_core "Term_op_ds.map_up"
end
