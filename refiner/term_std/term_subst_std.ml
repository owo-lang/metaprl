(*
 * Substitution, alpha equality, and unification.
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

INCLUDE "refine_error.mlh"

open Lm_debug
open Lm_symbol
open Lm_printf

open Opname
open Term_sig
open Refine_error_sig
open Term_std_sig
open Term_std

IFDEF VERBOSE_EXN THEN

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Term_subst_std%t"

let debug_alpha_equal =
   create_debug (**)
      { debug_name = "alpha_equal";
        debug_description = "display alpha equality operations";
        debug_value = false
      }

ENDIF

module TermSubst
(Term : TermStdSig
        with type level_exp_var = TermType.level_exp_var
        with type level_exp = TermType.level_exp
        with type param = TermType.param
        with type operator = TermType.operator
        with type term = TermType.term
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
   type param = TermType.param
   type bound_term = TermType.bound_term
   type bound_term' = TermType.bound_term'

   type term_subst = (var * term) list

   (************************************************************************
    * Free variable calculations                                           *
    ************************************************************************)

   (*
    * Calculate the list of free variables.
    * Also count second order variables.
    * Just recursively descend the term, keeping track
    * of bound variables.
    *)
   let rec free_vars_term gvars bvars = function
      { term_op = { op_name = opname; op_params = [Var v] }; term_terms = bterms } when Opname.eq opname var_opname ->
         (* This is a variable *)
         let gvars' =
            if List.mem v bvars or List.mem v gvars then
               gvars
            else
               v::gvars
         in
            free_vars_bterms gvars' bvars bterms
    | { term_terms = bterms } ->
         free_vars_bterms gvars bvars bterms

   and free_vars_bterms gvars bvars = function
      { bvars = vars; bterm = term}::l ->
         let bvars' = vars @ bvars in
         let gvars' = free_vars_term gvars bvars' term in
            free_vars_bterms gvars' bvars l

    | [] ->
         gvars

   (* Actual function *)
   let free_vars_list = free_vars_term [] []
   let free_vars_set t = SymbolSet.of_list (free_vars_list t)

   (* Collect over a list of terms *)
   let free_vars_terms =
      let rec aux gvars = function
         [] -> gvars
       | t::r -> aux (SymbolSet.add_list gvars (free_vars_list t)) r
      in
         aux SymbolSet.empty

   let free_vars_equal t1 t2 =
      (Sort.list (<) (free_vars_list t1)) = (Sort.list (<) (free_vars_list t2))

   (*
    * See if a variable is free.
    *)
   let rec is_var_free v = function
      { term_op = { op_name = opname; op_params = [Var v'] };
      } when Opname.eq opname var_opname && v' = v ->
         true
    | { term_terms = bterms } ->
         List.exists (is_var_free_bterm v) bterms

   and is_var_free_bterm v bt =
      (not (List.mem v bt.bvars)) && is_var_free v bt.bterm

   (*
    * See if any of the variables are free in the terms.
    *)
   let is_some_var_free vars =
      let rec free_vars_term vars bvars = function
         { term_op = { op_name = opname; op_params = [Var v'] };
         } when Opname.eq opname var_opname && List.mem v' vars ->
            true
       | { term_terms = bterms } ->
            free_vars_bterms vars bvars bterms

      and free_vars_bterms vars bvars = function
         { bvars = bvars'; bterm = term }::t ->
            let first =
               let vars = Lm_list_util.subtract vars bvars' in
                  vars <> [] && free_vars_term vars (bvars' @ bvars) term
            in
               first or free_vars_bterms vars bvars t
       | [] ->
            false
      in
         free_vars_term vars []

   let is_some_var_free_list vars =
         List.exists (is_some_var_free vars)

   (*
    * Similar operation on contexts.
    *)
   let rec context_vars_term cvars = function
      { term_op = { op_name = opname; op_params = [Var v] };
        term_terms = bterms
      } when Opname.eq opname context_opname ->
         let cvars' =
            if List.mem v cvars then
               cvars
            else
               v::cvars
         in
            context_vars_bterms cvars' bterms
    | { term_terms = bterms } ->
         context_vars_bterms cvars bterms

   and context_vars_bterms cvars = function
      { bterm = t }::l ->
         context_vars_bterms (context_vars_term cvars t) l
    | [] -> cvars

   let context_vars = context_vars_term []

   (************************************************************************
    * ALPHA EQUALITY                                                       *
    ************************************************************************)

   (*
    * Recursive computation of alpha equality.
    *)
   let rec equal_params p1 p2 =
      match p1, p2 with
         Number n1, Number n2 ->
            Lm_num.eq_num n1 n2
       | ParamList pl1, ParamList pl2 ->
            List.for_all2 equal_params pl1 pl2
       | _ ->
            p1 = p2

   let rec equal_term vars t t' =
      match t, t' with
         { term_op = { op_name = opname1; op_params = [Var v] };
           term_terms = []
         },
         { term_op = { op_name = opname2; op_params = [Var v'] };
           term_terms = []
         } when Opname.eq opname1 var_opname & Opname.eq opname2 var_opname ->
            Lm_list_util.check_assoc v v' vars
       | { term_op = { op_name = name1; op_params = params1 }; term_terms = bterms1 },
         { term_op = { op_name = name2; op_params = params2 }; term_terms = bterms2 } ->
            (Opname.eq name1 name2)
            & (Lm_list_util.for_all2 equal_params params1 params2)
            & (equal_bterms vars bterms1 bterms2)

   and equal_bterms vars bterms1 bterms2 =
      let equal_bterm = fun
         { bvars = bvars1; bterm = term1 }
         { bvars = bvars2; bterm = term2 } ->
            equal_term (Lm_list_util.zip_list vars bvars1 bvars2) term1 term2
      in
         Lm_list_util.for_all2 equal_bterm bterms1 bterms2

   let alpha_equal t1 t2 =
      DEFINE body = try equal_term [] t1 t2 with Failure _ -> false
      IN
      IFDEF VERBOSE_EXN THEN
         if !debug_alpha_equal then
            try
               let result = equal_term [] t1 t2 in
               eprintf "alpha_equal: %b:\n%a\n%a%t" result debug_print t1 debug_print t2 eflush;
               result
            with Failure _ ->
               eprintf "alpha_equal: false:\n%a\n%a%t" debug_print t1 debug_print t2 eflush;
               false
         else body
      ELSE
         body
      ENDIF

   let alpha_equal_vars t v t' v' =
      DEFINE body = try equal_term (Lm_list_util.zip v v') t t' with Failure _ -> false
      IN
      IFDEF VERBOSE_EXN THEN
         if !debug_alpha_equal then
            try
               let _ = equal_term (Lm_list_util.zip v v') t t' in
               eprintf "alpha_equal_vars: true%t" eflush;
               true
            with Failure _ ->
               eprintf "alpha_equal_vars: false%t" eflush;
               false
         else body
      ELSE
         body
      ENDIF

   let rev_mem a b = List.mem b a

   let equal_fun f sub =
      let rec equal_fun_term bvars = function
         { term_op = { op_name = opname; op_params = [Var v] };
           term_terms = [] } ,
         ({ term_op = { op_name = opname'; op_params = [Var v'] };
           term_terms = []
         } as t') when Opname.eq opname var_opname && Opname.eq opname' var_opname ->
            ( try Lm_list_util.try_check_assoc v v' bvars with Not_found ->
               ( try f t' (List.assoc v sub) with Not_found ->
                  v = v'
            ))
       | { term_op = { op_name = opname; op_params = [Var v] };
           term_terms = []
         }, t' when Opname.eq opname var_opname ->
            not (List.mem_assoc v bvars) &&
            not (Lm_list_util.assoc_in_range rev_mem (free_vars_list t') bvars) &&
            f t' (List.assoc v sub)
       | { term_op = { op_name = name1; op_params = params1 }; term_terms = bterms1 },
         { term_op = { op_name = name2; op_params = params2 }; term_terms = bterms2 } ->
            Opname.eq name1 name2 & params1 = params2 & equal_fun_bterms bvars bterms1 bterms2

      and equal_fun_bterms bvars bterms1 bterms2 =
         let equal_fun_bterm = fun
            { bvars = bvars1; bterm = term1 }
            { bvars = bvars2; bterm = term2 } ->
               equal_fun_term (Lm_list_util.zip_list bvars bvars1 bvars2) (term1, term2)
         in
            Lm_list_util.for_all2 equal_fun_bterm bterms1 bterms2
      in
         equal_fun_term

   (* See refiner/refsig/term_subst_sig.mlz for explanation of this function *)
   let alpha_equal_fun f t v t' items =
      try equal_fun f (Lm_list_util.zip v items) [] (t, t') with
         Failure _ -> false
       | Not_found -> false

   (************************************************************************
    * Substitution                                                         *
    ************************************************************************)

   (*
    * Utilities for subst.
    *)
   let rec fsubtract l = function
      [] -> l
    | h::t ->
         fsubtract (Lm_list_util.subtract l h) t

   (*
    * Add a var list.
    *)
   let add_renames_terms r l =
      let rec aux = function
         [] -> l
       | v::t -> (mk_var_term v)::(aux t)
      in
         aux r

   (*
    * Add a var list onto free vars.
    *)
   let add_renames_fv r l =
      let rec aux = function
         [] -> l
       | v::t -> [v]::(aux t)
      in
         aux r

   (*
    * New variable production.
    * renames are the variables to be renamed,
    * and fv is a list list of variables to avoid.
    * Our algorithm is slow and simple: just append an
    * index and increment until no more collisions.
    *)
   let rec fv_mem fv v =
      match fv with
         [] -> false
       | h::t ->
            List.mem v h || fv_mem t v

   let rec new_vars fv = function
      [] -> []
    | v::t ->
         (* Rename the first one, then add it to free vars *)
         let v' = new_name v (fv_mem fv) in
            v'::(new_vars ([v']::fv) t)

   (*
    * First order simultaneous substitution.
    *)
   let rec subst_term terms fv vars = function
      { term_op = { op_name = opname; op_params = [Var(v)] }; term_terms = [] } as t
      when Opname.eq opname var_opname && List.mem v vars ->
         (* Var case *)
         List.nth terms (Lm_list_util.find_index v vars)
    | { term_terms = [] } as t -> t (* Optimization *)
    | { term_op = op; term_terms = bterms } ->
         (* Other term *)
         { term_op = op; term_terms = subst_bterms terms fv vars bterms }

   and subst_bterms terms fv vars bterms =
      (* When subst through bterms, catch binding occurrences *)
      let rec subst_bterm = function
         { bvars = []; bterm = term } ->
            (* Optimize the common case *)
            { bvars = []; bterm = subst_term terms fv vars term }

       | { bvars = bvars; bterm = term } ->
            (* First subtract bound instances *)
            let flags = List.map (function v -> List.mem v bvars) vars in
            let vars' = Lm_list_util.remove_elements flags vars in
            let fv' = Lm_list_util.remove_elements flags fv in
            let terms' = Lm_list_util.remove_elements flags terms in

            (* If any of the binding variables are free, rename them *)
            let renames = Lm_list_util.subtract bvars (fsubtract bvars fv') in
               if renames <> [] then
                  let fv'' = (free_vars_list term)::fv' in
                  let renames' = new_vars fv'' renames in
                     { bvars = subst_bvars renames' renames bvars;
                       bterm = subst_term
                               (add_renames_terms renames' terms')
                               (add_renames_fv renames' fv')
                               (renames @ vars')
                               term
                     }
               else
                  { bvars = bvars;
                    bterm = subst_term terms' fv' vars' term
                  }
      in
         List.map subst_bterm bterms

   and subst_bvars renames' renames bvars =
      let subst_bvar v =
         try List.nth renames' (Lm_list_util.find_index v renames) with
            Not_found -> v
      in
         List.map subst_bvar bvars

   let subst term vars terms =
         subst_term terms (List.map free_vars_list terms) vars term

   let subst1 t var term =
      if List.mem var (free_vars_list t) then
         subst_term [term] [free_vars_list term] [var] t
      else
         t

   let apply_subst s t =
      if s = [] then t else
      let vs,ts = List.split s in
      subst t vs ts

   let rec need_to_rename avoid = function
      [] -> false
    | v::vs ->
         SymbolSet.mem avoid v or
         need_to_rename (SymbolSet.add avoid v) vs

   let rec compute_renames avoid avoid' = function
      [] -> [], [], []
    | v::vs ->
         if List.mem v vs then
            let v' = new_name v (fv_mem avoid') in
            let vs', renames, terms = compute_renames avoid ([v']::avoid') vs in
               (v'::vs'), renames, terms
         else if SymbolSet.mem avoid v then
            let v' = new_name v (fv_mem avoid') in
            let vs', renames, terms = compute_renames avoid ([v']::avoid') vs in
               (v'::vs'), (v::renames), ((mk_var_term v') :: terms)
         else
            let vs', renames, terms = compute_renames avoid avoid' vs in
            (v::vs'), renames, terms

   let dest_bterm_and_rename bt avoid =
      if need_to_rename avoid bt.bvars then
         let avoid' = [ free_vars_list bt.bterm; bt.bvars; SymbolSet.to_list avoid ] in
         let bvars, renames, terms = compute_renames avoid avoid' bt.bvars in
         {
            bvars = bvars;
            bterm = subst bt.bterm renames terms
         }
      else bt

   (*
    * Inverse substitution.
    *)
   let var_subst t t' v =
      let { term_op = { op_name = opname } } = t' in
      let fv = free_vars_set t' in
      let vt = mk_var_term v in
      let rec subst_term = function
         { term_op = { op_name = opname'; op_params = params };
           term_terms = bterms
         } as t ->
            (* Check if this is the same *)
            if Opname.eq opname' opname && alpha_equal t t' then
               vt
            else
               { term_op = { op_name = opname'; op_params = params };
                 term_terms = List.map subst_bterm bterms
               }

      and subst_bterm ({ bvars = vars; bterm = term } as bt) =
         if List.exists (SymbolSet.mem fv) vars then
            (* Avoid capture *)
            bt
         else if List.mem v vars then
            let av = vars @ free_vars_list term in
            let v' = new_name v (fun v -> List.mem v av) in
            let rename var = if var = v then v' else var in
            let term = subst1 term v (mk_var_term v') in
               { bvars = Lm_list_util.smap rename vars; bterm = subst_term term }
         else
            { bvars = vars; bterm = subst_term term }
      in
         subst_term t

   (*
    * Matching is like unification, but variable matches
    * are only allowed on the left.  There is no occurs-check.
    *)
   let rec check_bvars tvs = function
      [] -> ()
    | (_,v)::tl ->
         if List.mem v tvs then RAISE_GENERIC_EXN else
         check_bvars tvs tl

   let rec zip_cons l l1 l2 =
      match l1, l2 with
         v1::t1, v2::t2 ->
            zip_cons ((v1, v2) :: l) t1 t2
       | [], [] ->
            l
       | _ ->
            RAISE_GENERIC_EXN

   let rec match_terms subst bvars tm1 tm2 =
      if is_var_term tm1 then
         let v = dest_var tm1 in
            try
               let v' = List.assoc v bvars in
                  if v' = dest_var tm2 then
                     subst
                  else
                     RAISE_GENERIC_EXN
            with
               Not_found ->
                  try
                     let tm1 = List.assoc v subst in
                        if equal_term bvars tm1 tm2 then subst
                        else RAISE_GENERIC_EXN
                  with
                     Not_found ->
                        check_bvars (free_vars_list tm2) bvars;
                        (v, tm2) :: subst
      else
         let { term_op = { op_name = opname1; op_params = params1 };
               term_terms = bterms1
             } = tm1
         in
         let { term_op = { op_name = opname2; op_params = params2 };
               term_terms = bterms2
             } = tm2
         in
            if Opname.eq opname1 opname2 & params1 = params2 then
               match_bterms subst bvars bterms1 bterms2
            else
               RAISE_GENERIC_EXN

   and match_bterms subst bvars bterms1 bterms2 =
      match bterms1, bterms2 with
         (bt1 :: tl1), (bt2 :: tl2) ->
            let subst' =
               match_terms subst (zip_cons bvars bt1.bvars bt2.bvars) bt1.bterm bt2.bterm
            in
               match_bterms subst' bvars tl1 tl2
       | [], [] ->
            subst
       | _ ->
            RAISE_GENERIC_EXN

   let match_terms subst t1 t2 =
      IFDEF VERBOSE_EXN THEN
         try List.rev (match_terms subst [] t1 t2) with
            RefineError (_, GenericError) ->
               raise (RefineError ("Term_subst_std.match_terms", TermPairError (t1, t2)))
      ELSE
         List.rev (match_terms subst [] t1 t2)
      ENDIF

   (************************************************************************
    * Term standardizing.
    ************************************************************************)

   (*
    * Make all the vars different by giving them a unique numeric suffix.
    *)
   let rec standardize_bterm index { bvars = bvars; bterm = t } =
      let bvars, subst, index =
         List.fold_left (fun (bvars, subst, index) v ->
            let v_str = string_of_symbol v in
            let v' =
               try String.sub v_str 0 (String.rindex v_str '_') with
                  Not_found ->
                     v_str
            in
            let v' = Lm_symbol.make v' index in
            let t = mk_var_term v' in
            let bvars = v' :: bvars in
            let subst = (v, t) :: subst in
            let index = succ index in
               bvars, subst, index) ([], [], index) bvars
      in
      let bvars = List.rev bvars in
      let t = apply_subst subst t in
      let t, index = standardize_term index t in
         { bvars = bvars; bterm = t }, index

   and standardize_term index t =
      let { term_op = op; term_terms = bterms } = t in
         let bterms, index =
            List.fold_left (fun (bterms, index) bterm ->
               let bterm, index = standardize_bterm index bterm in
                  bterm :: bterms, index) ([], index) bterms
         in
         let t = mk_term op (List.rev bterms) in
            t, index

   let standardize t =
      fst (standardize_term 1 t)
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
