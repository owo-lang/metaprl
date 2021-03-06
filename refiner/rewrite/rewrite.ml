(*
 * This module specifies rewrite rules, which require second
 * order variables.  Each rule has a "redex" and a "contractum",
 * although rewrites can be performed in either direction.
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
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *
 *)

INCLUDE "refine_error.mlh"

open Lm_debug
open Lm_symbol
open Lm_printf

open Term_sig
open Term_base_sig
open Term_op_sig
open Term_man_sig
open Term_addr_sig
open Term_subst_sig
open Term_shape_sig
open Refine_error_sig

open Rewrite_sig
open Rewrite_types
open Rewrite_util
open Rewrite_debug
open Rewrite_compile_redex
open Rewrite_compile_contractum
open Rewrite_match_redex
open Rewrite_build_contractum

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Rewrite%t"

let debug_rewrite = load_debug "rewrite"

(*
 * Rewrite module.
 *)
module Rewrite (**)
   (TermType : TermSig)
   (Term : TermBaseSig with module TermTypes = TermType)
   (TermOp : TermOpSig with module OpTypes = TermType)
   (TermMan : TermManSig with module ManTypes = TermType)
   (TermAddr : TermAddrSig with module AddrTypes = TermType)
   (TermSubst : TermSubstSig with module SubstTypes = TermType)
   (TermShape : TermShapeSig with type term = TermType.term and type param = TermType.param)

   (RefineError : RefineErrorSig with module Types = TermType) =
struct
   (************************************************************************
    * MODULES                                                              *
    ************************************************************************)

   open Term
   open TermMan
   open TermSubst
   open RefineError

   module RwTypes = struct
      include TermType
      type address = TermAddr.address
   end

   module RewriteTypes = MakeRewriteTypes (TermType) (TermAddr)
   module RewriteUtil = MakeRewriteUtil (TermType) (TermAddr) (Term) (RefineError)
   module RewriteDebug = MakeRewriteDebug (TermType) (Term) (TermAddr) (TermShape) (RefineError)
   module RewriteCompileRedex =
      MakeRewriteCompileRedex (TermType) (Term) (TermOp) (TermMan) (TermAddr) (**)
         (TermSubst) (RefineError) (RewriteUtil) (RewriteDebug)
   module RewriteCompileContractum =
      MakeRewriteCompileContractum (TermType) (Term) (TermMan) (TermAddr) (**)
         (TermSubst) (RefineError) (RewriteUtil) (RewriteDebug)
   module RewriteMatchRedex =
      MakeRewriteMatchRedex (TermType) (Term) (TermMan) (TermAddr) (TermSubst) (TermShape) (**)
         (RefineError) (RewriteUtil) (RewriteDebug)
   module RewriteBuildContractum =
      MakeRewriteBuildContractum (TermType) (Term) (TermMan) (TermAddr) (TermSubst) (TermShape) (**)
         (RefineError) (RewriteUtil) (RewriteDebug)

   open RwTypes
   open RewriteTypes
   open RewriteCompileRedex
   open RewriteCompileContractum
   open RewriteMatchRedex
   open RewriteBuildContractum
   open RewriteDebug

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type rw_args = address rw_args_poly
   type rewrite_args = rw_args * SymbolSet.t

   type rewrite_rule = RewriteTypes.rewrite_rule
   type rewrite_redex = RewriteTypes.rewrite_redex

   type rewrite_item = (param, term, level_exp) poly_rewrite_item

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   let opname_exn = RefineError ("Rewrite.apply_rewrite", RewriteStringError "opnames do not match")

   let empty_args_spec = { spec_ints = [||]; spec_addrs = [||] }
   let empty_rw_args = { arg_ints = [||]; arg_addrs = [||] }
   let empty_args = empty_rw_args, SymbolSet.empty

   let rec collect_hyp_bnames hyps bnames len i =
      if (len=0) then bnames else
         let bnames =
            match SeqHyp.get hyps i with
               Hypothesis (v, _) | Context (v, _, _) -> SymbolSet.add bnames v
         in
            collect_hyp_bnames hyps bnames (len-1) (i+1)

   let rec collect_bnames stack bnames len i =
      if i = len then bnames else match stack.(i) with
         StackSeqContext (_, (i', len', hyps)) ->
            collect_bnames stack (collect_hyp_bnames hyps bnames len' i') len (i+1)
       | StackBTerm (hyp, vars) ->
            collect_bnames stack (SymbolSet.union bnames (SymbolSet.subtract_list (free_vars_set hyp) vars)) len (i+1)
       | _ -> collect_bnames stack bnames len (i+1)

   (*
    * To do the rewrite. match agaist the redex, then
    * instantiate the contractum.
    *)
   let apply_rewrite rw (addrs, bnames) goal params =
      let _ =
         (* Check the opnames to short-circuit applications that quickly fail *)
         match rw.rr_redex with
            RWComposite { rw_op = { rw_name = opname1; _ }; _ } :: _ ->
               let opname2 = opname_of_term goal in
                  if not (Opname.eq opname1 opname2) then
                     REF_RAISE(opname_exn);
(*
 * JYH sometimes likes to see the actual opnames.
                     REF_RAISE(RefineError ("Rewrite.apply_rewrite", RewriteStringOpnameOpnameError ("opnames do not match", opname1, opname2)))
 *)
          | _ ->
               ()
      in
      let bnames =
         if rw.rr_strict == Strict then
            SymbolSet.union bnames (free_vars_set goal)
         else
            SymbolSet.empty
      in
      let gstack = Array.create rw.rr_gstacksize StackVoid in
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrite then
               eprintf "Rewrite.apply_rewrite: match_redex on %a%t" debug_print goal eflush
         END;
         match_redex addrs gstack bnames goal params rw.rr_redex;
         let result =
            match rw.rr_contractum with
               RWCTerm con ->
                  IFDEF VERBOSE_EXN THEN
                     if !debug_rewrite then
                        eprintf "Rewrite.apply_rewrite: build_contractum%t" eflush
                  END;
                  let bnames = if (rw.rr_strict==Strict) then
                     collect_bnames gstack bnames rw.rr_gstacksize 0
                     else SymbolSet.empty
                  in List.map (build_contractum bnames gstack) con
             | RWCFunction f ->
                  if params == [] then
                     [f goal]
                  else
                     REF_RAISE(RefineError ("apply_rewrite", RewriteBadMatch (TermMatch xnil_term)))
         in
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "Rewrite.apply_rewrite: done, result: [%a]%t" (print_any_list debug_print) result eflush
            END;
            result

   (*
    * Compute the redex types.
    *)
   let extract_redex_type = function
      FreeFOVarPattern s
    | FreeFOVarInstance s
    | SOVarPattern (s, _, 0) -> RewriteTermType, s
    | PVar (s, ShapeNumber) -> RewriteNumType, s
    | FOVar s
    | PVar (s, ShapeVar) -> RewriteVarType, s
    | PVar (s, ShapeString) -> RewriteStringType, s
    | PVar (s, ShapeToken) -> RewriteTokenType, s
    | PVar (s, ShapeShape) -> RewriteShapeType, s
    | PVar (s, ShapeOperator) -> RewriteOperatorType, s
    | PVar (s, ShapeLevel) -> RewriteLevelType, s
    | SOVarPattern (s , _, _)
    | SOVarMaybePattern (s, _, _)
    | SOVarInstance (s , _, _)
    | CVar (s, _, _)
    | PVar (s, ShapeQuote) -> RewriteUnsupportedType, s

   let extract_redex_types { redex_stack = stack; _ } =
      let l = Array.length stack in
      let rec aux j =
         if j < l then
            (extract_redex_type stack.(j))::(aux (j + 1))
         else
            []
      in
         aux 0

   (*
    * Given the two stack, extract values that can be used in a program.
    * For each object:
    *    1. A second order variable becomes a function
    *       that takes a list of subexpressions and performs the substitution.
    *    2. A first order variable becomes the string with the name.
    *    3. A context variable is converted to a function
    *       that takes the hole and the subexpressions and
    *       performs the substitution
    *    4. A param variable becaome the param that was matched.
    *)
   let extract_exn = RefineError ("extract_redex_values", RewriteStringError "internal error: stack entry is not valid")

   let extract_redex_values_aux gstack = function
      SOVarPattern (_, _, 0) ->
         begin
            match gstack with
               StackBTerm (t, []) -> RewriteTerm t
             | _ -> REF_RAISE(extract_exn)
         end
    | FreeFOVarPattern _ ->
         begin
            match gstack with
               StackVar v -> RewriteTerm (mk_var_term v)
             | _ -> REF_RAISE(extract_exn)
         end
    | FreeFOVarInstance _
    | SOVarMaybePattern _
    | SOVarInstance _ ->
         raise (Invalid_argument "Rewrite.extract_redex_values: internal error: instance is not expected")
    | PVar (_, ShapeNumber) ->
         RewriteNum begin
            match gstack with
               StackNumber i -> RewriteParam i
             | StackVar v -> RewriteMetaParam v
             | _ -> REF_RAISE(extract_exn)
         end
    | FOVar _
    | PVar (_, ShapeString)
    | PVar (_, ShapeVar) ->
         RewriteString (**)
            (match gstack with
               StackString s -> RewriteParam s
             | StackVar v -> RewriteMetaParam v
             | _ ->
               REF_RAISE(extract_exn))
    | PVar (_, ShapeToken) ->
         RewriteToken (**)
            (match gstack with
               StackOpname opname -> RewriteParam opname
             | StackVar v -> RewriteMetaParam v
             | _ ->
               REF_RAISE(extract_exn))
    | PVar (_, ShapeShape) ->
         RewriteShape (**)
            (match gstack with
               StackShape sh -> RewriteParam sh
             | StackVar v -> RewriteMetaParam v
             | _ ->
               REF_RAISE(extract_exn))
    | PVar (_, ShapeOperator) ->
         RewriteOperator (**)
            (match gstack with
               StackOperator op -> RewriteParam op
             | StackVar v -> RewriteMetaParam v
             | _ ->
               REF_RAISE(extract_exn))
    | PVar (_, ShapeLevel) ->
         RewriteLevel (**)
            (match gstack with
               StackLevel l -> l
             | StackVar v -> mk_var_level_exp v
             | _ -> REF_RAISE(extract_exn))
    | SOVarPattern _
    | PVar (_, ShapeQuote)
    | CVar _ ->
         RewriteUnsupported

   let extract_redex_values gstack stack=
      let l = Array.length gstack in
      let rec aux' i =
         if i < l then
            (extract_redex_values_aux gstack.(i) stack.(i))::(aux' (i + 1))
         else
            []
      in
         aux' 0

   (*
    * Match with a redex, and extract the forms to be bound.
    *)
   let test_redex_applicability { redex_stack = stack; redex_redex = redex } addrs term terms =
      let gstack = Array.create (Array.length stack) StackVoid in
         match_redex addrs gstack SymbolSet.empty term terms redex

   let apply_redex { redex_stack = stack; redex_redex = redex } addrs term terms =
      let gstack = Array.create (Array.length stack) StackVoid in
         match_redex addrs gstack SymbolSet.empty term terms redex;
         extract_redex_values gstack stack

   (*
    * Compile redex and contractum, and form a rewrite rule.
    *)
   let term_rewrite strict addrs redex contracta =
      let stack, redex = compile_so_redex strict addrs redex in
      let contracta = compile_so_contracta strict stack contracta in
         { rr_redex = redex;
           rr_contractum = RWCTerm (contracta);
           rr_gstacksize = Array.length stack;
           rr_strict = strict;
         }

   (*
    * Make a ML function rewrite.
    *)
   let fun_rewrite strict redex f =
      let stack, redex' = compile_so_redex strict empty_args_spec [redex] in
         { rr_redex = redex';
           rr_contractum = RWCFunction f;
           rr_gstacksize = Array.length stack;
           rr_strict = strict;
         }

   (*
    * Compile just the redex.
    *)
   let compile_redices strict addrs redices =
      let stack, redices = compile_so_redex strict addrs redices in
         { redex_stack = stack; redex_redex = redices }

   let compile_redex strict addrs redex =
      let redex = compile_redices strict addrs [redex] in
         match redex.redex_redex with
            [_] ->
               redex
          | _ ->
               failwith "compile_redex: too many redices"

   (*
    * Printing.
    *)
   let print_rewrite_redex out redex =
      let { redex_stack = stack;
            redex_redex = redex
          } = redex
      in
         fprintf out "@[<hv 3>Redex:@ @[<v 3>Stack:@ %a@]@ @[<v 3>Terms:" print_rstack stack;
         List.iter (fun prog -> fprintf out "@ %a" print_prog prog) redex;
         fprintf out "@]@]"

   let print_rewrite_rule out rw =
      let { rr_redex = redex;
            rr_gstacksize = size;
            rr_contractum = con;
            rr_strict = strict
          } = rw
      in
         fprintf out "@[<hv 0>@[<hv 3>RewriteRule {";
         fprintf out "@ @[<v 3>Redex:";
         List.iter (fun prog -> fprintf out "@ %a" print_prog prog) redex;
         fprintf out "@]@ Size = %d" size;
         fprintf out "@ @[<v 3>Contractum:%a@]" print_contractum con;
         fprintf out "@ Mode = %a" print_strict strict;
         fprintf out "@]@ }@]"
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
