(*
 * This module provide conversion between proofs and terms.
 * We use a bi-directional memo.
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
 * Copyright (C) 1999-2004, MetaPRL Group
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
open Opname
open Weak_memo
open Mp_resource

open Term_sig
open Term_addr_sig
open Refiner_sig
open Refiner.Refiner.TermAddr
open Refiner.Refiner.RefineError

open Term_copy2_weak

open Tactic_boot_sig
open Tactic_boot
open Tactic_boot.TacticInternalType
open Tactic_boot.TacticInternal

module ProofTerm (ToTerm : RefinerSig) =
struct
   open ToTerm.Term
   open ToTerm.TermOp
   open ToTerm.TermMan

   (* Convert from this refiner's terms *)
   module TermCopy2 = TermCopy2Weak (Refiner.Refiner) (ToTerm);;

   let convert = TermCopy2.convert
   let revert = TermCopy2.revert

   (************************************************************************
    * TERM CONVERSION                                                      *
    ************************************************************************)

   (*
    * Term operations.
    *)
   let summary_opname         = mk_opname "Summary"           nil_opname

   let tactic_arg_op          = mk_opname "tactic_arg"        summary_opname
   let msequent_op            = mk_opname "msequent"          summary_opname

   let term_arg_op            = mk_opname "arg_term"          summary_opname
   let type_arg_op            = mk_opname "arg_type"          summary_opname
   let int_arg_op             = mk_opname "arg_int"           summary_opname
   let bool_arg_op            = mk_opname "arg_bool"          summary_opname
   let string_arg_op          = mk_opname "arg_string"        summary_opname
   let term_list_arg_op       = mk_opname "arg_term_list"     summary_opname
   let named_arg_op           = mk_opname "arg_named"         summary_opname
   let address_arg_op         = mk_opname "arg_address"       summary_opname

   let goal_op                = mk_opname "ext_goal"          summary_opname
   let identity_op            = mk_opname "ext_identity"      summary_opname
   let annotate_op            = mk_opname "ext_annotate"      summary_opname
   let unjustified_op         = mk_opname "ext_unjustified"   summary_opname
   let wrapped_op             = mk_opname "ext_wrapped"       summary_opname
   let compose_op             = mk_opname "ext_compose"       summary_opname
   let rule_op                = mk_opname "ext_rule"          summary_opname

   let status_bad_op          = mk_opname "status_bad"        summary_opname
   let status_incomplete_op   = mk_opname "status_incomplete" summary_opname
   let status_partial_op      = mk_opname "status_partial"    summary_opname
   let status_complete_op     = mk_opname "status_complete"   summary_opname

   let addr_subterm_op        = mk_opname "addr_subterm"      summary_opname
   let addr_arg_op            = mk_opname "addr_arg"          summary_opname
   let addr_clause_op         = mk_opname "addr_clause"       summary_opname

   (*
    * Make a term with a string parameter.
    *)
   let mk_simple_string_term opname s terms =
      let param = make_param (String s) in
      let op = mk_op opname [param] in
      let bterms = List.map (fun t -> mk_bterm [] t) terms in
         mk_term op bterms

   let mk_simple_int_term opname i terms =
      let param = make_param (Number (Lm_num.num_of_int i)) in
      let op = mk_op opname [param] in
      let bterms = List.map (fun t -> mk_bterm [] t) terms in
         mk_term op bterms

(* unused
   let mk_string_string_term opname s1 s2 =
      let param1 = make_param (String s1) in
      let param2 = make_param (String s2) in
      let op = mk_op opname [param1; param2] in
         mk_term op []

   let mk_string_int_term opname s i =
      let param1 = make_param (String s) in
      let param2 = make_param (Number (Lm_num.num_of_int i)) in
      let op = mk_op opname [param1; param2] in
         mk_term op []

   let dest_string_string_term t =
      match dest_params (dest_op (dest_term t).term_op).op_params with
            [String s1; String s2] ->
               s1, s2
          | _ ->
               raise (Failure "dest_string_string_term")
*)

   let dest_int_term t =
      match dest_params (dest_op (dest_term t).term_op).op_params with
            [Number n] ->
               Lm_num.int_of_num n
          | _ ->
               raise (Failure "dest_int_term")

   let mk_status_term status =
      let opname =
         match status with
            LazyStatusBad ->
               status_bad_op
          | LazyStatusDelayed
          | LazyStatusIncomplete ->
               status_incomplete_op
          | LazyStatusPartial ->
               status_partial_op
          | LazyStatusComplete ->
               status_complete_op
      in
         mk_simple_term opname []

   let dest_status t =
      let opname = opname_of_term t in
         if Opname.eq opname status_bad_op then
            LazyStatusBad
         else if Opname.eq opname status_incomplete_op then
            LazyStatusIncomplete
         else if Opname.eq opname status_partial_op then
            LazyStatusPartial
         else if Opname.eq opname status_complete_op then
            LazyStatusComplete
         else
            raise (RefineError ("Proof_boot.dest_status", StringError "ill-formed proof"))

   let mk_addr_term = function
      Subterm i -> mk_simple_int_term addr_subterm_op i []
    | ArgAddr -> mk_simple_term addr_arg_op []
    | ClauseAddr i -> mk_simple_int_term addr_clause_op i []

   let dest_addr t =
      let opname = opname_of_term t in
         if Opname.eq opname addr_subterm_op then
            Subterm (dest_int_term t)
         else if Opname.eq opname addr_arg_op then
            ArgAddr
         else if Opname.eq opname addr_clause_op then
            ClauseAddr (dest_int_term t)
         else
            raise (RefineError ("Proof_boot.dest_addr", StringError "ill-formed proof"))

   (*
    * Versions of the extract.
    *)
   module WeakMemo = TheWeakMemo

   (* %%MAGICBEGIN%% *)
   type term_index = TermCopy2.term_index
   type msequent_index = TermCopy2.msequent_index

   type term_weak_index = TermCopy2.term_weak_index
   type msequent_weak_index = TermCopy2.msequent_weak_index

   type 'term_index attribute_header =
      HeadTermArg of 'term_index
    | HeadTypeArg of 'term_index
    | HeadIntArg of int
    | HeadAddrArg of address
    | HeadBoolArg of bool
    | HeadStringArg of string
    | HeadTermListArg of 'term_index list

   type 'attribute_index attributes_header = (string * 'attribute_index) list

   type 'attribute_index arglist_header = 'attribute_index list

   type ('msequent_index, 'attributes_index) tactic_arg_header =
      'msequent_index * string * 'attributes_index

   type ('term_index, 'arglist_index, 'tactic_arg_index, 'extract_index) extract_header =
      HeadGoal of 'tactic_arg_index
    | HeadUnjustified of 'tactic_arg_index * 'tactic_arg_index list
    | HeadWrapped of 'arglist_index * 'extract_index
    | HeadCompose of 'extract_index * 'extract_index list * 'extract_index list
    | HeadRule of lazy_status * string * 'extract_index * 'extract_index list * 'extract_index list
    | HeadIdentity of 'tactic_arg_index
    | HeadAnnotate of 'tactic_arg_index * 'tactic_arg_index

   (*
    * Memo tables.
    *)
   type args_info =
      { sentinal : Tactic.sentinal;
        raw_attributes : raw_attributes;
        bookmark : global_resource;
        parse_expr : string -> MLast.expr;
        parse_tactic : MLast.expr -> tactic
      }

   type 'a d = 'a WeakMemo.descriptor
   type 'a wd = 'a WeakMemo.weak_descriptor

   (*
    * I know, this looks ugly, but the idea is pretty simple.
    * The reason it is so verbose is because we want a type for
    * both the proof -> term and term -> proof memo functions.
    *
    * There is a separate table for:
    *    attribute: all attributes are managed individually
    *    attributes: attributes in tactic_arg
    *    arglist: args for the Wrapped extract
    *    tactic_arg: arguments for tactics
    *    extract: proof terms
    *)
   type ('ext_attribute,  'term_attribute,
         'ext_attributes, 'term_attributes,
         'ext_arglist,    'term_arglist,
         'ext_tactic_arg, 'term_tactic_arg,
         'ext_extract,    'term_extract) memo =
      { attribute       : (args_info * ('ext_attribute,  'term_attribute,
                                        'ext_attributes, 'term_attributes,
                                        'ext_arglist,    'term_arglist,
                                        'ext_tactic_arg, 'term_tactic_arg,
                                        'ext_extract,    'term_extract) memo,
                           'ext_attribute,
                           term_index attribute_header,
                           term_weak_index attribute_header,
                           'term_attribute) WeakMemo.t;
        attributes      : (args_info * ('ext_attribute,  'term_attribute,
                                        'ext_attributes, 'term_attributes,
                                        'ext_arglist,    'term_arglist,
                                        'ext_tactic_arg, 'term_tactic_arg,
                                        'ext_extract,    'term_extract) memo,
                           'ext_attributes,
                           'term_attribute d attributes_header,
                           'term_attribute wd attributes_header,
                           'term_attributes) WeakMemo.t;
        arglist         : (args_info * ('ext_attribute,  'term_attribute,
                                        'ext_attributes, 'term_attributes,
                                        'ext_arglist,    'term_arglist,
                                        'ext_tactic_arg, 'term_tactic_arg,
                                        'ext_extract,    'term_extract) memo,
                           'ext_arglist,
                           'term_attribute d arglist_header,
                           'term_attribute wd arglist_header,
                           'term_arglist) WeakMemo.t;
        tactic_arg      : (args_info * ('ext_attribute,  'term_attribute,
                                        'ext_attributes, 'term_attributes,
                                        'ext_arglist,    'term_arglist,
                                        'ext_tactic_arg, 'term_tactic_arg,
                                        'ext_extract,    'term_extract) memo,
                           'ext_tactic_arg,
                           (msequent_index, 'term_attributes d) tactic_arg_header,
                           (msequent_weak_index, 'term_attributes wd) tactic_arg_header,
                           'term_tactic_arg) WeakMemo.t;
        extract         : (args_info * ('ext_attribute,  'term_attribute,
                                        'ext_attributes, 'term_attributes,
                                        'ext_arglist,    'term_arglist,
                                        'ext_tactic_arg, 'term_tactic_arg,
                                        'ext_extract,    'term_extract) memo,
                           'ext_extract,
                           (term_index, 'term_arglist d, 'term_tactic_arg d, 'term_extract d) extract_header,
                           (term_weak_index, 'term_arglist wd, 'term_tactic_arg wd, 'term_extract wd) extract_header,
                           'term_extract) WeakMemo.t
      }
   (* %%MAGICEND%% *)

   (*
    * Build weak headers.
    *)
   let weaken_term = TermCopy2.weaken

   let weaken_msequent = TermCopy2.weaken_msequent

   let weaken_attribute_header _ = function
      HeadTermArg t ->
         HeadTermArg (weaken_term t)
    | HeadTypeArg t ->
         HeadTypeArg (weaken_term t)
    | HeadIntArg _
    | HeadAddrArg _
    | HeadBoolArg _
    | HeadStringArg _ as h -> h
    | HeadTermListArg tl ->
         HeadTermListArg (List.map weaken_term tl)

   let weaken_attributes_header _ args =
      let weaken (name, arg) =
         name, WeakMemo.weaken arg
      in
         List.map weaken args

   let weaken_arglist_header _ args =
      List.map WeakMemo.weaken args

   let weaken_tactic_arg_header _ (goal, text, args) =
      weaken_msequent goal, text, WeakMemo.weaken args

   let weaken_extract_header _ = function
      HeadGoal arg ->
         HeadGoal (WeakMemo.weaken arg)
    | HeadUnjustified (goal, subgoals) ->
         HeadUnjustified (WeakMemo.weaken goal, List.map WeakMemo.weaken subgoals)
    | HeadWrapped (label, ext) ->
         HeadWrapped (WeakMemo.weaken label, WeakMemo.weaken ext)
    | HeadCompose (goal, subgoals, extras) ->
         HeadCompose (WeakMemo.weaken goal,
                      List.map WeakMemo.weaken subgoals,
                      List.map WeakMemo.weaken extras)
    | HeadRule (status, text, goal, subgoals, extras) ->
         HeadRule (status,
                   text,
                   WeakMemo.weaken goal,
                   List.map WeakMemo.weaken subgoals,
                   List.map WeakMemo.weaken extras)
    | HeadIdentity goal ->
         HeadIdentity (WeakMemo.weaken goal)
    | HeadAnnotate (goal1, goal2) ->
         HeadAnnotate (WeakMemo.weaken goal1, WeakMemo.weaken goal2)

   (*
    * Comparison functions.
    *)
   let list_mem_eq = Lm_list_util.compare_eq

   let compare_terms = TermCopy2.compare

   let compare_msequents = TermCopy2.compare_msequent

   let term_list_eq = Lm_list_util.compare_cmp compare_terms

   let compare_attribute arg1 arg2 =
      match arg1, arg2 with
         HeadTermArg t1, HeadTermArg t2
       | HeadTypeArg t1, HeadTypeArg t2 ->
            compare_terms t1 t2
       | HeadIntArg i1, HeadIntArg i2 ->
            i1 = i2
       | HeadAddrArg a1, HeadAddrArg a2 ->
            a1 = a2
       | HeadBoolArg b1, HeadBoolArg b2 ->
            b1 = b2
       | HeadStringArg s1, HeadStringArg s2 ->
            s1 = s2
       | HeadTermListArg tl1, HeadTermListArg tl2 ->
            term_list_eq tl1 tl2
       | _ ->
            false

   let compare_attributes args1 args2 =
      let compare (name1, arg1) (name2, arg2) =
         name1 = name2 && arg1 == arg2
      in
         Lm_list_util.for_all2 compare args1 args2

   let compare_arglist args1 args2 =
      list_mem_eq args1 args2

   let compare_tactic_arg (goal1, label1, args1) (goal2, label2, args2) =
      compare_msequents goal1 goal2 && label1 = label2 && args1 == args2

   let compare_extract ext1 ext2 =
      match ext1, ext2 with
         HeadGoal g1, HeadGoal g2 ->
            g1 == g2
       | HeadUnjustified (goal1, subgoals1), HeadUnjustified (goal2, subgoals2) ->
            goal1 == goal2 && list_mem_eq subgoals1 subgoals2
       | HeadWrapped (label1, ext1), HeadWrapped (label2, ext2) ->
            label1 == label2 && ext1 == ext2
       | HeadCompose (goal1, subgoals1, extras1), HeadCompose (goal2, subgoals2, extras2) ->
            goal1 == goal2 && list_mem_eq subgoals1 subgoals2 && list_mem_eq extras1 extras2
       | HeadRule (_, text1, goal1, subgoals1, extras1),
         HeadRule (_, text2, goal2, subgoals2, extras2) ->
            text1 = text2 && goal1 == goal2 && list_mem_eq subgoals1 subgoals2 && list_mem_eq extras1 extras2
       | HeadIdentity goal1, HeadIdentity goal2 ->
            goal1 == goal2
       | HeadAnnotate (goal11, goal12), HeadAnnotate (goal21, goal22) ->
            goal11 == goal21 && goal12 == goal22
       | _ ->
            false

   (*
    * Build a header from an extract.
    *)
   let ext_add_term = TermCopy2.add_src

   let ext_add_msequent = TermCopy2.add_msequent_src

   let rec ext_add_attribute info arg =
      WeakMemo.lookup (snd info).attribute info (ext_make_attribute_header info arg)

   and ext_add_attributes info args =
      WeakMemo.lookup (snd info).attributes info (ext_make_attributes_header info args)

   and ext_add_arglist info args =
      WeakMemo.lookup (snd info).arglist info (ext_make_arglist_header info args)

   and ext_add_tactic_arg info arg =
      WeakMemo.lookup (snd info).tactic_arg info (ext_make_tactic_arg_header info arg)

   and ext_add_extract info arg =
      WeakMemo.lookup (snd info).extract info (ext_make_extract_header info arg)

   and ext_make_attribute_header info = function
      TermArg t ->
         HeadTermArg (ext_add_term t)
    | TypeArg t ->
         HeadTypeArg (ext_add_term t)
    | IntArg i ->
         HeadIntArg i
    | AddrArg a ->
         HeadAddrArg a
    | BoolArg b ->
         HeadBoolArg b
    | StringArg s ->
         HeadStringArg s
    | TermListArg tl ->
         HeadTermListArg (List.map ext_add_term tl)

   and ext_make_attributes_header info args =
      List.map (fun (name, arg) -> name, ext_add_attribute info arg) args

   and ext_make_arglist_header info args =
      List.map (ext_add_attribute info) args

   and ext_make_tactic_arg_header info arg =
      ext_add_msequent arg.ref_goal,
      arg.ref_label,
      ext_add_attributes info (Tactic.attributes arg)

   and ext_make_extract_header info = function
      Goal arg ->
         HeadGoal (ext_add_tactic_arg info arg)
    | Unjustified (goal, subgoals)
    | Extract (goal, subgoals, _) ->
         HeadUnjustified (ext_add_tactic_arg info goal, List.map (ext_add_tactic_arg info) subgoals)
    | Wrapped (label, ext) ->
         HeadWrapped (ext_add_arglist info (Tactic.expand_arglist label), ext_add_extract info ext)
    | Compose { comp_goal = goal;
                comp_subgoals = subgoals;
                comp_extras = extras;
                _
      } ->
         HeadCompose (ext_add_extract info goal,
                      List.map (ext_add_extract info) subgoals,
                      List.map (ext_add_extract info) extras)
    | RuleBox { rule_status = status;
                rule_string = text;
                rule_extract = goal;
                rule_subgoals = subgoals;
                rule_extras = extras;
                _
      } ->
         HeadRule (status,
                   text,
                   ext_add_extract info goal,
                   List.map (ext_add_extract info) subgoals,
                   List.map (ext_add_extract info) extras)
    | Pending f ->
         ext_make_extract_header info (f ())
    | Locked ext ->
         ext_make_extract_header info ext
    | Identity goal ->
         HeadIdentity (ext_add_tactic_arg info goal)
    | Annotate (goal1, goal2) ->
         HeadAnnotate (ext_add_tactic_arg info goal1, ext_add_tactic_arg info goal2)

   (*
    * Build an extract from the header.
    *)
   let ext_retrieve_term = TermCopy2.retrieve_src

   let ext_retrieve_msequent = TermCopy2.retrieve_msequent_src

   let ext_retrieve_attribute info arg =
      WeakMemo.retrieve (snd info).attribute info arg

   let ext_retrieve_attributes info arg =
      WeakMemo.retrieve (snd info).attributes info arg

   let ext_retrieve_arglist info arg =
      WeakMemo.retrieve (snd info).arglist info arg

   let ext_retrieve_tactic_arg info arg =
      WeakMemo.retrieve (snd info).tactic_arg info arg

   let ext_retrieve_extract info arg =
      WeakMemo.retrieve (snd info).extract info arg

   let ext_make_attribute info = function
      HeadTermArg t ->
         TermArg (ext_retrieve_term t)
    | HeadTypeArg t ->
         TypeArg (ext_retrieve_term t)
    | HeadIntArg i ->
         IntArg i
    | HeadAddrArg a ->
         AddrArg a
    | HeadBoolArg b ->
         BoolArg b
    | HeadStringArg s ->
         StringArg s
    | HeadTermListArg tl ->
         TermListArg (List.map ext_retrieve_term tl)

   let ext_convert_named_attribute info (name, arg) =
      match ext_retrieve_attribute info arg with
         TermArg t ->
            Tactic.term_attribute name t
       | TypeArg t ->
            Tactic.type_attribute name t
       | IntArg i ->
            Tactic.int_attribute name i
       | AddrArg a ->
            Tactic.addr_attribute name a
       | BoolArg b ->
            Tactic.bool_attribute name b
       | StringArg s ->
            Tactic.string_attribute name s
       | TermListArg tl ->
            Tactic.term_list_attribute name tl

   let ext_make_attributes info args =
      attribute_info_of_raw_attributes (List.map (ext_convert_named_attribute info) args @ (fst info).raw_attributes)

   let ext_make_arglist info args =
      Tactic.compress_arglist (List.map (ext_retrieve_attribute info) args)

   let ext_make_tactic_arg info (mseq, label, args) =
      { ref_goal = ext_retrieve_msequent mseq;
        ref_label = label;
        ref_attributes = ext_retrieve_attributes info args;
        ref_bookmark = (fst info).bookmark;
        ref_sentinal = (fst info).sentinal
      }

   let lazy_apply f x =
      let cell = ref None in
      let f () =
         match !cell with
            None ->
               let p = f x in
                  cell := Some p;
                  p
          | Some x ->
               x
      in
         f

   let ext_make_extract info = function
      HeadGoal arg ->
         Goal (ext_retrieve_tactic_arg info arg)
    | HeadUnjustified (goal, subgoals) ->
         let retrieve_tactic_arg = ext_retrieve_tactic_arg info
         in
            Unjustified (retrieve_tactic_arg goal, List.map retrieve_tactic_arg subgoals)
    | HeadWrapped (label, ext) ->
         Wrapped (ext_retrieve_arglist info label, ext_retrieve_extract info ext)
    | HeadCompose (goal, subgoals, extras) ->
         Compose { comp_status = LazyStatusDelayed;
                   comp_goal = ext_retrieve_extract info goal;
                   comp_subgoals = List.map (ext_retrieve_extract info) subgoals;
                   comp_leaves = LazyLeavesDelayed;
                   comp_extras = List.map (ext_retrieve_extract info) extras
         }
    | HeadRule (_, text, goal, subgoals, extras) ->
         let arg = fst info in
         let parse = arg.parse_expr in
         let eval = arg.parse_tactic in
         let expr = lazy_apply parse text in
         let tac = lazy_apply (fun text -> eval (parse text)) text in
            RuleBox { rule_status = LazyStatusDelayed;
                      rule_string = text;
                      rule_expr = expr;
                      rule_tactic = tac;
                      rule_extract_normalized = false;
                      rule_extract = ext_retrieve_extract info goal;
                      rule_subgoals = List.map (ext_retrieve_extract info) subgoals;
                      rule_leaves = LazyLeavesDelayed;
                      rule_extras = List.map (ext_retrieve_extract info) extras
            }
    | HeadIdentity goal ->
         Identity (ext_retrieve_tactic_arg info goal)
    | HeadAnnotate (goal1, goal2) ->
         Annotate (ext_retrieve_tactic_arg info goal1, ext_retrieve_tactic_arg info goal2)

   (*
    * Build a term from the header.
    *)
   let term_retrieve_term = TermCopy2.retrieve_dst

   let term_retrieve_msequent = TermCopy2.retrieve_msequent_dst

   let term_retrieve_attribute info arg =
      WeakMemo.retrieve (snd info).attribute info arg

   let term_retrieve_attributes info arg =
      WeakMemo.retrieve (snd info).attributes info arg

   let term_retrieve_arglist info arg =
      WeakMemo.retrieve (snd info).arglist info arg

   let term_retrieve_tactic_arg info arg =
      WeakMemo.retrieve (snd info).tactic_arg info arg

   let term_retrieve_extract info arg =
      WeakMemo.retrieve (snd info).extract info arg

   let term_make_msequent arg =
      let mseq = term_retrieve_msequent arg in
      let goal, subgoals = ToTerm.Refine.dest_msequent mseq in
         mk_simple_term msequent_op [goal; mk_xlist_term subgoals]

   let term_make_attribute info = function
      HeadTermArg t ->
         mk_simple_term term_arg_op [term_retrieve_term t]
    | HeadTypeArg t ->
         mk_simple_term type_arg_op [term_retrieve_term t]
    | HeadIntArg i ->
         mk_simple_int_term int_arg_op i []
    | HeadAddrArg a ->
         mk_simple_term address_arg_op [mk_xlist_term (List.map mk_addr_term (dest_address a))]
    | HeadBoolArg b ->
         mk_string_term bool_arg_op (if b then "true" else "false")
    | HeadStringArg s ->
         mk_string_term string_arg_op s
    | HeadTermListArg tl ->
         mk_simple_term term_list_arg_op [mk_xlist_term (List.map term_retrieve_term tl)]

   let term_make_named_attribute info (name, arg) =
      mk_simple_string_term named_arg_op name [term_retrieve_attribute info arg]

   let term_make_attributes info args =
      mk_xlist_term (List.map (term_make_named_attribute info) args)

   let term_make_arglist info args =
      mk_xlist_term (List.map (term_retrieve_attribute info) args)

   let term_make_tactic_arg info (goal, label, args) =
      mk_simple_string_term tactic_arg_op label [term_make_msequent goal;
                                                 term_retrieve_attributes info args]

   let term_make_extract info = function
      HeadGoal arg ->
         mk_simple_term goal_op [term_retrieve_tactic_arg info arg]
    | HeadUnjustified (goal, subgoals) ->
         mk_simple_term unjustified_op [term_retrieve_tactic_arg info goal;
                                        mk_xlist_term (List.map (term_retrieve_tactic_arg info) subgoals)]
    | HeadWrapped (label, ext) ->
         mk_simple_term wrapped_op [term_retrieve_arglist info label;
                                    term_retrieve_extract info ext]
    | HeadCompose (goal, subgoals, extras) ->
         mk_simple_term compose_op [term_retrieve_extract info goal;
                                    mk_xlist_term (List.map (term_retrieve_extract info) subgoals);
                                    mk_xlist_term (List.map (term_retrieve_extract info) extras)]
    | HeadRule (status, text, goal, subgoals, extras) ->
         mk_simple_string_term rule_op text [mk_status_term status;
                                             term_retrieve_extract info goal;
                                             mk_xlist_term (List.map (term_retrieve_extract info) subgoals);
                                             mk_xlist_term (List.map (term_retrieve_extract info) extras)]
    | HeadIdentity goal ->
         mk_simple_term identity_op [term_retrieve_tactic_arg info goal]
    | HeadAnnotate (goal1, goal2) ->
         mk_simple_term annotate_op [term_retrieve_tactic_arg info goal1; term_retrieve_tactic_arg info goal2]

   (*
    * Lookup values in the reverse direction.
    *)
   let term_add_term = TermCopy2.add_dst

   let term_add_msequent = TermCopy2.add_msequent_dst

   let term_add_msequent t =
      let op = opname_of_term t in
         if Opname.eq op msequent_op then
            let goal, assums = two_subterms t in
            let assums = dest_xlist assums in
               term_add_msequent (ToTerm.Refine.mk_msequent goal assums)
         else
            raise (RefineError ("Proof_boot.term_lookup_msequent", StringError "ill-formed proof"))

   let rec term_add_attribute info t =
      WeakMemo.lookup (snd info).attribute info (term_make_attribute_header info t)

   and term_add_attributes info t =
      WeakMemo.lookup (snd info).attributes info (term_make_attributes_header info t)

   and term_add_arglist info t =
      WeakMemo.lookup (snd info).arglist info (term_make_arglist_header info t)

   and term_add_tactic_arg info t =
      WeakMemo.lookup (snd info).tactic_arg info (term_make_tactic_arg_header info t)

   and term_add_extract info t =
      WeakMemo.lookup (snd info).extract info (term_make_extract_header info t)

   (*
    * Build the header from a term.
    *)
   and term_make_attribute_header info t =
      let op = opname_of_term t in
         if Opname.eq op term_arg_op then
            HeadTermArg (term_add_term (one_subterm t))
         else if Opname.eq op type_arg_op then
            HeadTypeArg (term_add_term (one_subterm t))
         else if Opname.eq op int_arg_op then
            HeadIntArg (dest_int_term t)
         else if Opname.eq op bool_arg_op then
            HeadBoolArg (dest_string_param t = "true")
         else if Opname.eq op string_arg_op then
            HeadStringArg (dest_string_param t)
         else if Opname.eq op term_list_arg_op then
            HeadTermListArg (List.map term_add_term (dest_xlist (one_subterm t)))
         else if Opname.eq op address_arg_op then
            HeadAddrArg (make_address (List.map dest_addr (dest_xlist (one_subterm t))))
         else
            raise (RefineError ("Proof_boot.attribute_header_of_term", StringError "ill-formed proof"))

   and term_make_named_attribute_header info t =
      if Opname.eq (opname_of_term t) named_arg_op then
         dest_string_param t, term_add_attribute info (one_subterm t)
      else
         raise (RefineError ("Proof_boot.named_attribute_header_of_term", StringError "ill-formed proof"))

   and term_make_attributes_header info t =
      List.map (term_make_named_attribute_header info) (dest_xlist t)

   and term_make_arglist_header info t =
      List.map (term_add_attribute info) (dest_xlist t)

   and term_make_tactic_arg_header info t =
      let op = opname_of_term t in
         if Opname.eq op tactic_arg_op then
            let label = dest_string_param t in
            (*
             * XXX HACK: The "match" is here because tactic_arg terms used to have 3 subterms in old files.
             * (ASCII IO format <= 1.0.9 and Term IO format <= 1.0.6)
             *)
            let goal, args = (* two_subterms t *)
               match (dest_term t).term_terms with
                  [g; a]
                | [g; a; _ ] -> (dest_bterm g).bterm, (dest_bterm a).bterm
                | _ -> raise (RefineError ("Proof_boot.tactic_arg_header_of_term", StringError "ill-formed proof"))
            in
               term_add_msequent goal,
               label,
               term_add_attributes info args
         else
            raise (RefineError ("Proof_boot.tactic_arg_header_of_term", StringError "ill-formed proof"))

   and term_make_goal_header info t =
      HeadGoal (term_add_tactic_arg info (one_subterm t))

   and term_make_identity_header info t =
      HeadIdentity (term_add_tactic_arg info (one_subterm t))

   and term_make_annotate_header info t =
      let t1, t2 = two_subterms t in
         HeadAnnotate (term_add_tactic_arg info t1, term_add_tactic_arg info t2)

   and term_make_unjustified_header info t =
      let goal, subgoals = two_subterms t in
      let subgoals = dest_xlist subgoals in
         HeadUnjustified (term_add_tactic_arg info goal,
                          List.map (term_add_tactic_arg info) subgoals)

   and term_make_wrapped_header info t =
      let label, ext = two_subterms t in
         HeadWrapped (term_add_arglist info label,
                      term_add_extract info ext)

   and term_make_compose_header info t =
      let goal, subgoals, extras = three_subterms t in
      let subgoals = dest_xlist subgoals in
      let extras = dest_xlist extras in
         HeadCompose (term_add_extract info goal,
                      List.map (term_add_extract info) subgoals,
                      List.map (term_add_extract info) extras)

   and term_make_rule_header info t =
      let text = dest_string_param t in
      let status, goal, subgoals, extras = four_subterms t in
      let subgoals = dest_xlist subgoals in
      let extras = dest_xlist extras in
         HeadRule (LazyStatusDelayed,
                   text,
                   term_add_extract info goal,
                   List.map (term_add_extract info) subgoals,
                   List.map (term_add_extract info) extras)

   and term_make_extract_header info t =
      let op = opname_of_term t in
         if Opname.eq op goal_op then
            term_make_goal_header info t
         else if Opname.eq op identity_op then
            term_make_identity_header info t
         else if Opname.eq op annotate_op then
            term_make_annotate_header info t
         else if Opname.eq op unjustified_op then
            term_make_unjustified_header info t
         else if Opname.eq op wrapped_op then
            term_make_wrapped_header info t
         else if Opname.eq op compose_op then
            term_make_compose_header info t
         else if Opname.eq op rule_op then
            term_make_rule_header info t
         else
            raise (RefineError ("Proof_boot.term_make_extract_header", StringError "ill-formed proof"))

   (*
    * Make the memo table.
    * Keep the table around forever.
    *)
   let ext_memo =
      { attribute = WeakMemo.create_default "Proof_term_boot.ext_memo.attribute" (**)
           weaken_attribute_header compare_attribute ext_make_attribute;
        attributes = WeakMemo.create_default "Proof_term_boot.ext_memo.attributes" (**)
           weaken_attributes_header compare_attributes ext_make_attributes;
        arglist = WeakMemo.create_default "Proof_term_boot.ext_memo.arglist" (**)
           weaken_arglist_header compare_arglist ext_make_arglist;
        tactic_arg = WeakMemo.create_default "Proof_term_boot.ext_memo.tactic_arg" (**)
           weaken_tactic_arg_header compare_tactic_arg ext_make_tactic_arg;
        extract = WeakMemo.create_default "Proof_term_boot.ext_memo.extract" (**)
           weaken_extract_header compare_extract ext_make_extract
      }

   let term_memo =
      { attribute = WeakMemo.create_default "Proof_term_boot.term_memo.attribute" (**)
           weaken_attribute_header compare_attribute term_make_attribute;
        attributes = WeakMemo.create_default "Proof_term_boot.term_memo.attributes" (**)
           weaken_attributes_header compare_attributes term_make_attributes;
        arglist = WeakMemo.create_default "Proof_term_boot.term_memo.arglist" (**)
           weaken_arglist_header compare_arglist term_make_arglist;
        tactic_arg = WeakMemo.create_default "Proof_term_boot.term_memo.tactic_arg" (**)
           weaken_tactic_arg_header compare_tactic_arg term_make_tactic_arg;
        extract = WeakMemo.create_default "Proof_term_boot.term_memo.extract" (**)
           weaken_extract_header compare_extract term_make_extract
      }

   (*
    * Build the term for the proof.
    *)
   let to_term parse_expr parse_tactic goal node =
      let args =
         { sentinal = goal.ref_sentinal;
           raw_attributes = Tactic.raw_attributes goal;
           bookmark = goal.ref_bookmark;
           parse_expr = parse_expr;
           parse_tactic = parse_tactic
         }
      in
      let info = args, term_memo in
      let index = ext_add_extract info node in
         term_retrieve_extract info index

   let of_term args sentinal bookmark parse_expr parse_tactic term =
      let args =
         { sentinal = sentinal;
           raw_attributes = args;
           bookmark = bookmark;
           parse_expr = parse_expr;
           parse_tactic = parse_tactic
         }
      in
      let info = args, ext_memo in
      let index = term_add_extract info term in
         ext_retrieve_extract info index

   (*
    * Some operations are allowed on the terms.
    *)
   let status_of_term t =
      let opname = opname_of_term t in
         if Opname.eq opname rule_op then
            let status, _, _, _ = four_subterms t in
               dest_status status
         else
            LazyStatusDelayed

   let node_count_of_term t =
      let rec collect (rcount, ncount) t =
         let opname = opname_of_term t in
            if Opname.eq opname rule_op then
               let _, _, subgoals, _ = four_subterms t in
               let subgoals = dest_xlist subgoals in
                  List.fold_left collect (succ rcount, succ ncount) subgoals
            else
               rcount, succ ncount
      in
         collect (0, 0) t
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
