(*
 * Define a common tactic type.
 *
 * We build tactics as a layer over the refiner,
 * and the tactics are summarized using Tactic_cache.extract.
 *
 * Eventually, it would be desirable to have tactics just
 * manipulate the Tactic_cache.extract, and perform all
 * search outside the refiner.  Then once the search is
 * complete, the extract would be generated by the refiner.
 *
 * For now, this is too hard.  We use the refiner to guide the
 * search, and we keep the extract up-to-date with the
 * current refinement.  This allows is to use chaining while
 * retaining the traditional search mechanisms.
 *
 * A tactic has two parts:
 *    1. It contains a Refine.tactic
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

open Mp_debug
open Printf
open Thread_util
open Rformat
open Dform

open Opname
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermSubst
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine

open Theory
open Term_eq_table

open Tactic_boot_sig

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Tactic_type%t"

let debug_tactic =
   create_debug (**)
      { debug_name = "tactic";
        debug_description = "display primitive tactic operations";
        debug_value = false
      }

let debug_refine = load_debug "refine"

(*
 * This module implements:
 *   TacticType
 *   Tactic
 *   TacticInternal
 *)
module Tactic =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   module ThreadRefinerTacticals = Thread_refiner.ThreadRefinerTacticals
   module ThreadRefiner = Thread_refiner.ThreadRefiner

   (*
    * Functions needed for the table.
    *)
   let union () () = ()

   let append l1 l2 = l1 @ l2

   (*
    * these are the different types of normal arguments
    * we can pass with the tactic.
    *)
   type attribute =
      TermArg of term
    | TypeArg of term
    | IntArg of int
    | BoolArg of bool
    | StringArg of string
    | SubstArg of term
    | TermListArg of term list

   type attributes = (string * attribute) list

   (*
    * For efficiency, we expand a few attribute lists,
    * but we retain a general argument description.
    *)
   type arglist =
      NoneArgList               of string
    | IntArgList                of string * int
    | BoolArgList               of string * bool
    | StringArgList             of string * string
    | TermArgList               of string * term
    | IntIntArgList             of string * int * int
    | IntBoolArgList            of string * int * bool
    | IntStringArgList          of string * int * string
    | IntTermArgList            of string * int * term
    | BoolIntArgList            of string * bool * int
    | BoolBoolArgList           of string * bool * bool
    | BoolStringArgList         of string * bool * string
    | BoolTermArgList           of string * bool * term
    | StringIntArgList          of string * string * int
    | StringBoolArgList         of string * string * bool
    | StringStringArgList       of string * string * string
    | StringTermArgList         of string * string * term
    | TermIntArgList            of string * term * int
    | TermBoolArgList           of string * term * bool
    | TermStringArgList         of string * term * string
    | TermTermArgList           of string * term * term
    | GeneralArgList            of attribute array

   (*
    * The attribute calculations are delayed to minimize communication
    * cost.  The tactic_arg uses keys to distribute the attributes.
    * The values are stored in keys.
    *)
   type shared_object =
      ShareConv of conv
    | ShareTactic of tactic
    | ShareIntTactic of (int -> tactic)
    | ShareArgTactic of (tactic_arg -> tactic)
    | ShareTSubst of typeinf_subst_fun
    | ShareTypeinf of typeinf_func
    | ShareSentinal of Refine.sentinal

   and shared_key = shared_object ThreadRefiner.key

   and sentinal = shared_key

   and raw_attribute_info =
      RawTerm of term
    | RawTermList of term list
    | RawType of term
    | RawInt of int
    | RawBool of bool
    | RawString of string
    | RawSubst of term
    | RawObject of shared_key

   and raw_attribute = string * raw_attribute_info

   and raw_attributes = raw_attribute list

   (*
    * Attributes are user-defined arguments that are
    * threaded through the proof tree.
    *)
   and attribute_info =
      { attr_terms      : (string * term) list;
        attr_term_lists : (string * term list) list;
        attr_types      : (string * term) list;
        attr_ints       : (string * int) list;
        attr_bools      : (string * bool) list;
        attr_strings    : (string * string) list;
        attr_subst      : (string * term) list;
        attr_keys       : (string * shared_key) list
      }

   (*
    * A tactic argument includes the msequent goal,
    * as well as the attributes.
    *)
   and tactic_arg =
      { ref_goal : msequent;
        ref_label : string;
        mutable ref_parent : tactic_parent;
        ref_attributes : attribute_info;
        ref_sentinal : sentinal
      }

   and tactic_parent =
      ParentNone
    | ParentLazy of tactic_arg
    | ParentSet of tactic_arg * parents

   and parents = (unit, tactic_arg) Term_eq_table.msequent_table

   and tactic_value = (tactic_arg, arglist, extract) ThreadRefiner.t

   and tactic = tactic_arg -> tactic_value

   and pre_tactic = (tactic_arg -> Refiner.Refiner.Refine.msequent list -> tactic_arg list) * prim_tactic

   (*
    * An extract has these kinds:
    *   + A goal term without any justification
    *   + A step that is unjustified
    *   + A raw refine extract, saving the number of subgoals
    *   + A composition of extracts
    *   + An annotated extract
    *   + A rule box, which is a combined annotation/composition
    *)
   and extract =
      Goal of tactic_arg
    | Unjustified of tactic_arg * tactic_arg list
    | Extract of tactic_arg * tactic_arg list * Refine.extract
    | ExtractRewrite of tactic_arg * tactic_arg * address * Refine.rw_extract
    | ExtractCondRewrite of tactic_arg * tactic_arg list * address * Refine.crw_extract
    | ExtractNthHyp of tactic_arg * int
    | ExtractCut of tactic_arg * term * tactic_arg * tactic_arg
    | Wrapped of arglist * extract
    | Compose of compose_info
    | RuleBox of rule_info
    | Pending of pending_extract
    | Locked of extract
    | Identity of tactic_arg

   and pending_extract = unit -> extract

   and compose_info =
      { mutable comp_status : lazy_status;
        comp_goal : extract;
        comp_subgoals : extract list;
        mutable comp_leaves : lazy_leaves;
        comp_extras : extract list
      }

   and rule_info =
      { mutable rule_status : lazy_status;
        rule_string : string;
        rule_expr : (unit -> MLast.expr);
        rule_tactic : (unit -> tactic);
        mutable rule_extract_normalized : bool;
        mutable rule_extract : extract;
        rule_subgoals : extract list;
        mutable rule_leaves : lazy_leaves;
        rule_extras : extract list
      }

   and lazy_status =
      LazyStatusDelayed
    | LazyStatusBad
    | LazyStatusIncomplete
    | LazyStatusPartial
    | LazyStatusComplete

   and lazy_leaves =
      LazyLeavesDelayed
    | LazyLeaves of tactic_arg list

   (*
    * Conversions are used by the rewrite module.
    *)
   and env = tactic_arg * address

   and conv =
      RewriteConv of rw
    | CondRewriteConv of cond_rewrite
    | ComposeConv of conv Flist.t
    | ChooseConv of conv Flist.t
    | AddressConv of address * conv
    | ClauseConv of int * conv
    | FoldConv of term * conv
    | CutConv of term
    | FunConv of (env -> conv)
    | HigherConv of conv
    | IdentityConv

   (*
    * Parent table.
    *)
   module ParentBase =
   struct
      type set = unit
      type data = tactic_arg

      let union () () =
         ()

      let append data1 data2 =
         data1 @ data2
   end

   module ParentTable = Term_eq_table.MakeMsequentTable (ParentBase)

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Server is created at file execution time.
    *)
   let print_tactic_arg out arg =
      let goal = TermMan.nth_concl (msequent_goal arg.ref_goal) 0 in
         debug_print out goal

   (*
    * Composition function on extracts.
    *)
   let compose ext extl =
      Compose { comp_status = LazyStatusDelayed;
                comp_goal = ext;
                comp_subgoals = extl;
                comp_leaves = LazyLeavesDelayed;
                comp_extras = []
      }

   let wrap arglist ext =
      Wrapped (arglist, ext)

   let args = ThreadRefiner.args

   let remote_server = (* Register.set 0 *) (ThreadRefiner.create print_tactic_arg compose wrap)

   let get_remote_server () =
      (* Register.get *) remote_server

   let format_alist1 buf ffunc (s,data) =
      format_string buf ";";
      format_space buf;
      format_string buf (s ^ "->");
      ffunc buf data

   let format_alist name buf ffunc = function
      [] -> ()
    | [s,data] ->
         format_space buf;
         format_pushm buf 2;
         format_string buf (name ^ " =");
         format_space buf;
         format_string buf ("[ " ^ s ^"->");
         ffunc buf data;
         format_string buf " ];";
         format_popm buf
    | (s,data)::rest -> 
         format_space buf;
         format_pushm buf 2;
         format_string buf (name ^ " =");
         format_space buf;
         format_pushm buf 2;
         format_string buf ("[ " ^ s ^"->");
         ffunc buf data;
         List.iter (format_alist1 buf ffunc) rest;
         format_popm buf;
         format_space buf;
         format_string buf "];";
         format_popm buf

   let format_term1 db buf t =
      format_string buf ";";
      format_space buf;
      format_term db buf t

   let format_tlist db buf = function
      [] -> format_string buf "[]"
    | [t] ->
         format_string buf "[ ";
         format_term db buf t;
         format_string buf " ]"
    | t::rest -> 
         format_pushm buf 2;
         format_string buf "[ ";
         format_term db buf t;
         List.iter (format_term1 db buf) rest;
         format_popm buf;
         format_space buf;
         format_string buf "]"

   let format_bool buf b =
      let s = if b then "tt" else "ff" in
      format_string buf s

   let format_attrs db buf attrs =
      format_pushm buf 2;
      format_string buf "<";
      format_alist "terms" buf (format_term db) attrs.attr_terms;
      format_alist "term_lists" buf (format_tlist db) attrs.attr_term_lists;
      format_alist "types" buf (format_term db) attrs.attr_types;
      format_alist "ints" buf format_int attrs.attr_ints;
      format_alist "bools" buf format_bool attrs.attr_bools;
      format_alist "strings" buf format_string attrs.attr_strings;
      format_alist "substs" buf (format_term db) attrs.attr_subst;
      format_popm buf;
      format_space buf;
      format_string buf ">"

   let empty_attribute = 
      { attr_terms      = [];
        attr_term_lists = [];
        attr_types      = [];
        attr_ints       = [];
        attr_bools      = [];
        attr_strings    = [];
        attr_subst      = [];
        attr_keys       = []
      }

   (*
    * Create an initial tactic_arg for a proof.
    * Cache is initially out-of-date.  It will be
    * set to the current goal when requested.
    *)
   let attribute_info_of_raw_attributes attributes =
      { attr_terms      = List_util.some_map (function (name, RawTerm t)     -> Some (name, t) | _ -> None) attributes;
        attr_term_lists = List_util.some_map (function (name, RawTermList t) -> Some (name, t) | _ -> None) attributes;
        attr_types      = List_util.some_map (function (name, RawType t)     -> Some (name, t) | _ -> None) attributes;
        attr_ints       = List_util.some_map (function (name, RawInt i)      -> Some (name, i) | _ -> None) attributes;
        attr_bools      = List_util.some_map (function (name, RawBool b)     -> Some (name, b) | _ -> None) attributes;
        attr_strings    = List_util.some_map (function (name, RawString s)   -> Some (name, s) | _ -> None) attributes;
        attr_subst      = List_util.some_map (function (name, RawSubst t)    -> Some (name, t) | _ -> None) attributes;
        attr_keys       = List_util.some_map (function (name, RawObject k)   -> Some (name, k) | _ -> None) attributes
      }

   let squash_attributes attrs =
      { attrs with attr_keys = [] }

   let update_attributes attrs raws =
      { attrs with attr_keys = List_util.some_map (function (name, RawObject k)   -> Some (name, k) | _ -> None) raws }

   let create sentinal label goal attributes =
      { ref_goal = goal;
        ref_label = label;
        ref_parent = ParentNone;
        ref_attributes = attribute_info_of_raw_attributes attributes;
        ref_sentinal = sentinal
      }

   let main_loop () =
      ThreadRefiner.main_loop (get_remote_server ())

   (*
    * Access to the sequent.
    *)
   let msequent arg = arg.ref_goal

   let goal arg =
      msequent_goal arg.ref_goal

   let nth_hyp arg i =
      TermMan.nth_hyp (msequent_goal arg.ref_goal) i

   let nth_concl arg i =
      TermMan.nth_concl (msequent_goal arg.ref_goal) i

   let label arg = arg.ref_label

   (*
    * Modify the argument.
    *)
   let set_goal arg goal =
      { arg with ref_goal = mk_msequent goal (snd (dest_msequent arg.ref_goal)) }

   let set_concl arg concl =
      let goal, hyps = dest_msequent arg.ref_goal in
         { arg with ref_goal = mk_msequent (replace_goal goal concl) hyps }

   let set_label arg label =
      { arg with ref_label = label }

   (************************************************************************
    * SENTINAL                                                             *
    ************************************************************************)

   (*
    * Sentinal function is lazy.
    *)
   let get_theory name =
      let rec search = function
         thy :: t ->
            if thy.thy_name = name then
               thy
            else
               search t
       | [] ->
            raise (RefineError ("get_theory", StringStringError ("theory is not found", name)))
      in
         search (get_theories ())

   let null_sentinal =
      let xlazy () =
         ShareSentinal (Refine.null_sentinal)
      in
         ThreadRefiner.share (get_remote_server ()) "sentinal" xlazy

   let sentinal_of_refiner mod_name =
      let xlazy () =
         let refiner = (get_theory mod_name).thy_refiner in
            ShareSentinal (Refine.sentinal_of_refiner refiner)
      in
         ThreadRefiner.share (get_remote_server ()) "sentinal" xlazy

   let sentinal_of_refiner_object mod_name name =
      let xlazy () =
         let refiner = (get_theory mod_name).thy_refiner in
         let opname = make_opname [name; mod_name] in
         let refiner =
            try snd (dest_refiner (find_refiner refiner opname)) with
               Not_found ->
                  eprintf "Warning: using default refiner for %s%t" name eflush;
                  refiner
         in
            ShareSentinal (Refine.sentinal_of_refiner refiner)
      in
         ThreadRefiner.share (get_remote_server ()) "sentinal_object" xlazy

   let get_sentinal key =
      match ThreadRefiner.arg_of_key (get_remote_server ()) key with
         ShareSentinal sent ->
            sent
       | _ ->
            raise (Failure "Thread_refiner.get_sentinal")

   (************************************************************************
    * ATTRIBUTES                                                           *
    ************************************************************************)

   (*
    * Arglists.
    * This compression/decompression is inefficient (perhaps the
    * OCaml compiler can do pattern optimization?)  But this code
    * is only used in proof conversion.
    *)
   let compress_arglist = function
      [StringArg a1] ->
         NoneArgList a1
    | [StringArg a1; IntArg a2] ->
         IntArgList (a1, a2)
    | [StringArg a1; BoolArg a2] ->
         BoolArgList (a1, a2)
    | [StringArg a1; StringArg a2] ->
         StringArgList (a1, a2)
    | [StringArg a1; TermArg a2] ->
         TermArgList (a1, a2)
    | [StringArg a1; IntArg a2; IntArg a3] ->
         IntIntArgList (a1, a2, a3)
    | [StringArg a1; IntArg a2; BoolArg a3] ->
         IntBoolArgList (a1, a2, a3)
    | [StringArg a1; IntArg a2; StringArg a3] ->
         IntStringArgList (a1, a2, a3)
    | [StringArg a1; IntArg a2; TermArg a3] ->
         IntTermArgList (a1, a2, a3)
    | [StringArg a1; BoolArg a2; IntArg a3] ->
         BoolIntArgList (a1, a2, a3)
    | [StringArg a1; BoolArg a2; BoolArg a3] ->
         BoolBoolArgList (a1, a2, a3)
    | [StringArg a1; BoolArg a2; StringArg a3] ->
         BoolStringArgList (a1, a2, a3)
    | [StringArg a1; BoolArg a2; TermArg a3] ->
         BoolTermArgList (a1, a2, a3)
    | [StringArg a1; StringArg a2; IntArg a3] ->
         StringIntArgList (a1, a2, a3)
    | [StringArg a1; StringArg a2; BoolArg a3] ->
         StringBoolArgList (a1, a2, a3)
    | [StringArg a1; StringArg a2; StringArg a3] ->
         StringStringArgList (a1, a2, a3)
    | [StringArg a1; StringArg a2; TermArg a3] ->
         StringTermArgList (a1, a2, a3)
    | [StringArg a1; TermArg a2; IntArg a3] ->
         TermIntArgList (a1, a2, a3)
    | [StringArg a1; TermArg a2; BoolArg a3] ->
         TermBoolArgList (a1, a2, a3)
    | [StringArg a1; TermArg a2; StringArg a3] ->
         TermStringArgList (a1, a2, a3)
    | [StringArg a1; TermArg a2; TermArg a3] ->
         TermTermArgList (a1, a2, a3)
    | args ->
         GeneralArgList (Array.of_list args)

   let expand_arglist = function
      NoneArgList a1 ->
         [StringArg a1]
    | IntArgList (a1, a2) ->
         [StringArg a1; IntArg a2]
    | BoolArgList (a1, a2) ->
         [StringArg a1; BoolArg a2]
    | StringArgList (a1, a2) ->
         [StringArg a1; StringArg a2]
    | TermArgList (a1, a2) ->
         [StringArg a1; TermArg a2]
    | IntIntArgList (a1, a2, a3) ->
         [StringArg a1; IntArg a2; IntArg a3]
    | IntBoolArgList (a1, a2, a3) ->
         [StringArg a1; IntArg a2; BoolArg a3]
    | IntStringArgList (a1, a2, a3) ->
         [StringArg a1; IntArg a2; StringArg a3]
    | IntTermArgList (a1, a2, a3) ->
         [StringArg a1; IntArg a2; TermArg a3]
    | BoolIntArgList (a1, a2, a3) ->
         [StringArg a1; BoolArg a2; IntArg a3]
    | BoolBoolArgList (a1, a2, a3) ->
         [StringArg a1; BoolArg a2; BoolArg a3]
    | BoolStringArgList (a1, a2, a3) ->
         [StringArg a1; BoolArg a2; StringArg a3]
    | BoolTermArgList (a1, a2, a3) ->
         [StringArg a1; BoolArg a2; TermArg a3]
    | StringIntArgList (a1, a2, a3) ->
         [StringArg a1; StringArg a2; IntArg a3]
    | StringBoolArgList (a1, a2, a3) ->
         [StringArg a1; StringArg a2; BoolArg a3]
    | StringStringArgList (a1, a2, a3) ->
         [StringArg a1; StringArg a2; StringArg a3]
    | StringTermArgList (a1, a2, a3) ->
         [StringArg a1; StringArg a2; TermArg a3]
    | TermIntArgList (a1, a2, a3) ->
         [StringArg a1; TermArg a2; IntArg a3]
    | TermBoolArgList (a1, a2, a3) ->
         [StringArg a1; TermArg a2; BoolArg a3]
    | TermStringArgList (a1, a2, a3) ->
         [StringArg a1; TermArg a2; StringArg a3]
    | TermTermArgList (a1, a2, a3) ->
         [StringArg a1; TermArg a2; TermArg a3]
    | GeneralArgList args ->
         Array.to_list args

   (*
    * List all the normal attributes.
    *)
   let attributes { ref_attributes = { attr_terms = terms;
                                       attr_term_lists = term_lists;
                                       attr_types = types;
                                       attr_ints = ints;
                                       attr_bools = bools;
                                       attr_subst = subst
                                     } } =
      (List.map (fun (name, t) -> name, TermArg t) terms)
      @ (List.map (fun (name, t) -> name, TermListArg t) term_lists)
      @ (List.map (fun (name, t) -> name, TypeArg t) types)
      @ (List.map (fun (name, i) -> name, IntArg i) ints)
      @ (List.map (fun (name, b) -> name, BoolArg b) bools)
      @ (List.map (fun (name, t) -> name, SubstArg t) subst)

   let raw_attributes { ref_attributes = { attr_terms = terms;
                                           attr_term_lists = term_lists;
                                           attr_types = types;
                                           attr_ints = ints;
                                           attr_bools = bools;
                                           attr_subst = subst;
                                           attr_keys = keys
                                     } } =
      (List.map (fun (name, t) -> name, RawTerm t) terms)
      @ (List.map (fun (name, t) -> name, RawTermList t) term_lists)
      @ (List.map (fun (name, t) -> name, RawType t) types)
      @ (List.map (fun (name, i) -> name, RawInt i) ints)
      @ (List.map (fun (name, b) -> name, RawBool b) bools)
      @ (List.map (fun (name, t) -> name, RawSubst t) subst)
      @ (List.map (fun (name, t) -> name, RawObject t) keys)

   (*
    * Lazy attribute generation for keys.
    *)
   let term_attribute name t =
      name, RawTerm t

   let term_list_attribute name tl =
      name, RawTermList tl

   let type_attribute name t =
      name, RawType t

   let int_attribute name i =
      name, RawInt i

   let bool_attribute name flag =
      name, RawBool flag

   let string_attribute name s =
      name, RawString s

   let subst_attribute name t =
      name, RawSubst t

   let conv_attribute name f =
      name, RawObject (ThreadRefiner.share (get_remote_server ()) name (fun () -> ShareConv (f ())))

   let tactic_attribute name f =
      name, RawObject (ThreadRefiner.share (get_remote_server ()) name (fun () -> ShareTactic (f ())))

   let int_tactic_attribute name f =
      name, RawObject (ThreadRefiner.share (get_remote_server ()) name (fun () -> ShareIntTactic (f ())))

   let arg_tactic_attribute name f =
      name, RawObject (ThreadRefiner.share (get_remote_server ()) name (fun () -> ShareArgTactic (f ())))

   let tsubst_attribute name f =
      name, RawObject (ThreadRefiner.share (get_remote_server ()) name (fun () -> ShareTSubst (f ())))

   let typeinf_attribute name f =
      name, RawObject (ThreadRefiner.share (get_remote_server ()) name (fun () -> ShareTypeinf (f ())))

   (*
    * Fetch the attributes.
    *)
   let rec assoc name = function
      (name', h) :: t ->
         if name = name' then
            h
         else
            assoc name t
    | [] ->
         raise (RefineError ("get_attribute", StringStringError ("attribute not found", name)))

   let get_term { ref_attributes = { attr_terms = terms } } name =
      assoc name terms

   let get_term_list { ref_attributes = { attr_term_lists = term_lists } } name =
      assoc name term_lists

   let get_type { ref_attributes = { attr_types = types } } name =
      assoc name types

   let get_int { ref_attributes = { attr_ints = ints } } name =
      assoc name ints

   let get_bool { ref_attributes = { attr_bools = bools } } name =
      assoc name bools

   let get_string { ref_attributes = { attr_strings = strings } } name =
      assoc name strings

   let get_subst { ref_attributes = { attr_subst = subst } } =
      subst

   let get_conv { ref_attributes = { attr_keys = keys } } name =
      match ThreadRefiner.arg_of_key (get_remote_server ()) (assoc name keys) with
         ShareConv conv ->
            conv
       | _ ->
            raise (RefineError ("get_conv", StringStringError ("attribute type error", name)))

   let get_tactic { ref_attributes = { attr_keys = keys } } name =
      match ThreadRefiner.arg_of_key (get_remote_server ()) (assoc name keys) with
         ShareTactic tac ->
            tac
       | _ ->
            raise (RefineError ("get_tactic", StringStringError ("attribute type error", name)))

   let get_int_tactic { ref_attributes = { attr_keys = keys } } name =
      match ThreadRefiner.arg_of_key (get_remote_server ()) (assoc name keys) with
         ShareIntTactic tac ->
            tac
       | _ ->
            raise (RefineError ("get_int_tactic", StringStringError ("attribute type error", name)))

   let get_arg_tactic { ref_attributes = { attr_keys = keys } } name =
      match ThreadRefiner.arg_of_key (get_remote_server ()) (assoc name keys) with
         ShareArgTactic tac ->
            tac
       | _ ->
            raise (RefineError ("get_arg_tactic", StringStringError ("attribute type error", name)))

   let get_tsubst { ref_attributes = { attr_keys = keys } } name =
      match ThreadRefiner.arg_of_key (get_remote_server ()) (assoc name keys) with
         ShareTSubst t ->
            t
       | _ ->
            raise (RefineError ("get_tsubst", StringStringError ("attribute type error", name)))

   let get_typeinf { ref_attributes = { attr_keys = keys } } name =
      match ThreadRefiner.arg_of_key (get_remote_server ()) (assoc name keys) with
         ShareTypeinf t ->
            t
       | _ ->
            raise (RefineError ("get_typeinf", StringStringError ("attribute type error", name)))

   (*
    * Two args are equal if their goals are equal.
    * Other arguments are ignored.
    *)
   let tactic_arg_alpha_equal { ref_goal = goal1 } { ref_goal = goal2 } =
      msequent_alpha_equal goal1 goal2

   (************************************************************************
    * REFINEMENT                                                           *
    ************************************************************************)

   (*
    * The refiner just applies the tactic to the arg.
    * We keep a list of values
    *)
   let refine_final_list = ref []

   let add_final_hook f =
      refine_final_list := f :: !refine_final_list

   let refine tac arg =
      refine_final_list := [];
      let x = ThreadRefiner.eval (get_remote_server ()) (tac arg) in
         List_util.rev_iter (fun f -> f ()) !refine_final_list;
         refine_final_list := [];
         x

   (*
    * Utility for reconstructing the subgoals
    * in a tactic application.
    *)
   let make_subgoals labels arg goals =
      let { ref_label = label';
            ref_attributes = attributes;
            ref_sentinal = sentinal
          } = arg
      in
      let parent_lazy = ParentLazy arg in
      let rec collect labels goals =
         match labels, goals with
            label :: lt, goal :: gt ->
               let label =
                  match label with
                     Some label ->
                        label
                   | None ->
                        label'
               in
               let goal =
                  { ref_goal = goal;
                    ref_label = label;
                    ref_parent = parent_lazy;
                    ref_attributes = attributes;
                    ref_sentinal = sentinal
                  }
               in
                  goal :: collect lt gt
          | [], [] ->
               []
          | _ ->
               raise (RefineError ("make_subgoals", StringError ("length mismatch between labels and goals")))
      in
         collect labels goals

   let rec make_labeled_subgoals arg goals =
      let { ref_label = label;
            ref_attributes = attributes;
            ref_sentinal = sentinal
          } = arg
      in
      let parent_lazy = ParentLazy arg in
      let rec collect = function
         goal :: t ->
            let goal', assums = dest_msequent goal in
            let goal =
               if is_xstring_dep0_term goal' then
                  let label, goal' = dest_xstring_dep0_term goal' in
                     { ref_goal = mk_msequent goal' assums;
                       ref_label = label;
                       ref_parent = ParentLazy arg;
                       ref_attributes = attributes;
                       ref_sentinal = sentinal
                     }
               else
                  { ref_goal = goal;
                    ref_label = label;
                    ref_parent = ParentLazy arg;
                    ref_attributes = attributes;
                    ref_sentinal = sentinal
                  }
            in
               goal :: make_labeled_subgoals arg t
       | [] ->
            []
      in
         collect goals

   (*
    * Eventually, we may want to look at the rule and do something
    * special here.
    *)
   let compile_rule refiner labels tac =
      (make_subgoals labels, tac)

   let compile_labeled_rule refiner tac =
      (make_labeled_subgoals, tac)

   (*
    * Construct polymorphic tactic.
    *)
   let tactic_of_rule (make_subgoals, rl) addrs_names params arg =
      let rl = rl addrs_names params in
      let subgoals, ext = Refine.refine (get_sentinal arg.ref_sentinal) rl arg.ref_goal in
      let subgoals = make_subgoals arg subgoals in
         ThreadRefinerTacticals.create_value subgoals (Extract (arg, subgoals, ext))

   (*
    * Construct polymorphic tactic.
    *)
   let tactic_of_refine_tactic labels rl arg =
      let _ =
         if !debug_tactic then
            eprintf "Starting refinement%t" eflush
      in
      let { ref_goal = goal; ref_sentinal = sentinal } = arg in
      let subgoals, ext = Refine.refine (get_sentinal sentinal) rl goal in
      let subgoals = make_subgoals labels arg subgoals in
         if !debug_tactic then
            eprintf "tactic_of_rule done%t" eflush;
         subgoals, Extract (arg, subgoals, ext)

   (*
    * Convert a rewrite into a tactic.
    *)
   let tactic_of_rewrite_exn1 = RefineError ("tactic_of_rewrite", StringError "rewrite did not produce a goal")
   let tactic_of_rewrite_exn2 = RefineError ("tactic_of_rewrite", StringError "rewrite produced too many goals")

   let tactic_of_rewrite i rw arg =
      let rl = rwtactic i rw in
      let { ref_goal = goal;
            ref_label = label;
            ref_attributes = attributes;
            ref_sentinal = sentinal
          } = arg
      in
         match Refine.refine (get_sentinal sentinal) rl goal with
            [subgoal], ext ->
               let subgoal =
                  { ref_goal = subgoal;
                    ref_label = label;
                    ref_parent = ParentLazy arg;
                    ref_attributes = attributes;
                    ref_sentinal = sentinal
                  }
               in
                  ThreadRefinerTacticals.create_value [subgoal] (Extract (arg, [subgoal], ext))
          | [], _ ->
               raise tactic_of_rewrite_exn1
          | _ ->
               raise tactic_of_rewrite_exn2

   (*
    * Convert a conditional rewrite to a tactic.
    *)
   let tactic_of_cond_rewrite i crw arg =
      let rl = crwtactic i crw in
      let { ref_goal = goal;
            ref_label = label;
            ref_attributes = attributes;
            ref_sentinal = sentinal
          } = arg
      in
      let subgoals, ext = Refine.refine (get_sentinal sentinal) rl goal in
      let make_subgoal label goal =
         { ref_goal = goal;
           ref_label = label;
           ref_parent = ParentLazy arg;
           ref_attributes = attributes;
           ref_sentinal = sentinal
         }
      in
      let subgoals =
         match subgoals with
            subgoal :: subgoals ->
               make_subgoal label subgoal :: List.map (make_subgoal "assertion") subgoals
          | [] ->
               raise (Invalid_argument "tactic_of_cond_rewrite: produced no subgoals")
      in
         ThreadRefinerTacticals.create_value subgoals (Extract (arg, subgoals, ext))

   (************************************************************************
    * EXTRACTS                                                             *
    ************************************************************************)

   (*
    * Compose two extracts.
    *)
   let term_of_extract ext =
      raise (Failure "term_of_extract: not implemented")

   let extract_boot_of_extract ext =
      ext

   let extract_of_extract_boot ext =
      ext

   (************************************************************************
    * TACTICALS                                                            *
    ************************************************************************)

   (*
    * Assumption tactic from the refiner.
    * Assumptions are numbered from 1, but
    * refiner numbers them from 0.
    *)
   let nthAssumT i p =
      let i = pred i in
         if !debug_refine then
            begin
               let { ref_goal = seq } = p in
               let goal, hyps = dest_msequent seq in
                  eprintf "Tactic_type.nthAssumT:\nHyp: %d%t" i eflush;
                  List.iter (fun hyp ->
                        print_term stderr hyp;
                        eflush stderr) hyps;
                  eprintf "\nGoal: ";
                  print_term stderr goal;
                  eflush stderr
            end;
         let subgoals, ext = tactic_of_refine_tactic [] (Refine.nth_hyp i) p in
            ThreadRefinerTacticals.create_value subgoals ext

   (*
    * Identity doesn't do anything.
    *)
   let idT p =
      ThreadRefinerTacticals.create_value [p] (Identity p)

   (*
    * Cut rule.
    *)
   let cutT t p =
      let subgoals, ext = tactic_of_refine_tactic [Some "assertion"; Some "main"] (Refine.cut t) p in
         ThreadRefinerTacticals.create_value subgoals ext

   (*
    * Sequencing tactics.
    *)
   let prefix_thenT = ThreadRefinerTacticals.compose1
   let prefix_thenLT = ThreadRefinerTacticals.compose2
   let prefix_thenFLT = ThreadRefinerTacticals.composef
   let firstT = ThreadRefinerTacticals.first
   let wrapT = ThreadRefinerTacticals.wrap
   let prefix_orelseT tac1 tac2 =
      firstT [tac1; tac2]

   (*
    * Modify the label.
    *)
   let setLabelT name p =
      let p = { p with ref_label = name }
      in
         ThreadRefinerTacticals.create_value [p] (Identity p)

   (*
    * Add a term argument.
    *)
   let withT f tac p =
      let attributes = p.ref_attributes in
      let attributes' = f attributes in
      let make_goal p =
         let p' = { p with ref_attributes = attributes' }
         in
            ThreadRefinerTacticals.create_value [p'] (Identity p)
      in
      let make_subgoal p =
         let p = { p with ref_attributes = attributes }
         in
            ThreadRefinerTacticals.create_value [p] (Identity p)
      in
         (prefix_thenT make_goal (prefix_thenT tac make_subgoal)) p

   let withTermT name t =
      withT (fun attr -> { attr with attr_terms = (name, t) :: attr.attr_terms })

   let withTermListT name arg =
      withT (fun attr -> { attr with attr_term_lists = (name, arg) :: attr.attr_term_lists })

   let withTypeT name t =
      withT (fun attr -> { attr with attr_types = (name, t) :: attr.attr_types })

   let withIntT name i =
      withT (fun attr -> { attr with attr_ints = (name, i) :: attr.attr_ints })

   let withBoolT name flag =
      withT (fun attr -> { attr with attr_bools = (name, flag) :: attr.attr_bools })

   let withStringT name s =
      withT (fun attr -> { attr with attr_strings = (name, s) :: attr.attr_strings })

   (*
    * Add some substitutions.
    *)
   let withSubstT subst =
      withT (fun attr -> { attr with attr_subst = subst })

   (*
    * Time the tactic.
    * This shows the time between now and the end of refinement.
    *)
   let finalT f p =
      add_final_hook f;
      idT p

   let timingT tac p =
      let start = Unix.times () in
      let start_time = Unix.gettimeofday () in
      let finalize () =
         let finish = Unix.times () in
         let finish_time = Unix.gettimeofday () in
            eprintf "User time %f; System time %f; Real time %f%t" (**)
               ((finish.Unix.tms_utime +. finish.Unix.tms_cutime)
                -. (start.Unix.tms_utime +. start.Unix.tms_cstime))
               ((finish.Unix.tms_stime +. finish.Unix.tms_cstime)
                -. (start.Unix.tms_stime +. finish.Unix.tms_cstime))
               (finish_time -. start_time)
               eflush
      in
         add_final_hook finalize;
         tac p
end

(*
 * Type definitions.
 *)
module TacticType = Tactic
module TacticInternalType = Tactic

(*
 * Internal functions.
 *)
module TacticInternal = Tactic

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
