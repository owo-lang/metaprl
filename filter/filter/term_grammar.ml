(*
 * Extend the language with a term parser as the default quotation.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 * Modified By: Alexei Kopylov <kopylov@cs.cornell.edu>
 * Modified By: Adam Granicz <granicz@cs.caltech.edu>
 *)

open Printf

open Mp_debug
open Opname
open Term_shape_sig
open Refiner.Refiner
open Term
open TermType
open TermOp
open TermMan
open TermMeta
open TermShape
open RefineError
open Ml_string
open Simple_print
open Simple_print.SimplePrint

open Filter_type
open Filter_summary

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Term_grammar%t"

let debug_grammar =
   create_debug (**)
      { debug_name = "grammar";
        debug_description = "display term parsing operations";
        debug_value = false
      }

let debug_spell =
   create_debug (**)
      { debug_name = "spell";
        debug_description = "check spelling";
        debug_value = false
      }

let _ =
   Grammar.error_verbose:=true;
   Grammar.warning_verbose:=true

(*
 * Terms for representing comments.
 *)
let mk_comment_opname =
   let op = Opname.mk_opname "Comment" Opname.nil_opname in
      fun s -> Opname.mk_opname s op

let comment_white_op = mk_comment_opname "comment_white"
let comment_string_op = mk_comment_opname "comment_string"
let comment_block_op = mk_comment_opname "comment_block"
let comment_term_op = mk_comment_opname "comment_term"
  
let misspelled = ref []
let dict_inited = ref false

let raise_spelling_error () =
   if !misspelled <> [] then begin
      let rec print col word = function
         h :: t ->
            if h = word then
               print col word t
            else
               let len = String.length h in
               let col =
                  if col + len >= 80 then
                     begin
                        eprintf "\n\t%s" h;
                        len + 9
                     end
                  else
                     begin
                        eprintf " %s" h;
                        col + len + 1
                     end
               in
                  print col h t
       | [] ->
            ()
      in
      let l = Sort.list (<) !misspelled in
         misspelled := [];
         eprintf "The following words may be misspelled:";
         print 80 "" l;
         eflush stderr;
         raise (Failure "spelling")
      end

(*
 * Grammars to extend.
 *)
module type TermGrammarSig =
sig
   val mk_opname : MLast.loc -> string list -> shape_param list -> int list -> opname
   val term_eoi : term Grammar.Entry.e
   val term : term Grammar.Entry.e
   val quote_term : quote_term Grammar.Entry.e
   val mterm : meta_term Grammar.Entry.e
   val bmterm : meta_term Grammar.Entry.e
   val singleterm : aterm Grammar.Entry.e
   val applytermlist : (term list) Grammar.Entry.e
   val bound_term : aterm Grammar.Entry.e
   val xdform : term Grammar.Entry.e
end

(*
 * Build the grammar.
 *)
module MakeTermGrammar (TermGrammar : TermGrammarSig) =
struct
   open TermGrammar

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Also meta-terms.
    *)
   type amterm = { mname : string option; mterm : meta_term }
   type vmterm = { vname : term option; vterm : meta_term }

   (*
    * String or term.
    *)
   type string_or_term =
      ST_String of string
    | ST_Term of (term * (int * int))

   (************************************************************************
    * UTILITIES                                                            *
    ************************************************************************)

   let mk_0opname loc name =
      mk_opname loc [name] [] []

   let mk_dep0_opname loc name =
      mk_opname loc [name] [] [0]

   let mk_dep1_opname loc name =
      mk_opname loc [name] [] [1]

   let mk_dep0_dep0_opname loc name =
      mk_opname loc [name] [] [0;0]

   let mk_dep0_dep1_opname loc name =
      mk_opname loc [name] [] [0;1]

   let mk_dep0_dep2_opname loc name =
      mk_opname loc [name] [] [0;2]

   let mk_dep0_dep0_dep0_opname loc name =
      mk_opname loc [name] [] [0;0;0]

   let bterm_arities =
      List.map ( fun bt -> List.length (dest_bterm bt).bvars)

   let mk_bopname loc names params bterms =
      mk_opname loc names (List.map param_type params) (bterm_arities bterms)

   (*
    * For new symbols.
    *)
   let gensym = ref 0

   let mk_gensym () =
      incr gensym;
      "$" ^ (string_of_int !gensym)

   (* Currying *)
   let mk_bterm' (vars, bterm) = mk_bterm vars bterm

   (*
    * Construct an application.
    *)
   let mk_apply_term loc a b =
      mk_dep0_dep0_term (mk_dep0_dep0_opname loc "apply") a b

   let make_application loc terms =
      (* Convert the list to an application *)
      let rec aux x = function
         [] -> x
       | h::t ->
            aux (mk_apply_term loc x h) t
      in
         match terms with
            [] -> raise (Invalid_argument "make_application")
          | h::t -> aux h t

   (*
    * Cast a parameter to a level expression.
    *)
   let rec level_var s =
      if s.[pred (String.length s)] = '\'' then
         incr_level_exp (level_var (String.sub s 0 (pred (String.length s))))
      else
         mk_var_level_exp s

   let cast_level p =
      match dest_param p with
         Number n when Mp_num.is_integer_num n ->
           mk_const_level_exp (Mp_num.int_of_num n)
       | MLevel l -> l
       | MString v -> level_var v
       | _ -> raise (BadParamCast (p, "l"))

   (*
    * Cast to a number.
    *)
   let cast_number p =
      match dest_param p with
         Number _ | MNumber _ -> p
       | MString s -> make_param (MNumber s)
       | _ -> raise (BadParamCast (p, "n"))

   (*
    * Parameter casting.
    *)
   let cast_param p = function
      "n" -> cast_number p
    | "s" ->
         begin
            match dest_param p with
               Number(n) -> make_param (String (Mp_num.string_of_num n))
             | String _ | MString _ -> p
             | _ -> raise (BadParamCast (p, "s"))
         end
    | "t" ->
         begin
            match dest_param p with
               Number(n) -> make_param (Token (Mp_num.string_of_num n))
             | String(s) -> make_param (Token s)
             | MString(v) -> make_param (MToken v)
             | _ -> raise (BadParamCast (p, "t"))
         end

    | "v" ->
         begin
            match dest_param p with
               Number(n) -> make_param (Var (Mp_num.string_of_num n))
             | String(s) -> make_param (Var s)
             | MString(v) -> make_param (MVar v)
             | _ -> raise (BadParamCast (p, "v"))
         end

    | "l" ->
         make_param (MLevel (cast_level p))

    | x -> raise (BadParamCast (p, x))

   (*
    * Constructors.
    *)
   let mk_pair_term loc a b =
      mk_dep0_dep0_term (mk_dep0_dep0_opname loc "pair") a b

   (*
    * Turn a reversed list of terms into a tuple.
    *)
   let make_term loc = function
      ST_String s ->
         mk_term (mk_op (mk_0opname loc s) []) []
    | ST_Term (t, _) ->
         t

   let rec tupelize loc = function
      [h] -> make_term loc h
    | h::t -> mk_pair_term loc (make_term loc h) (tupelize loc t)
    | [] -> raise (Invalid_argument "tupelize")

   (*
    * Construct a binary term, with a possible dependency.
    *)
   let mk_type_term loc name t1 t2 =
      match t1 with
         { aname = None; aterm = t } ->
            { aname = None; aterm = mk_dep0_dep0_term (mk_dep0_dep0_opname loc name) t t2.aterm }
       | { aname = Some name'; aterm = t } ->
            { aname = None; aterm = mk_dep0_dep1_term (mk_dep0_dep1_opname loc name) (dest_var name') t t2.aterm }

   let mk_arith_term loc name t1 t2 =
      { aname = None; aterm = mk_dep0_dep0_term (mk_dep0_dep0_opname loc name) t1.aterm t2.aterm }


   (*
    * Make record terms
    *)

   let mk_field_term loc r field =
                mk_term (mk_op (mk_opname loc ["field"] [ShapeToken] [0])
                               [make_param (Token field)])  [mk_simple_bterm r]

   let mk_field_self_term loc field =  mk_field_term loc (mk_var_term "self") field


   (*
    * Check that all are strings.
    *)
   let check_bvars l =
      let check = function
         ST_Term (_, loc) ->
            Stdpp.raise_with_loc loc (ParseError "Not a binding var")
       | ST_String s ->
            s
      in
         List.map check l

   (************************************************************************
    * QUOTATIONS                                                           *
    ************************************************************************)
   
   let parse_term s =
      let cs = Stream.of_string s in
         Grammar.Entry.parse TermGrammar.term_eoi cs
   
   let pho_grammar_filename =
      try
         Sys.getenv "LANG_FILE"
      with
         Not_found ->
            !Phobos_state.mp_grammar_filename
   
   let pho_desc_grammar_filename =
      try
         Sys.getenv "DESC_LANG_FILE"
      with
         Not_found ->
            !Phobos_state.mp_desc_grammar_filename
   
   let dest_quot quot =
      try
         let i = String.index quot ':' in
            String.sub quot 0 i, String.sub quot (i+1) (String.length quot-i-1)
      with
         Not_found ->
            "term", quot
   
   (*
    * Parse a comment string.
    *)
   type spelling =
      SpellOff
    | SpellOn
    | SpellAdd
   
   let fake_arities =
      List.map ( fun _ -> 0)
   
   let string_params =
      List.map ( fun _ -> ShapeString)

   let rec parse_quotation loc curr = function
      nm, _ when nm = curr ->
         Stdpp.raise_with_loc loc (Failure (nm ^ " quotation inside a " ^ curr ^ " quotation"))
    | "ext", s ->
         Phobos_exn.catch (Phobos_compile.term_of_string [] pho_grammar_filename) s
    | "desc", s ->
         Phobos_exn.catch (Phobos_compile.term_of_string [] pho_desc_grammar_filename) s
    | ("term" | ""), s -> begin
         try parse_term s
         with Stdpp.Exc_located ((l1, l2), exn) ->
            let offset = fst(loc) in
            Stdpp.raise_with_loc (offset+l1, offset+l2) exn
      end
    | "doc", s ->
         parse_comment loc s
    | nm, _ ->
         Stdpp.raise_with_loc loc (Invalid_argument ("Camlp4 term grammar: unknown " ^ nm ^ " quotation"))
   
   and parse_comment (loc, _) s =
      if !debug_spell && not(!dict_inited) then begin
         Filter_spell.init ();
         dict_inited:=true
      end;
      (*
       * Convert the result of the Comment_parse.
       *)
      let rec build_comment_term spelling = function
         Comment_parse.White ->
            mk_simple_term comment_white_op []
       | Comment_parse.String s ->
            if !debug_spell then
               begin
                  match spelling with
                     SpellOff ->
                        ()
                   | SpellAdd ->
                        Filter_spell.add s
                   | SpellOn ->
                        if not (Filter_spell.check s) then
                           misspelled := s :: !misspelled
               end;
            mk_string_term comment_string_op s
       | Comment_parse.Term ((opname, (l1, l2)), params, args) ->
            let spelling =
               if !debug_spell then
                  match opname with
                     ["spelling"] ->
                        SpellAdd
                   | ["misspelled"]
                   | ["math_misspelled"]
                   | ["license"]
                   | ["url"]
                   | ["comment"] ->
                        SpellOff
                   | _ ->
                        spelling
               else
                  spelling
            in
            let opname =
               mk_opname (loc+l1, loc+l2) opname (string_params params) (fake_arities args)
            in let params = List.map (fun s -> make_param (String s)) params in
            let args = List.map (fun t -> mk_simple_bterm (build_term spelling t)) args in
            let op = mk_op opname params in
            let t = mk_term op args in
               mk_simple_term comment_term_op [t]
       | Comment_parse.Block items ->
            mk_simple_term comment_block_op [build_term spelling items]
       | Comment_parse.Quote ((l1,l2), tag, s) ->
            mk_simple_term comment_term_op [parse_quotation (loc+l1, loc+l2) "doc" (tag,s)]
   
      and build_term spelling tl =
         mk_xlist_term (List.map (build_comment_term spelling) tl)
      in
      let items =
         try Comment_parse.parse s with
            Comment_parse.Parse_error (s, (l1, l2)) ->
               Stdpp.raise_with_loc (loc + l1, loc + l2) (ParseError s)
      in
         mk_simple_term comment_term_op [build_term SpellOn items]

   let mk_comment_term tl =
      let mk_comment t =
         mk_simple_term comment_term_op [t]
      in
         mk_comment (mk_xlist_term (List.map mk_comment tl))

   let convert_comment loc t =
      if is_string_term comment_string_op t then
         parse_comment loc (dest_string_term comment_string_op t)
      else t
   
   (************************************************************************
    * GRAMMAR                                                              *
    ************************************************************************)

   EXTEND
      GLOBAL: term_eoi term quote_term mterm bmterm singleterm applytermlist bound_term xdform;

      (*
       * Meta-terms include meta arrows.
       *)
      mterm:
         [[ t = amterm ->
             t.vterm
          ]];

      bmterm:
         [[ t = bbmterm ->
             t.vterm
          ]];

      amterm:
         [[ t = singleterm ->
             match t with
                { aname = None; aterm = t } ->
                   { vname = None; vterm = MetaTheorem t }
              | { aname = Some n; aterm = t } ->
                   { vname = Some n; vterm = MetaTheorem t }
          ]
          | [ sl_open_brack; name = word_or_string; sl_close_brack; t = singleterm ->
              match t with
                 { aname = None; aterm = t } ->
                   { vname = None; vterm = MetaLabeled (name, MetaTheorem t) }
               | { aname = Some n; aterm = t } ->
                   { vname = Some n; vterm = MetaLabeled (name, MetaTheorem t) }
            ]
          | "meta_implies" LEFTA
            [ t1 = amterm; sl_meta_right_arrow; t2 = amterm ->
               begin
                  match t1 with
                     { vname = None; vterm = t } ->
                        { vname = None; vterm = MetaImplies (t, t2.vterm) }
                   | { vname = Some n; vterm = t } ->
                        { vname = None; vterm = MetaFunction (n, t, t2.vterm) }
               end
            ]
          | "meta_rev_implies" RIGHTA
            [ t2 = amterm; sl_meta_left_arrow; t1 = amterm ->
               begin
                  match t1 with
                     { vname = None; vterm = t } ->
                        { vname = None; vterm = MetaImplies (t, t2.vterm) }
                   | { vname = Some n; vterm = t } ->
                        { vname = None; vterm = MetaFunction (n, t, t2.vterm) }
               end
            ]
          | "meta_iff" LEFTA
            [ t1 = amterm; sl_meta_left_right_arrow; t2 = amterm ->
               { vname = None; vterm = MetaIff (t1.vterm, t2.vterm) }
            ]
         ];

      bbmterm:
         [[ t = bound_term ->
             match t with
                { aname = None; aterm = t } ->
                   { vname = None; vterm = MetaTheorem t }
              | { aname = Some n; aterm = t } ->
                   { vname = Some n; vterm = MetaTheorem t }
          ]
          | [ sl_open_brack; name = word_or_string; sl_close_brack; t = bound_term ->
              match t with
                 { aname = None; aterm = t } ->
                   { vname = None; vterm = MetaLabeled (name, MetaTheorem t) }
               | { aname = Some n; aterm = t } ->
                   { vname = Some n; vterm = MetaLabeled (name, MetaTheorem t) }
            ]
          | "meta_implies" LEFTA
            [ t1 = bbmterm; sl_meta_right_arrow; t2 = bbmterm ->
               begin
                  match t1 with
                     { vname = None; vterm = t } ->
                        { vname = None; vterm = MetaImplies (t, t2.vterm) }
                   | { vname = Some n; vterm = t } ->
                        { vname = None; vterm = MetaFunction (n, t, t2.vterm) }
               end
            ]
          | "meta_rev_implies" RIGHTA
            [ t2 = bbmterm; sl_meta_left_arrow; t1 = bbmterm ->
               begin
                  match t1 with
                     { vname = None; vterm = t } ->
                        { vname = None; vterm = MetaImplies (t, t2.vterm) }
                   | { vname = Some n; vterm = t } ->
                        { vname = None; vterm = MetaFunction (n, t, t2.vterm) }
               end
            ]
          | "meta_iff" LEFTA
            [ t1 = bbmterm; sl_meta_left_right_arrow; t2 = bbmterm ->
               { vname = None; vterm = MetaIff (t1.vterm, t2.vterm) }
            ]
         ];

      (*
       * Regular terms.
       * term: any possible term
       * aterm: annotated term
       * nocommaterm: any possible term that is not a pair
       * noncommaterm: a term that is not an operator or a pair
       * nonsimpleterm: a term that is not an expression, and
       *     not a simple operator
       * simpleterm: a term that is just an operator
       * quote_term: a tuple of opname * params * bterms
       *)
      term_eoi: [[ x = term; EOI -> x ]];

      term:
         [[ x = aterm ->
               x.aterm
          ]|[ "$"; s = STRING; "$" ->
               Phobos_exn.catch (Phobos_compile.term_of_string [] pho_grammar_filename) s
          ]|[ x = QUOTATION ->
            parse_quotation loc "term" (dest_quot x)
         ]];

      aterm:
         ["comma" LEFTA
          [ x = noncommaterm ->
             x
           | x = noncommaterm; sl_comma; y = noncommaterm ->
             { aname = None; aterm = mk_pair_term loc x.aterm y.aterm }
          ]
         ];

      noncommaterm:
         [
           "ite" LEFTA
            [ "if"; e1 = noncommaterm; "then"; e2 = noncommaterm; "else"; e3 = noncommaterm ->
               { aname = None; aterm = mk_dep0_dep0_dep0_term (mk_dep0_dep0_dep0_opname loc "ifthenelse") e1.aterm e2.aterm e3.aterm }
            | "let"; x = word_or_string; sl_equal; e1 = applyterm; "in"; e2 = noncommaterm ->
               { aname = None; aterm = mk_dep0_dep1_term (mk_dep0_dep1_opname loc "let") x e1.aterm e2.aterm }
            | e2 = noncommaterm; "where";  x = word_or_string; sl_equal; e1 = noncommaterm ->
               { aname = None; aterm = mk_dep0_dep1_term (mk_dep0_dep1_opname loc "let") x e1.aterm e2.aterm }
            | "open";  e1 =  applyterm; "in"; e2 = noncommaterm ->
               { aname = None; aterm = mk_dep0_dep1_term (mk_dep0_dep1_opname loc "let") "self" e1.aterm e2.aterm }
            ]

          (* Logical operators *)
          | "implies" RIGHTA
            [ t1 = noncommaterm; op = sl_implies; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            ]
          | "or" RIGHTA
            [ t1 = noncommaterm; op = sl_or; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            ]
          | "and" RIGHTA
            [ t1 = noncommaterm; op = sl_and; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            ]
          | "quantify" LEFTA
            [ (* all/exists*)
               op = sl_quantify; v = word_or_string; sl_colon; t1 = noncommaterm; sl_period; t2 = noncommaterm ->
                  { aname = None; aterm = mk_dep0_dep1_term (mk_dep0_dep1_opname loc op) v t1.aterm t2.aterm }
            |(* dall/dexists*)
               op = sl_quantify; v = word_or_string; sl_set_in; t1 = noncommaterm; sl_period; t2 = noncommaterm ->
                  { aname = None; aterm = mk_dep0_dep1_term (mk_dep0_dep1_opname loc  ("d"^op)) v t1.aterm t2.aterm }
            |(* sall/sexists*)
              op = sl_quantify; v = word_or_string; sl_period; t2 = noncommaterm ->
                  { aname = None; aterm = mk_dep1_term (mk_dep1_opname loc ("s"^op)) v t2.aterm }
            |(* thereis/forall *)
              op = sl_open_quantify; t1 = noncommaterm; sl_period; t2 = noncommaterm ->
                  { aname = None; aterm = mk_dep0_dep1_term (mk_dep0_dep1_opname loc op) "self" t1.aterm t2.aterm }
            ]
          | "neg"
            [ op = sl_not; x = noncommaterm ->
               { aname = None; aterm = mk_dep0_term (mk_dep0_opname loc op) x.aterm }
            ]

          (* Relations *)
          | "equal" NONA
            [ (* t1 = t2 in ty,   t1 <> t2 in ty -- (in)equality in type *)
              t1 = noncommaterm; (op,_) = sl_equal_rel; t2 = NEXT; sl_in; ty = noncommaterm ->
               { aname = None; aterm = mk_dep0_dep0_dep0_term (mk_dep0_dep0_dep0_opname loc op) ty.aterm t1.aterm t2.aterm }
            | (* t1 in ty  *)
              t = noncommaterm; op = sl_in; ty = NEXT ->
               (* XXX HACK - this is to support ad-hoc I/O form "member" - see TODO 2.14 -2.15 *)
                  let t =
                     try mk_dep0_dep0_term (mk_dep0_dep0_opname loc "member_of") t.aterm ty.aterm
                     with _ -> mk_dep0_dep0_dep0_term (mk_dep0_dep0_dep0_opname loc "equal") ty.aterm t.aterm t.aterm
                  in 
                     { aname = None; aterm = t }
            | (* t1 in t2 subset t3 *)
              t1 = noncommaterm; sl_in; t2 = NEXT; sl_subset; t3 = noncommaterm ->
               { aname = None; aterm = mk_dep0_dep0_dep0_term (mk_dep0_dep0_dep0_opname loc "member") t1.aterm t2.aterm t3.aterm }
            | (* t1 subset t2 *)
              t1 = noncommaterm; op = sl_subset; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            | (* t1 = t2, t1 <> t2  *)
              t1 = noncommaterm; (op,_) = sl_equal_rel; t2 = NEXT ->
                mk_arith_term loc op t1 t2
              (* t1 =[g] t2, t1 <>[g] t2  - algebraic relations for g *)
            | t1 = noncommaterm; (_,op) = sl_equal_rel; sl_open_brack; g = aterm; sl_close_brack; t2 = noncommaterm ->
               { aname = None; aterm = make_application loc [mk_field_term loc g.aterm op; t1.aterm; t2.aterm] }
            | (* t1 ~ t2 - squiggle equality  *)
              t1 = noncommaterm; sl_tilde; t2 = noncommaterm ->
               (* HACK - Perv!rewrite should be eventially replaced by mk_opname loc ["sqeq"] *)
               (* { aname = None; aterm = mk_dep0_dep0_term (mk_dep0_dep0_opname loc "sqeq") t1.aterm t2.aterm } *)
                  { aname = None; aterm = mk_xrewrite_term t1.aterm t2.aterm }
            | (* t1 In t2  - membership for set theory *)
              t1 = noncommaterm; op = sl_set_in; t2 = noncommaterm ->
               { aname = None; aterm = mk_dep0_dep0_term (mk_dep0_dep0_opname loc op) t1.aterm t2.aterm }
            ]
          | "compare" NONA
            [ (* t1 =@ t2, t1 <>@ t2, t1 <@ t2, t1 >@ t2, ...  - integer relations as booleans *)
               t1 = noncommaterm; op = sl_arith_rel; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            |(* t1 < t2, t1 > t2, ...  - integer relations as propositions *)
               t1 = noncommaterm; (op,_) = sl_rel;  t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            |(* t1 ^= t2, t1 ^<> t2, t1 ^< t2, t1 ^> t2, ...  - algebraic relations for self *)
               t1 = noncommaterm; op = sl_label_self_rel;  t2 = noncommaterm ->
               { aname = None; aterm = make_application loc [mk_field_self_term loc op; t1.aterm; t2.aterm] }
            |(* t1 <[g] t2, t1 >[g] t2, ...  - algebraic relations for g *)
               t1 = noncommaterm; (_,op) = sl_rel; sl_open_brack; g = aterm; sl_close_brack; t2 = noncommaterm ->
               { aname = None; aterm = make_application loc [mk_field_term loc g.aterm op; t1.aterm; t2.aterm] }
            ]


          (* Other operations *)
          | "cons" RIGHTA
            [ t1 = noncommaterm; op = sl_double_colon; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            ]
          | "fun" RIGHTA
           [ t1 = noncommaterm; op = sl_arrow; t2 = noncommaterm ->
              mk_type_term loc op t1 t2
           ]
          | "rev_fun" LEFTA
            [ t1 = noncommaterm; op = sl_left_arrow; t2 = noncommaterm ->
               mk_arith_term loc op t2 t1
            ]
          | "isect" RIGHTA
            [ (* Isect x: A. B[x], Union x:A. B[x]  - intersection, union of family of types *)
               op = sl_Isect; v = word_or_string; sl_colon; t1 = noncommaterm; sl_period; t2 = noncommaterm ->
                        { aname = None; aterm = mk_dep0_dep1_term (mk_dep0_dep1_opname loc op) v t1.aterm t2.aterm }
             | (* A union B, A isect B, x: A isect B[x]  - binary union, intersection and dependent intersection *)
               t1 = noncommaterm; op = sl_isect; t2 = noncommaterm ->
               mk_type_term loc op t1 t2
             |(* quot x,y: t1 // t2  - quotient type *)
               op = sl_quotient; x = word_or_string; sl_comma; y = word_or_string; sl_colon; t1 = noncommaterm; sl_double_slash; t2 = noncommaterm ->
               { aname = None; aterm = mk_dep0_dep2_term (mk_dep0_dep2_opname loc op) x y t1.aterm t2.aterm }
            ]
          | "plus" RIGHTA
            [  (* t1 +[g] t2  - algebraic plus *)
               t1 = noncommaterm; sl_plus; sl_open_brack; g = aterm;  sl_close_brack; t2 = noncommaterm ->
               { aname = None; aterm = make_application loc [mk_field_term loc g.aterm "+"; t1.aterm; t2.aterm] }
             | (* t1 + t2 - disjoint union *)
               t1 = noncommaterm; op = sl_plus; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
             | (* t1 ^+ t2   - algebraic plus for self *)
               t1 = noncommaterm; op = sl_label_self_plus;  t2 = noncommaterm ->
               { aname = None; aterm = make_application loc [mk_field_self_term loc op; t1.aterm; t2.aterm] }
            ]
          | "add" LEFTA
            [ (* t1 +@ t2, t1 -@ t2 - integer plus, minus *)
               t1 = noncommaterm; op = sl_arith_add; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            |(* t1 ^- t2   - algebraic minus for self *)
               t1 = noncommaterm; op = sl_label_self_minus;  t2 = noncommaterm ->
               { aname = None; aterm = make_application loc [mk_field_self_term loc op; t1.aterm; t2.aterm] }
            |(* t1 -[g] t2   - algebraic minus for g *)
               t1 = noncommaterm; op = sl_add; sl_open_brack; g = aterm; sl_close_brack; t2 = noncommaterm ->
               { aname = None; aterm = make_application loc [mk_field_term loc g.aterm op; t1.aterm; t2.aterm] }
            ]
          | "uminus"
            [ op = sl_minus; x = noncommaterm ->
               { aname = None; aterm = mk_dep0_term (mk_dep0_opname loc op) x.aterm }
            ]
          | "prod" RIGHTA
            [  (* t1 *[g] t2  - algebraic multiplication (e.g. group operation) *)
               t1 = noncommaterm; sl_star; sl_open_brack; g = aterm;  sl_close_brack; t2 = noncommaterm ->
               { aname = None; aterm = make_application loc [mk_field_term loc g.aterm "*"; t1.aterm; t2.aterm] }
             | (* t1 * t2 - type product *)
               t1 = noncommaterm; op = sl_star; t2 = noncommaterm ->
               mk_type_term loc op t1 t2
             |(* t1 ^* t2   - algebraic multiplication for self *)
               t1 = noncommaterm; op = sl_label_self_star;  t2 = noncommaterm ->
               { aname = None; aterm = make_application loc [mk_field_self_term loc op; t1.aterm; t2.aterm] }
            ]
          | "mul" LEFTA
            [ (* t1 *@ t2, t1 /@ t2, t1 %@ t2  - integer multiplication, division, reminder *)
               t1 = noncommaterm; op = sl_arith_mul; t2 = noncommaterm ->
               mk_arith_term loc op t1 t2
            |(* t1 ^/ t2   - algebraic division for self *)
               t1 = noncommaterm; op = sl_label_self_div;  t2 = noncommaterm ->
               { aname = None; aterm = make_application loc [mk_field_self_term loc op; t1.aterm; t2.aterm] }
            |(* t1 /[g] t2   - algebraic right division for g *)
               t1 = noncommaterm; op = sl_div; sl_open_brack; g = aterm; sl_close_brack; t2 = noncommaterm ->
               { aname = None; aterm = make_application loc [mk_field_term loc g.aterm op; t1.aterm; t2.aterm] }
            ]
          | "apply" LEFTA
            [ t = applyterm ->
               t
             | t = applyterm; l = applytermlist ->
               { aname = None; aterm = make_application loc (t.aterm :: l) }
            ]
         ];

      (* Term that can be used in application lists *)
      applyterm:
         [ [ op = opname ->
              { aname = None; aterm = mk_term (mk_op (mk_opname loc op [] []) []) [] }
            | op = opname; (params, bterms) = termsuffix ->
              { aname = None; aterm = mk_term (mk_op (mk_bopname loc op params bterms) params) bterms }
            | op = opname; sl_colon; t = applyterm ->
              match op with
                 [name] ->
                    if !debug_grammar then
                       eprintf "Got bound term: %s%t" name eflush;
                    { aname = Some (mk_var_term name); aterm = t.aterm }
               | _ ->
                    Stdpp.raise_with_loc loc (ParseError "illegal binding variable")
           ]
          | "power" RIGHTA
            [ (* t1 ^@ t2  - integer power *)
               t1 = applyterm; op = sl_arith_power; t2 = applyterm ->
               mk_arith_term loc op t1 t2
            |(* t1 ^^ t2   - algebraic power for self *)
               t1 = applyterm; op = sl_label_self_power;  t2 = applyterm ->
               { aname = None; aterm = make_application loc [mk_field_self_term loc op; t1.aterm; t2.aterm] }
            |(* t1 ^[g] t2   - algebraic power for g *)
               t1 = applyterm; op = sl_power; sl_open_brack; g = aterm; sl_close_brack; t2 = applyterm ->
               { aname = None; aterm = make_application loc [mk_field_term loc g.aterm op; t1.aterm; t2.aterm] }
            |(* r ^ lab - field selection for records *)
              r = applyterm; sl_power; lab = word_or_string  ->
             { aname = None;
               aterm = mk_field_term loc r.aterm lab
             }
            |(* r ^ lab := t - field update for records *)
               r = applyterm; sl_power; lab = word_or_string; sl_assign; t = noncommaterm  ->
             { aname = None;
               aterm = mk_term (mk_op (mk_opname loc ["rcrd"] [ShapeToken] [0;0])
                               [make_param (Token lab )])  [mk_simple_bterm t.aterm; mk_simple_bterm  r.aterm]
             }
            |(* ^ lab  - field selection for self *)
              sl_power; lab = word_or_string  ->
             { aname = None;
               aterm = mk_field_self_term loc lab
             }
            |(* ^ lab - field update for records *)
               sl_power; lab = word_or_string; sl_assign; t = noncommaterm   ->
             { aname = None;
               aterm = mk_term (mk_op (mk_opname loc ["rcrd"] [ShapeToken] [0;0])
                               [make_param (Token lab )])  [mk_simple_bterm t.aterm; mk_simple_bterm  (mk_var_term "self")]
             }
           ]
          | [ t = nonwordterm ->
               t
            ]
         ];

      (* Singleterm is a distinct term and no colons *)
      singleterm:
         [ [ op = opname ->
              { aname = None; aterm = mk_term (mk_op (mk_opname loc op [] []) []) [] }
            | op = opname; (params, bterms) = termsuffix ->
              { aname = None; aterm = mk_term (mk_op (mk_bopname loc op params bterms) params) bterms }
           ]
          | [ t = nonwordterm ->
               t
            ]
          | [ "$"; s = STRING; "$" ->
               { aname = None; aterm = Phobos_exn.catch (Phobos_compile.term_of_string [] pho_grammar_filename) s}
            ]
          | [ x = QUOTATION ->
               { aname = None; aterm = parse_quotation loc "term" (dest_quot x) }
            ]
         ];

      bound_term:
         [ [ sl_open_paren; v = varterm; sl_colon; t = aterm; sl_close_paren ->
               { aname = Some v; aterm = t.aterm }
           ]
          | [ t = singleterm ->
              t
           ]
         ];

      termsuffix:
         [[ p = params ->
             p, []
           | p = params; sl_open_curly; bterms = btermslist; sl_close_curly ->
             p, bterms
           | sl_open_curly; bterms = btermslist; sl_close_curly ->
             [], bterms
          ]];


      rcrdterm:
         [[ ";"; lab = word_or_string; sl_equal; t = aterm  ->
               (lab,t)
         ]];

      recordterm:
         [[ lab = word_or_string; sl_colon; t = aterm  ->
             (Some lab,t)
           ]
         |[ t = aterm  ->
             (None,t)
           ]];

      nonwordterm:
         [[ (* vars *)
             v = varterm ->
             { aname = None; aterm = v }

             (* Abbreviations *)
           | i = sl_number ->
             { aname = None;
               aterm = mk_term (mk_op (mk_opname loc ["number"] [ShapeNumber] [])
                               [make_param (Number i)]) []
             }
           | x = sequent ->
             { aname = None; aterm = x }

             (* Parenthesized terms *)
           | sl_open_paren; t = aterm; sl_close_paren ->
             t
             (* records {x1=a1;x2=a2;...} *)
           | sl_open_curly; lab = word_or_string; sl_equal; t = aterm; rest = LIST0 rcrdterm; sl_close_curly ->
                let r0 =   mk_term (mk_op (mk_opname loc ["rcrd"] [] []) []) [] in
                let aux = fun r -> function (lab,t) ->
                           mk_term (mk_op (mk_opname loc ["rcrd"] [ShapeToken] [0;0])
                               [make_param (Token lab )])  [mk_simple_bterm t.aterm; mk_simple_bterm  r]
                in
                   { aname = None;
                     aterm = List.fold_left aux r0 ((lab,t)::rest)
                   }
             (* record typess {x1:A1;x2:a2;...} *)
           | sl_open_curly; lab = word_or_string; sl_colon; t = aterm; ";"; r = LIST0 recordterm SEP ";"; sl_close_curly ->
                let r0 =   mk_term (mk_op (mk_opname loc ["record"] [ShapeToken] [0])
                               [make_param (Token lab )])  [mk_simple_bterm t.aterm] in
                let aux = fun r -> function
                      (Some lab,t) ->
                           mk_term (mk_op (mk_opname loc ["record"] [ShapeToken] [1;0])
                               [make_param (Token lab )])  [mk_bterm ["self"] t.aterm; mk_simple_bterm  r]
                   |  (None,t) ->
                           mk_dep0_dep1_term (mk_dep0_dep1_opname loc "set") "self" r t.aterm
                in
                   { aname = None;
                     aterm = List.fold_left aux r0 r
                   }
             (* single record types {x1:A1} *)
           | sl_open_curly; lab = word_or_string; sl_colon; t = aterm; sl_close_curly ->
                let r0 =   mk_term (mk_op (mk_opname loc ["record"] [ShapeToken] [0])
                               [make_param (Token lab )])  [mk_simple_bterm t.aterm]
                in
                   { aname = None;
                     aterm = r0
                   }
             (* sets {x:A | P[x]} *)
           | sl_open_curly; v =  word_or_string; sl_colon; ty = aterm; sl_pipe; b = aterm; sl_close_curly ->
             { aname = None; aterm = mk_dep0_dep1_term (mk_dep0_dep1_opname loc "set") v ty.aterm b.aterm }
             (* very dependent functions {f | x:A -> B[x]} *)
           | sl_open_curly; f =  word_or_string; sl_pipe; t = aterm; sl_close_curly ->
             let t = t.aterm in
             let rfun_op = mk_dep0_dep2_opname loc "rfun" in
             let t' =
                if is_dep0_dep1_term (mk_dep0_dep1_opname loc "fun") t then
                   let v, a, b = dest_dep0_dep1_any_term t in
                      mk_dep0_dep2_term rfun_op f v a b
                else if is_dep0_dep0_term (mk_dep0_dep0_opname loc "fun") t then
                   let a, b = two_subterms t in
                      mk_dep0_dep2_term rfun_op f (mk_gensym ()) a b
                else
                   raise (ParseError "body of <rfun> is not a function")
             in
                { aname = None; aterm = t' }
          ]];

      varterm:
         [[ sl_single_quote; v = word_or_string ->
             mk_var_term v
           | sl_single_quote; v = word_or_string; sl_open_brack; terms = opttermlist; sl_close_brack ->
             mk_so_var_term v terms
          ]];

      quote_term:
         [[ v = word_or_string; params = optparams; bterms = optbterms ->
             v, params, bterms
          ]];

      (* Application lists *)
      applytermlist:
         [[ x = applyterm ->
             [x.aterm]
           | l = applytermlist; x = applyterm ->
             l @ [x.aterm]
          ]];

      (* List of terms *)
      opttermlist:
         [[ l = OPT termlist ->
             match l with
                Some l' -> l'
              | None -> []
          ]];

      termlist:
         [[ t = term ->
             [t]
           | l = termlist; sl_semi_colon; t = term ->
             l @ [t]
          ]];

      (* Parameters and bterm lists *)
      opname:
         [[ op = rev_opname ->
             List.rev op
          ]];

      rev_opname:
         [[ w = word_or_string ->
             [w]
           | l = rev_opname; sl_exclamation; w = word_or_string ->
             w :: l
          ]];

      optparams:
         [[ params = OPT params ->
             match params with
                Some params' -> params'
              | None -> []
          ]];

      params:
         [[ sl_open_brack; params = LIST0 param SEP ","; sl_close_brack ->
             params
          ]];

      (* Parameters *)
      param:
         [[  w = sl_word ->
             make_param (MString w)
           | w = STRING ->
             make_param (String (Token.eval_string w))
           | n = sl_number ->
             make_param (Number n)
          ]
          | [ p = param; sl_colon; w = sl_word ->
               cast_param p w
             | p = param; sl_single_quote ->
               make_param (MLevel (incr_level_exp (cast_level p)))
             | p1 = param; sl_pipe; p2 = param ->
               make_param (MLevel (max_level_exp (cast_level p1) (cast_level p2) 0))
            ]
         ];

      (* Bound terms *)
      optbterms:
         [[ bterms = OPT bterms ->
             match bterms with
                Some bterms' -> bterms'
              | None -> []
          ]];

      bterms:
         [[ sl_open_curly; bterms = btermslist; sl_close_curly ->
             bterms
          ]];

      btermslist:
         [[ l = OPT btermlist ->
             let l' =
                match l with
                   Some l' -> l'
                 | None -> []
             in
                List.map mk_bterm' l'
          ]];

      btermlist:
         [[ t = bterm ->
             [t]
           | l = btermlist; sl_semi_colon; t = bterm ->
             l @ [t]
          ]];

      bterm:
         [[ h = bhead ->
             [], tupelize loc h
           | h = bhead; sl_period; t = term ->
             check_bvars h, t
           | sl_period; t = term ->
             [], t
          ]];

      bhead:
         [[ t = bsingle ->
             [t]
           | h = bhead; sl_comma; w = bsingle ->
             h @ [w]
          ]];

      bsingle:
         [[ w = word_or_string ->
             ST_String w
           | w = word_or_string; (params, bterms) = termsuffix ->
             ST_Term (mk_term (mk_op (mk_bopname loc [w] params bterms) params) bterms, loc)
           | t = noncommaterm ->
             ST_Term (t.aterm, loc)
          ]];

      (* Special forms *)
      sequent:
         [[ sl_sequent; args = optseqargs; sl_open_curly;
            hyps = LIST0 hyp SEP ";"; sl_turnstile;
            concl = LIST1 term SEP ";"; sl_close_curly ->
             let esequent =
                { sequent_args = mk_xlist_term args;
                  sequent_hyps = SeqHyp.of_list hyps;
                  sequent_goals = SeqGoal.of_list concl
                }
             in
                if !debug_grammar then
                   eprintf "Constructing sequent: %d, %d%t" (List.length hyps) (List.length concl) eflush;
                mk_sequent_term esequent
          ]];

      hyp:
         [[ "<"; name = word_or_string; args=optseqargs; ">" ->
             Context(name,args)
             (* XXX HACK!!! OPT sl_period is here to have a way to tell camlp4 that opname *
              * is not a hyp variabe and it should not expect a : after it                 *)
          | v = word_or_string; rest = hyp_suffix ->
              rest(v)
          | t = aterm ->
              Hypothesis t.aterm
          ]];

      hyp_suffix:
         [[ sl_colon; t = aterm ->
               fun v -> HypBinding (v, t.aterm)
          | (params, bterms) = termsuffix ->
               fun op -> Hypothesis (mk_term (mk_op (mk_bopname loc [op] params bterms) params) bterms)
          | ->
               fun op -> Hypothesis (mk_term (mk_op (mk_opname loc [op] [] []) []) [])
          ]];

      optseqargs:
         [[ args = OPT seqargs ->
             match args with
                Some l -> l
              | None -> []
          ]];

      seqargs:
         [[ sl_open_brack; l = LIST0 term SEP ";"; sl_close_brack ->
             l
          ]];

      (*
       * A term describing the display form.
       * We allow a concatenation of terms.
       *)
      xdform:
         [[ l = LIST0 df_item ->
             mk_xlist_term l
          ]];

      df_item:
         [[ t = singleterm ->
             t.aterm
           | sl_back_quote; name = STRING ->
             mk_xstring_term (Token.eval_string name)
          ]];

      (* Terminals *)
      sl_meta_left_right_arrow:
         [[ "<-->" -> () ]];

      sl_meta_right_arrow:
         [[ "-->" -> () ]];

      sl_meta_left_arrow:
         [[ "<--" -> () ]];

      sl_open_curly:
         [[ "{" -> () ]];

      sl_close_curly:
         [[ "}" -> () ]];

      sl_open_paren:
         [[ "(" -> () ]];

      sl_close_paren:
         [[ ")" -> () ]];

      sl_open_brack:
         [[ "[" -> () ]];

      sl_close_brack:
         [[ "]" -> () ]];

      sl_in:
         [[ "in" -> ()
          | "IN" -> () ]];

      sl_set_in:
         [[ "In" -> "mem"
         ]];

      sl_colon:
         [[ ":" -> () ]];

      sl_double_colon:
         [[ "::" -> "cons" ]];

      sl_semi_colon:
         [[ ";" -> () ]];

      sl_double_slash:
         [[ "//" -> () ]];

      sl_comma:
         [[ "," -> () ]];

      sl_period:
         [[ "." -> () ]];

      sl_pipe:
         [[ "|" -> () ]];

      sl_single_quote:
         [[ "'" -> () ]];

      sl_back_quote:
         [[ "`" -> () ]];

      sl_sequent:
         [[ "sequent" -> () ]];

      sl_turnstile:
         [[ ">-" -> () ]];

      sl_exclamation:
         [[ "!" -> () ]];

      sl_assign:
         [[ ":=" -> "assign" ]];

      sl_equal:
         [[ "=" -> () ]];

      sl_equal_rel:
         [[ "=" -> "equal","="
          | "<>" -> "nequal","<>"
          ]];

      sl_subset:
         [[ "subset" -> "subset" ]];

      sl_rel:
         [[ "<" -> "lt","<"
          | ">" -> "gt",">"
          | "<=" -> "le", "<="
          | ">=" -> "ge", ">="
          | "subtype" -> "subtype","subtype"
          ]];

      sl_arith_rel:
         [[ "=@" -> "beq_int"
          | "<@" -> "lt_bool"
          | ">@" -> "gt_bool"
          | "<=@" -> "le_bool"
          | ">=@" -> "ge_bool"
          | "<>@" -> "bneq_int"
          ]];

      sl_label_self_rel:
         [[ "^=" -> "="
          | "^<" -> "<"
          | "^>" -> ">"
          | "^<=" -> "<="
          | "^>=" -> ">="
          | "^<>" -> "<>"
          ]];

      sl_plus:
         [[ "+" -> "union" ]];

      sl_add: (* other operations with addition prioruty *)
         [[ "-" -> "-" ]];

      sl_minus: (* unary minus *)
         [[ "-" -> "minus" ]];

      sl_arith_add:
         [[ "+@" -> "add"
          | "-@" -> "sub" ]];

      sl_label_self_plus:
         [[ "^+" -> "+"]];

      sl_label_self_minus:
         [[ "^-" -> "-"]];

      sl_star:
         [[ "*" -> "prod" ]];

      sl_div:
         [[ "/" -> "/"
          ]];

      sl_arith_mul:
         [[ "*@" -> "mul"
          | "/@" -> "div"
          | "%@" -> "rem" ]];

      sl_label_self_star:
         [[ "^*" -> "*"
          ]];

      sl_label_self_div:
         [[ "^/" -> "/"
          ]];

      sl_power:
         [[ "^" -> "^" ]];

      sl_arith_power:
         [[ "^@" -> "power" ]];

      sl_label_self_power:
         [[ "^^" -> "^" ]];

      sl_arrow:
         [[ "->" -> "fun" ]];

      sl_left_arrow:
         [[ "<-" -> "fun" ]];

      sl_not:
         [[ "neg" -> "not" ]];

      sl_or:
         [[ "or" -> "or" ]];

      sl_and:
         [[ "and" -> "and"
           | "&" -> "and"
          ]];

      sl_implies:
         [[ "=>" -> "implies" ]];

      sl_open_quantify:
         [[ "forany" -> "all"
          | "thereis" -> "exists"
          ]];

      sl_quantify:
         [[ "all" -> "all"
          | "exst" -> "exists"
          ]];

      sl_isect:
         [[ "isect" -> "bisect"
          | "bunion" -> "bunion"
          ]];

      sl_Isect:
         [[ "Isect" -> "isect"
          | "Union" -> "tunion"
          ]];

      sl_quotient:
         [[ "quot" -> "quot" ]];

      sl_tilde:
         [[ "~" -> () ]];

(*
      sl_bind:
         [[ "bind" -> "bind" ]];
*)

      sl_number:
         [[ n = INT ->
             Mp_num.num_of_string n
          ]];

      (* Take a word or a string as an identifier *)
      word_or_string:
         [[ name = UIDENT ->
             name
           | name = LIDENT ->
             name
           | name = STRING ->
             Token.eval_string name
          ]];

      sl_word:
         [[ s = LIDENT -> s
           | s = UIDENT -> s
          ]];
   END

   (* Implementation *)
   let mterm = mterm
   let bmterm = bmterm
   let quote_term = quote_term
   let term = term
   let term_eoi = term_eoi
   let singleterm = singleterm
   let applytermlist = applytermlist
   let bound_term = bound_term
   let mk_opname = mk_opname
   let xdform = xdform
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
