(*
 * This file contains the primitive syntax and display
 * for ocaml terms.
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

include Ocaml
include Ocaml_base_df

open Mp_debug
open Printf

let _ =
   show_loading "Loading Ocaml_expr_df%t"

(*
 * Special flags.
 *)
declare ident_expr
declare list_expr
declare se_list
declare ee_list
declare e_list

(*
 * Precedences.
 *)
prec prec_proj
prec prec_apply
prec prec_cons
prec prec_assign
prec prec_equal
prec prec_if
prec prec_rel
prec prec_not
prec prec_fun
prec prec_let

(*
 * Constants.
 *)
dform char_df1 : "char"[c:s] =
   "'" slot[c:s] "'"

dform char_df2 : internal :: "char"[start:n, finish:n, c:s] =
   "char"[c:s]

dform int_df1 : "int"[i:n] =
   slot[i:s]

dform int_df2 : internal :: "int"[start:n, finish:n, i:n] =
   "int"[i:n]

dform string_df1 : Ocaml!"string"[s:s] =
   "\"" slot[s:s] "\""

dform string_df2 : internal :: Ocaml!"string"[start:n, finish:n, s:s] =
   Ocaml!"string"[s:s]

dform float_df1 : "float"[f:s] =
   slot[f:s]

dform float_df2 : internal :: "float"[start:n, finish:n, f:s] =
   "float"[f:s]

dform lid_df1 : "lid"{'v} =
   slot{'v}

dform lid_df2 : internal :: "lid"[start:n, finish:n]{'v} =
   "lid"{'v}

dform lid_df3 : "lid"[v:s] =
   slot[v:s]

dform uid_df1 : "uid"{'v} =
   slot{'v}

dform uid_df2 : internal :: "uid"[start:n, finish:n]{'v} =
   "uid"{'v}

dform uid_df3 : "uid"[v:s] =
   slot[v:s]

(*
 * Projection.
 *)
dform proj_df1 : parens :: "prec"[prec_proj] :: "proj"{'A; 'B} =
   pushm[0] slot{'A} "." slot{'B} popm

dform proj_df2 : internal :: "proj"[start:n, finish:n]{'A; 'B} =
   "proj"{'A; 'B}

(*
 * Application.
 *)
dform apply_df1 : parens :: "prec"[prec_apply] :: "apply"{'e1; 'e2} =
   pushm[0] slot{'e1} hspace slot{'e2} popm

dform apply_df2 : "apply"[start:n, finish:n]{'e1; 'e2} =
   "apply"{'e1; 'e2}

dform apply_df3 : "apply"{."apply"{.lid{lid[name:s]}; 'e1}; 'e2} =
   "apply"[name:s]{'e1; 'e2}

dform apply_df4 : internal :: "apply"{."apply"[start1:n, finish1:n]{.lid[start2:n, finish2:n]{lid[name:s]}; 'e1}; 'e2} =
   "apply"{."apply"{lid{lid[name:s]}; 'e1}; 'e2}

dform apply_plus_df : internal :: "apply"["+"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace "+" hspace slot{'e2} popm

dform apply_minus_df : internal :: "apply"["-"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace "-" hspace slot{'e2} popm

dform apply_star_df : internal :: "apply"["*"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace "*" hspace slot{'e2} popm

dform apply_slash_df : internal :: "apply"["/"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace "/" hspace slot{'e2} popm

dform apply_hat_df : internal :: "apply"["^"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword["^"] hspace slot{'e2} popm

dform apply_at_df : internal :: "apply"["@"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword["@"] hspace slot{'e2} popm

dform apply_cons_df : internal :: "apply"["::"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword["::"] hspace slot{'e2} popm

dform apply_lt_df : internal :: "apply"["<"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword["<"] hspace slot{'e2} popm

dform apply_le_df : internal :: "apply"["<="]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword["<="] hspace slot{'e2} popm

dform apply_eq_df : internal :: "apply"["="]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword["="] hspace slot{'e2} popm

dform apply_eqeq_df : internal :: "apply"["=="]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword["=="] hspace slot{'e2} popm

dform apply_ge_df : internal :: "apply"[">="]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword[">="] hspace slot{'e2} popm

dform apply_gt_df : internal :: "apply"[">"]{'e1; 'e2} =
   pushm[0] slot{'e1} hspace keyword[">"] hspace slot{'e2} popm

dform apply_any_df : internal :: "apply"[name:s]{'e1; 'e2} =
   pushm[0] slot[name:s] hspace slot{'e1} hspace slot{'e2} popm

(*
 * Subscripting.
 *)
dform array_subscript_df1 : parens :: "prec"[prec_proj] :: "array_subscript"{'e1; 'e2} =
   slot{'e1} array_subscript pushm[0] slot{'e2} popm ")"

dform array_subscript_df2 : internal :: "array_subscript"[start:n, finish:n]{'e1; 'e2} =
   "array_subscript"{'e1; 'e2}

dform string_subscript_df1 : parens :: "prec"[prec_proj] :: "string_subscript"{'e1; 'e2} =
   slot{'e1} string_subscript pushm[0] slot{'e2} popm ")"

dform string_subscript_df2 : internal :: "string_subscript"[start:n, finish:n]{'e1; 'e2} =
   "string_subscript"{'e1; 'e2}

(*
 * Sequencing.
 *)
dform sequence_df1 : "sequence"{'e1} =
   szone pushm[0] slot{list_expr; 'e1} popm ezone

dform sequence_df2 : internal :: "sequence"[start:n, finish:n]{'e1} =
   sequence{'e1}

(*
 * Lists, arrays, streams, records.
 * This is a recursive display form.
 *)
dform list_df1 : "list"{'e1} =
   "[" pushm[0] slot{list_expr; 'e1} popm "]"

dform array_df1 : "array"{'e1} =
   "[|" pushm[0] slot{list_expr; 'e1} popm "|]"

dform stream_df1 : "stream"{'e1} =
   "[<" pushm[0] slot{se_list; 'e1} popm ">]"

dform record_df1 : "record"{'e1} =
   "{" pushm[0] slot{ee_list; 'e1} popm "}"

dform tuple_df1 : "tuple"{'e1} =
   "(" pushm[0] slot{e_list; 'e1} popm ")"

dform list_df2 : internal :: "list"[start:n, finish:n]{'e1} =
   "list"{'e1}

dform array_df2 : internal :: "array"[start:n, finish:n]{'e1} =
   "array"{'e1}

dform stream_df2 : internal :: "stream"[start:n, finish:n]{'e1} =
   "stream"{'e1}

dform record_df2 : internal :: "record"[start:n, finish:n]{'e1} =
   "record"{'e1}

dform tuple_df2 : internal :: "tuple"[start:n, finish:n]{'e1} =
   "tuple"{'e1}

(*
 * Lists & arrays.
 *)
dform list_expr_cons_df1 : internal :: slot{list_expr; cons{'e1; 'e2}} =
   slot{list_expr; 'e1; 'e2}

dform list_expr_cons_df2 : internal :: slot{list_expr; 'e1; cons{'e2; 'e3}} =
   szone slot{'e1} ezone ";" hspace slot{list_expr; cons{'e2; 'e3}}

dform nil_df : internal :: slot{list_expr; 'e1; nil} =
   szone slot{'e1} ezone

(*
 * Module name.
 *)
dform ident_expr_cons_df1 : internal :: slot{ident_expr; cons{'e1; 'e2}} =
   slot{ident_expr; 'e1; 'e2}

dform ident_expr_cons_df2 : internal :: slot{ident_expr; .Ocaml!"string"[name:s]; cons{'e1; 'e2}} =
   slot[name:s] `"." slot{ident_expr; 'e1; 'e2}

dform ident_expr_nil_df : internal :: slot{ident_expr; .Ocaml!"string"[name:s]; nil} =
   slot[name:s]

(*
 * Streams.
 *)
dform se_list_nil_df : internal :: slot{se_list; nil} =
   `""

dform se_list_cons_df1 : internal :: slot{se_list; cons{'e1; 'e2}} =
   slot{se_list; 'e1; 'e2}

dform se_list_cons_df2 : internal :: slot{se_list; cons{'s; 'e}; nil} =
   slot{'s} `"XXX" slot{'e}

dform se_list_cons_df3 : internal :: slot{se_list; cons{'s; 'e}; cons{'e2; 'e3}} =
   slot{'s} `"XXX" slot{'e} ";" hspace slot{se_list; 'e2; 'e3}

(*
 * Tuples.
 *)
dform e_list_nil_df1 : internal :: slot{e_list; nil} =
   `""

dform e_list_nil_df2 : internal :: slot{e_list; nil; e_list} =
   `""

dform e_list_cons_df1 : internal :: slot{e_list; cons{'e1; 'e2}} =
   slot{'e1}
   slot{e_list; 'e2; e_list}

dform e_list_cons_df2 : internal :: slot{e_list; cons{'e1; 'e2}; e_list} =
   `"," hspace slot{'e1}
   slot{e_list; 'e2; e_list}

(*
 * Records.
 *)
dform ee_list_nil_df : internal :: slot{ee_list; nil} =
   `""

dform ee_list_nil_df : internal :: slot{ee_list; nil; ee_list} =
   `""

dform ee_list_nil_df1 : internal :: slot{ee_list; cons{ee{'e1; 'e2}; 'e3}} =
   szone slot{'e1} hspace "=" hspace slot{'e2} ezone
   slot{ee_list; 'e3; ee_list}

dform ee_list_nil_df2 : internal :: slot{ee_list; cons{ee{'e1; 'e2}; 'e3}; ee_list} =
   ";" hspace szone slot{'e1} `" " "=" hspace slot{'e2} ezone
   slot{ee_list; 'e3; ee_list}

(*
 * Assignment.
 *)
dform assign_df1 : parens :: "prec"[prec_assign] :: assign{'e1; 'e2} =
   push_indent slot{'e1} hspace assign slot{'e2} popm

dform assign_df2 : internal :: assign[start:n, finish:n]{'e1; 'e2} =
   assign{'e1; 'e2}

(*
 * Conditional.
 *)
dform ifthenelse_df : parens :: "prec"[prec_if] :: ifthenelse{'e1; 'e2; 'e3} =
   pushm[0] szone push_indent "_if" `" " slot{'e1} `" " "_then" hspace
   szone slot{'e2} ezone popm hspace
   push_indent "_else" hspace
   szone slot{'e3} ezone popm popm

dform ifthenelse_df2 : internal :: ifthenelse[start:n, finish:n]{'e1; 'e2; 'e3} =
   ifthenelse{'e1; 'e2; 'e3}

(*
 * Loops.
 *)
dform for_upto_df1 : for_upto{'e1; 'e2; x. 'e3} =
   pushm[0] push_indent
   "_for" hspace slot{'x} hspace assign slot{'e2} hspace "_to" slot{'e3} hspace "_do" hbreak
      slot{'e3} popm hbreak
      "_done" popm

dform for_upto_df2 : internal :: for_upto[start:n, finish:n]{'e1; 'e2; x. 'e3} =
   for_upto{'e1; 'e2; x. 'e3}

dform for_downto_df1 : for_downto{'e1; 'e2; x. 'e3} =
   pushm[0] push_indent
   "_for" hspace slot{'x} hspace assign slot{'e2} hspace "_downto" slot{'e3} hspace "_do" hbreak
      slot{'e3} popm hbreak
      "_done" popm

dform for_downto_df2 : internal :: for_downto[start:n, finish:n]{'e1; 'e2; x. 'e3} =
   for_downto{'e1; 'e2; x. 'e3}

dform while_df1 : "while"{'e1; 'e2} =
   szone pushm[0] push_indent "_while" hspace slot{'e1} hspace "_do" hspace
   slot{'e2} popm hspace
   "_done" popm ezone

dform while_df2 : internal :: "while"[start:n, finish:n]{'e1; 'e2} =
   "_while"{'e1; 'e2}

(*
 * Type casting.
 *)
dform cast_df1 : cast{'e; 't} =
   "(" slot{'e} hspace ":" hspace slot{'t} ")"

dform cast_df2 : internal :: cast[start:n, finish:n]{'e; 't} =
   cast{'e; 't}

(*
 * Class coercion.
 *)
dform class_coerce_df1 : parens :: "prec"[prec_rel] :: class_coerce{'e1; 'e2} =
   push_indent slot{'e1} hspace class_coerce slot{'e2} popm

dform class_coerce_df2 : internal :: class_coerce[start:n, finish:n]{'e1; 'e2} =
   class_coerce{'e1; 'e2}

(*
 * New object.
 *)
declare "new"{'e1}

dform new_df1 : parens :: "prec"[prec_not] :: "new"{'e1} =
   "_new" slot{'e1}

(*
 * "Match" forms.
 *)
dform fun_df1 : parens :: "prec"[prec_fun] :: "fun"{'pwel} =
   szone "_fun" `" " slot{patt_format; 'pwel; nil} ezone

dform fun_df2 : internal :: "fun"[start:n, finish:n]{'pwel} =
   "fun"{'pwel}

dform match_df1 : parens :: "prec"[prec_fun] :: "match"{'pwel; 'e} =
   szone push_indent "_match" hspace slot{'e} hspace "_with" hspace
   slot{patt_format; 'pwel; nil}
   popm ezone

dform match_df2 : internal :: "match"[start:n, finish:n]{'e; 'pwel} =
   "match"{'e; 'pwel}

dform try_df1 : parens :: "prec"[prec_fun] :: "try"{'pwel; 'e} =
   szone push_indent "_try" hspace slot{'e} hspace "_with" hspace
   slot{patt_format; 'pwel; nil}
   popm ezone

dform try_df2 : internal :: "try"[start:n, finish:n]{'e; 'pwel} =
   "try"{'e; 'pwel}

(*
 * "Let" forms.  The real work is performed in the patterns.
 *)
dform let_df1 : parens :: "prec"[prec_let] :: "let"{'p; 'e} =
   szone pushm[0] "_let" `" " slot{patt_format; 'p; 'e} popm ezone

dform let_df2 : internal :: "let"[start:n, finish:n]{'p; 'e} =
   "let"{'p; 'e}

dform fix_df1 : parens :: "prec"[prec_let] :: "fix"{'p} =
   szone pushm[0] "_letrec" hspace slot{patt_format; 'p; nil}

dform fix_df2 : internal :: "fix"[start:n, finish:n]{'p} =
   "fix"{'p}

(*
 * Finally, a special form for terms.
 *)
ml_dform term_df : "apply"[start1:n, finish1:n]{
                     ."proj"[start2:n, finish2:n]{
                        ."uid"[start3:n, finish3:n]{."uid"["Ml_term":s]};
                        ."lid"[start4:n, finish4:n]{."lid"["term_of_string":s]}};
                     .Ocaml!"string"[start5:n, finish5:n, s:s]} format_term buf =
   fun goal -> format_term buf Dform.LEParens (Ml_term.term_of_string s)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
