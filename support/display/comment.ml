doc <:doc<
   @spelling{typeset verbatim subparagraph italicizes hypertext}
   @module[Comment]

   The @tt{Comment} module defines @emph{structured} comments.
   Structured comments are typed in after a keyword @keyword[doc] followed by
   a quoted term expression. They are usually used to provide documentation,
   although special forms can be defined for other purposes.

   The term following the @keyword[doc] keyword can either utilize the usual syntax for terms
   or a special @tt["<:doc< ... >>"] quotation with a specialized syntax inside.
   The text inside the @tt["<:doc< ... >>"] is parsed
   into a sequence of strings in a list.  The text can also
   contain terms, which begin with the @tt{@@} character.

   @begin[verbatim]
   @opname[s1, ..., sm]{t1; ...; tn}
   @end[verbatim]

   The @tt[opname] is an operator name.  The usual quantification can be
   used; the term @code{@Itt_dprod!prod} refers to the @hrefterm[prod] term
   defined in the @hrefmodule[Itt_dprod] module.  The strings $s_1, @ldots, s_m$
   are the @emph{parameters} of the term, and the $t_1; @cdots; t_n$ expressions
   are the subterms.  The parameters must be strings, possibly enclosed in
   double-quotes.  The subterms are normal comment text.

   Terms can also be constructed using a @tt{begin/end} construction.  For example,
   the @tt[it] term italicizes its contents.

   @begin[verbatim]
   @it{Hello world}
   @end[verbatim]

   The following definition is equivalent.

   @begin[verbatim]
   @begin[it]
   Hello world.
   @end[it]
   @end[verbatim]

   There is also a @emph{math} mode, which is entered for terms between
   @tt["$"] or @tt["$$"] forms.  The contents of math mode is
   parsed in a similar manner to normal mode, but the `_' and `^' characters
   are significant in math mode (they are normal text in normal mode).
   The `_' term identifies a subscript operation, and the `^' term
   denotes a superscript.

   The @tt["<:doc< ... >>"] quotation can also contain the standard
   @tt["<< ... >>"] inside (which would imply math mode), and @emph{vice versa}.
   @docoff

   ----------------------------------------------------------------

   @begin[license]
   Copyright (C) 2000 Jason Hickey, Caltech

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Jason Hickey @email{jyh@cs.caltech.edu}
   @end[license]
>>

doc <:doc<
   @parents
>>
extends Base_dform

(************************************************************************
 * STRUCTURED COMMENTS                                                  *
 ************************************************************************)

doc <:doc<
   @terms

   The @tt[comment_white] term represents white space.
   The @tt[comment_string] term is a literal string.
   The @tt[comment_block] term encloses a nested comment
   block.
>>
declare comment_white : Dform
declare comment_string[s:s] : Dform
declare comment_block{'t : Dform} : Dform

doc docoff

declare "doc"{'t : Dform} : Dform

doc <:doc<
   The @tt[license] term is used to enclose the license agreement
   text for a module.

   All the words in the @tt[spelling] term are added to the
   spelling dictionary as correctly-spelled words.
>>
declare license{'t : Dform} : Dform
declare spelling{'t : Dform} : Dform
declare misspelled{'t : Dform} : Dform

doc docoff

(*
 * Standard display forms.
 *)
declare com_cbreak : Dform
declare com_hbreak : Dform

dform com_cbreak_df : mode[tex] :: com_cbreak =
   slot["raw", " "]

dform com_hbreak_df : mode[tex] :: com_hbreak =
   slot["raw", " "]

dform com_cbreak_df : except_mode[tex] :: com_cbreak =
   cbreak["", " "]

dform com_hbreak_df : except_mode[tex] :: com_hbreak =
   hbreak["", " "]

dform comment_string_df1 : comment_string[s:s] =
   slot[s:s]

dform comment_block_df1 : comment_block{'t} =
   't

doc <:doc<
   The @code{@docoff} term disables display for text that follows the structured comment.
   The @code{@docon} term and any @code{doc} directive not starting with a @code{@docoff}
   re-enables display.
>>
declare docoff : Dform
declare docon : Dform
doc docoff

dform docoff_df1 : mode[tex] :: docoff =
   izone slot["raw", "%\n\\fi\\texfalse\\iftex%\n"] ezone

dform docoff_df2 : except_mode[tex] :: docoff =
   bf["docoff"]

dform docon_df1 : mode[tex] :: docon =
   izone slot["raw", "%\n\\fi\\textrue\\bgmptab\\iftex%\n"] ezone

dform docom_df2 : mode[tex] :: xcons{docon; 't} =
   izone slot["raw", "%\n\\fi\\textrue\\iftex%\n"] ezone 't

dform docon_df3 : except_mode[tex] :: docon =
   bf["docoff"]

(*
 * TeX version.
 *)
dform tex_comment_df2 : tex_comment{'t} =
   't

dform tex_comment_item_df5 : mode[tex] :: "doc"{'t} =
   izone slot["raw", "%\n\\fi\\enmptab\\textrue\\iftex%\n"] ezone
   't
   izone slot["raw","%\n\\fi\\iftex\\bgmptab%\n"] ezone

dform tex_comment_white_df1 : mode[tex] :: comment_white =
   izone slot["raw", " "] ezone

dform tex_comment_white_df2 : mode[tex] :: xcons{comment_white; xcons{comment_white; 't}} =
   izone slot["raw", "\n\n"] ezone 't

dform tex_comment_white_df3 : mode[tex] :: xcons{comment_white; xcons{comment_white; xcons{comment_white; 't}}} =
   xcons{comment_white; xcons{comment_white; 't}}

(*
 * Plain version.
 *)
dform license_df : license{'t} = `""

(*
 * Spelling.
 *)
dform spelling_df1 : spelling{'t} = `""
dform misspelled_df1 : misspelled{'t} = 't

(*
 * PRL comments.
 *)
dform prl_comment_df : mode[prl] :: prl_comment{'t} =
   newline pushm[3] info["doc"] 't popm newline

dform normal_doc_df2 : mode[prl] :: "doc"{'t} =
   newline szone 't ezone

dform normal_doc_df4 : mode[html] :: mode[prl] :: "doc"{xcons{comment_white; 't}} =
   "doc"{'t}

dform prl_paragraph_df : mode[prl] :: xcons{comment_white; xcons{comment_white; 't}} =
   newline 't

dform prl_comment_white_df1 : mode[prl] :: comment_white =
   com_cbreak

(*
 * HTML comments.
 *)
dform html_comment_df : mode[html] :: html_comment{'t} =
   izone `"<!-- " ezone 't izone `" -->" ezone

dform normal_doc_df3 : mode[html] :: "doc"{'t} =
   izone `" -->" ezone 't izone `"<!-- " ezone

dform html_paragraph_df : mode[html] :: xcons{comment_white; xcons{comment_white; 't}} =
   izone slot["raw","\n<p style=\"doc\">"] ezone 't

dform html_comment_white_df1 : mode[html] :: comment_white =
   izone slot["raw", " "] ezone

(************************************************************************
 * COMMENT ITEMS                                                        *
 ************************************************************************)

doc <:doc<
   The @code{@theory} term produces a chapter header for a theory (e.g.
   a collection of modules) and the @code{@module[name:s]} produces a section
   header for the current module.
>>
declare "theory"{'t : Dform} : Dform
declare "module"[name:s] : Dform
declare "module"{'name : Dform} : Dform
doc docoff

dform theory_df1 : mode[tex] :: "theory"{'t} =
   izone `"\\theory{" ezone 't izone `"}" ezone

dform theory_df2 : mode[html] :: "theory"{'t} =
   html_head[1]{'t}

dform theory_df2 : except_mode[tex] :: "theory"{'t} =
   com_hbreak bf{'t} com_hbreak com_hbreak

dform module_df1 : mode[tex] :: "module"[name:s] =
   izone `"\\labelmodule{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform module_df2 : mode[html] :: "module"[name:s] =
   html_head[2]{html_anchor[name:s]{slot[name:s]}}

dform module_df3 : except_mode[tex] :: except_mode[html] :: "module"[name:s] =
   com_hbreak bf[name:s] com_hbreak com_hbreak

dform module_df4 : mode[tex] :: "module"{'name} =
   izone `"\\module{" ezone 'name izone `"}" ezone

dform module_df5 : mode[html] :: "module"{'name} =
   html_head[2]{'name}

dform module_df6 : except_mode[tex] :: except_mode[html] :: "module"{'name} =
   com_hbreak bf{'name} com_hbreak com_hbreak

doc <:doc<
   Bookmarking commands.
>>
declare chapter[name:s]{'t : Dform} : Dform
declare section[name:s]{'t : Dform} : Dform
declare subsection[name:s]{'t : Dform} : Dform
declare subsubsection[name:s]{'t : Dform} : Dform
doc docoff

dform chapter_df1 : mode[tex] :: "chapter"[name:s]{'t} =
   izone `"\\labelchapter{" slot[name:s] `"}{" ezone 't izone `"}" ezone

dform chapter_df2 : mode[html] :: "chapter"[name:s]{'t} =
   html_head[1]{html_anchor[name:s]{'t}}

dform chapter_df3 : except_mode[tex] :: except_mode[html] :: "chapter"[name:s]{'t} =
   com_hbreak bf{'t} com_hbreak com_hbreak

dform section_df1 : mode[tex] :: "section"[name:s]{'t} =
   izone `"\\labelsection{" slot[name:s] `"}{" ezone 't izone `"}" ezone

dform section_df2 : mode[html] :: "section"[name:s]{'t} =
   html_head[2]{html_anchor[name:s]{'t}}

dform section_df3 : except_mode[tex] :: except_mode[html] :: "section"[name:s]{'t} =
   com_hbreak bf{'t} com_hbreak com_hbreak

dform subsection_df1 : mode[tex] :: "subsection"[name:s]{'t} =
   izone `"\\labelsubsection{" slot[name:s] `"}{" ezone 't izone `"}" ezone

dform subsection_df2 : mode[html] :: "subsection"[name:s]{'t} =
   html_head[3]{html_anchor[name:s]{'t}}

dform subsection_df3 : except_mode[tex] :: except_mode[html] :: "subsection"[name:s]{'t} =
   com_hbreak bf{'t} com_hbreak com_hbreak

dform subsubsection_df1 : mode[tex] :: "subsubsection"[name:s]{'t} =
   izone `"\\labelsubsubsection{" slot[name:s] `"}{" ezone 't izone `"}" ezone

dform subsubsection_df2 : mode[html] :: "subsubsection"[name:s]{'t} =
   html_head[4]{html_anchor[name:s]{'t}}

dform subsubsection_df3 : except_mode[tex] :: except_mode[html] :: "subsubsection"[name:s]{'t} =
   com_hbreak bf{'t} com_hbreak com_hbreak

doc <:doc<
   The @code{@modsection} term produces a subsection header.
>>
declare modsection{'t : Dform} : Dform
doc docoff

dform modsection_df1 : mode[tex] :: modsection{'t} =
   izone `"\\modsection{" ezone 't izone `"}" ezone

dform modsection_df2 : mode[html] :: modsection{'t} =
   html_head[3]{'t}

dform modsection_df3 : except_mode[tex] :: except_mode[html] :: modsection{'t} =
   com_hbreak bf{'t} com_hbreak com_hbreak

doc <:doc<
   The @code{@modsubsection} term produces a sub-subsection header.
>>
declare modsubsection{'t : Dform} : Dform
doc docoff

dform modsubsection_df1 : mode[tex] :: modsubsection{'t} =
   izone `"\\modsubsection{" ezone 't izone `"}" ezone

dform modsubsection_df2 : mode[html] :: modsubsection{'t} =
   html_head[4]{'t}

dform modsubsection_df3 : except_mode[tex] :: except_mode[html] :: modsubsection{'t} =
   com_hbreak bf{'t} com_hbreak

doc <:doc<
   The @code{@paragraph} and @code{@subparagraph} terms produce paragraph and subparagraph headers.
>>
declare paragraph{'t : Dform} : Dform
declare subparagraph{'t : Dform} : Dform

doc docoff

dform paragraph_df1 : mode[tex] :: paragraph{'t} =
   izone `"\\paragraph{"  ezone 't izone `"}" ezone

dform paragraph_df2 : mode[html] :: paragraph{'t} =
   html_head[5]{'t}

dform paragraph_df3 : except_mode[tex] :: except_mode[html] :: paragraph{'t} =
   com_hbreak bf{'t}

dform subparagraph_df1 : mode[tex] :: subparagraph{'t} =
   izone `"\\subparagraph{" ezone 't izone `"}" ezone

dform subparagraph_df2 : mode[html] :: subparagraph{'t} =
   html_head[6]{'t}

dform subparagraph_df3 : except_mode[tex] :: except_mode[html] :: subparagraph{'t} =
   com_hbreak it{'t}

doc <:doc<
   Generic targets.
>>
declare target[name:s]{'t : Dform} : Dform
declare hreftarget[name:s] : Dform
doc docoff

dform target_df1 : mode[tex] :: target[name:s]{'t} =
   izone `"\\hreflabeltarget{" slot[name:s] `"}{" ezone 't izone `"}" ezone

dform target_df2 : except_mode[tex] :: target[name:s]{'t} =
   bf{'t}

dform hreftarget_df1 : mode[tex] :: "hreftarget"[name:s] =
   izone `"\\hreflabeltarget{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform hreftarget_df2 : except_mode[tex] :: "hreftarget"[name:s] =
   bf[name:s]

doc <:doc<
   The following terms generate the @emph{standard} @code{@modsection}
   headings for the commonly-defined parts of a module.
>>
declare parents : Dform
declare rewrites : Dform
declare rules : Dform
declare convs : Dform
declare tactics : Dform
declare resources : Dform
declare terms : Dform
doc docoff

dform parents_df1 : parents =
   modsection{comment_string["Parents"]}

dform rewrites_df1 : rewrites =
   modsection{comment_string["Rewrites"]}

dform rules_df1 : rules =
   modsection{comment_string["Rules"]}

dform convs_df1 : convs =
   modsection{comment_string["Conversions"]}

dform tactics_df1 : tactics =
   modsection{comment_string["Tactics"]}

dform resources_df1 : resources =
   modsection{comment_string["Resources"]}

dform terms_df1 : terms =
   modsection{comment_string["Terms"]}

doc <:doc<
   Structured comments are @emph{hypertext enabled}.  The following
   terms establish hypertext @emph{targets}.
>>
declare "term"[name:s] : Dform
declare "resource"[name:s] : Dform
declare "tactic"[name:s] : Dform
declare "conv"[name:s] : Dform
declare "rule"[name:s] : Dform
declare "rewrite"[name:s] : Dform
doc docoff

dform term_df1 : mode[tex] :: "term"[name:s] =
   izone `"\\labelterm{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform term_df2 : except_mode[tex] :: "term"[name:s] =
   bf[name:s]

dform resource_df1 : mode[tex] :: "resource"[name:s] =
   izone `"\\labelresource{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform resource_df2 : except_mode[tex] :: "resource"[name:s] =
   bf[name:s]

dform tactic_df1 : mode[tex] :: "tactic"[name:s] =
   izone `"\\labeltactic{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform tactic_df2 : except_mode[tex] :: "tactic"[name:s] =
   bf[name:s]

dform conv_df1 : mode[tex] :: "conv"[name:s] =
   izone `"\\labelconv{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform conv_df2 : except_mode[tex] :: "conv"[name:s] =
   bf[name:s]

dform rule_df1 : mode[tex] :: "rule"[name:s] =
   izone `"\\labelrule{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform rule_df1 : mode[tex] :: "rewrite"[name:s] =
   izone `"\\labelrewrite{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform rule_df2 : except_mode[tex] :: "rule"[name:s] =
   bf[name:s]

doc <:doc<
   The hypertext links are specified with the following terms.
>>
declare hrefmodule[name:s] : Dform
declare hrefmodule_internal[name:s] : Dform
declare hrefterm[name:s] : Dform
declare hrefresource[name:s] : Dform
declare hrefrewrite[name:s] : Dform
declare hreftactic[name:s] : Dform
declare hrefconv[name:s] : Dform
declare hrefrule[name:s] : Dform
doc docoff

ml_dform hrefmodule_df : hrefmodule[name:s] format_term buf = fun _ ->
   format_term buf Dform.NOParens (**)
      <:con<hrefmodule_internal[$String.capitalize (Dform.string_of_param name)$:s]>>

dform hrefmodule_internal_df1 : mode[tex] :: hrefmodule_internal[name:s] =
   izone `"\\hreflabelmodule{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform hrefmodule_internal_df2 : except_mode[tex] :: hrefmodule_internal[name:s] =
   bf[name:s]

dform hrefterm_df1 : mode[tex] :: hrefterm[name:s] =
   izone `"\\hreflabelterm{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform hrefterm_df2 : except_mode[tex] :: hrefterm[name:s] =
   bf[name:s]

dform hrefresource_df1 : mode[tex] :: hrefresource[name:s] =
   izone `"\\hreflabelresource{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform hrefresource_df2 : except_mode[tex] :: hrefresource[name:s] =
   bf[name:s]

dform hrefrewrite_df1 : mode[tex] :: hrefrewrite[name:s] =
   izone `"\\hreflabelrewrite{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform hrefrewrite_df2 : except_mode[tex] :: hrefrewrite[name:s] =
   bf[name:s]

dform hreftactic_df1 : mode[tex] :: hreftactic[name:s] =
   izone `"\\hreflabeltactic{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform hreftactic_df2 : except_mode[tex] :: hreftactic[name:s] =
   bf[name:s]

dform hrefconv_df1 : mode[tex] :: hrefconv[name:s] =
   izone `"\\hreflabelconv{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform hrefconv_df2 : except_mode[tex] :: hrefconv[name:s] =
   bf[name:s]

dform hrefrule_df1 : mode[tex] :: hrefrule[name:s] =
   izone `"\\hreflabelrule{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform hrefrule_df2 : except_mode[tex] :: hrefrule[name:s] =
   bf[name:s]

doc <:doc<
   The @emph{references} print section number of the target in
   a hypertext link.
>>
declare refchapter[name:s] : Dform
declare refsection[name:s] : Dform
declare refsubsection[name:s] : Dform
declare refsubsubsection[name:s] : Dform

declare refmodule[name:s] : Dform
declare refterm[name:s] : Dform
declare refresource[name:s] : Dform
declare refrewrite[name:s] : Dform
declare reftactic[name:s] : Dform
declare refconv[name:s] : Dform
declare refrule[name:s] : Dform
declare reffigure[name:s] : Dform
doc docoff

dform refchapter_df1 : mode[tex] :: refchapter[name:s] =
   izone `"\\reflabelchapter{" slot[name:s] `"}" ezone

dform refchapter_df2 : except_mode[tex] :: refchapter[name:s] =
   bf[name:s]

dform refsection_df1 : mode[tex] :: refsection[name:s] =
   izone `"\\reflabelsection{" slot[name:s] `"}" ezone

dform refsection_df2 : except_mode[tex] :: refsection[name:s] =
   bf[name:s]

dform refsubsection_df1 : mode[tex] :: refsubsection[name:s] =
   izone `"\\reflabelsubsection{" slot[name:s] `"}" ezone

dform refsubsection_df2 : except_mode[tex] :: refsubsection[name:s] =
   bf[name:s]

dform refsubsubsection_df1 : mode[tex] :: refsubsubsection[name:s] =
   izone `"\\reflabelsubsubsection{" slot[name:s] `"}" ezone

dform refsubsubsection_df2 : except_mode[tex] :: refsubsubsection[name:s] =
   bf[name:s]

dform refmodule_df1 : mode[tex] :: refmodule[name:s] =
   izone `"\\reflabelmodule{" slot[name:s] `"}" ezone

dform refmodule_df2 : except_mode[tex] :: refmodule[name:s] =
   bf[name:s]

dform refterm_df1 : mode[tex] :: refterm[name:s] =
   izone `"\\refterm{" slot[name:s] `"}" ezone

dform refterm_df2 : except_mode[tex] :: refterm[name:s] =
   bf[name:s]

dform refresource_df1 : mode[tex] :: refresource[name:s] =
   izone `"\\refresource{" slot[name:s] `"}" ezone

dform refresource_df2 : except_mode[tex] :: refresource[name:s] =
   bf[name:s]

dform refrewrite_df1 : mode[tex] :: refrewrite[name:s] =
   izone `"\\refrewrite{" slot[name:s] `"}" ezone

dform refrewrite_df2 : except_mode[tex] :: refrewrite[name:s] =
   bf[name:s]

dform reftactic_df1 : mode[tex] :: reftactic[name:s] =
   izone `"\\reftactic{" slot[name:s] `"}" ezone

dform reftactic_df2 : except_mode[tex] :: reftactic[name:s] =
   bf[name:s]

dform refconv_df1 : mode[tex] :: refconv[name:s] =
   izone `"\\refconv{" slot[name:s] `"}" ezone

dform refconv_df2 : except_mode[tex] :: refconv[name:s] =
   bf[name:s]

dform refrule_df1 : mode[tex] :: refrule[name:s] =
   izone `"\\refrule{" slot[name:s] `"}" ezone

dform refrule_df2 : except_mode[tex] :: refrule[name:s] =
   bf[name:s]

dform reffigure_df1 : mode[tex] :: reffigure[name:s] =
   izone `"\\reffigure{" slot[name:s] `"}" ezone

dform reffigure_df2 : except_mode[tex] :: reffigure[name:s] =
   bf[name:s]

(*
 * TeX structuring.
 *)
declare "begin"[name:s] : Dform
declare "end"[name:s] : Dform

dform begin_df1 : mode[tex] :: "begin"[name:s] =
   izone `"\\begin{" slot[name:s] `"}" ezone

dform begin_df2 : except_mode[tex] :: "begin"[name:s] =
   `""

dform end_df1 : mode[tex] :: "end"[name:s] =
   izone `"\\end{" slot[name:s] `"}" ezone

dform end_df2 : except_mode[tex] :: "end"[name:s] =
   `""

doc <:doc<
   The @code{@noindent} term specifies that the following paragraph
   should not include the usual indentation for the first line.
>>
declare noindent : Dform
doc docoff

dform noindent_df1 : mode[tex] :: noindent =
   izone `"\\noindent{}" ezone

dform noindent_df2 : except_mode[tex] :: noindent =
   `""

doc <:doc<
   The @code{@cite} term represents a @emph{citation}.  The citation
   only has effect on @LaTeX display mode.
>>
declare cite[s:s] : Dform
doc docoff

dform cite_df1 : mode[tex] :: cite[text:s] =
   izone `"\\cite{" slot[text:s] `"}" ezone

dform cite_df2 : except_mode[tex] :: cite[text:s] =
   `"cite{" slot[text:s] `"}"

doc <:doc<
   The @code{phantom} term produces white space, equivalent in width
   to the term being typeset.
>>
declare phantom{'t : Dform} : Dform
doc docoff

dform phantom_df1 : mode[tex] :: phantom{'t} =
   izone `"\\phantom{" 't `"}" ezone

dform phantom_df2 : except_mode[tex] :: phantom{'t} =
   `" "

doc <:doc<
   The following color commands use named colors.  You need to define
   these colors using @code{\definecolor} in your preamble.
>>
declare color[name:s] : Dform
declare pagecolor[name:s] : Dform
declare colorbox[name:s]{'t : Dform} : Dform
doc docoff

dform color_df1 : mode[tex] :: color[name:s] =
   izone `"\\color{" slot[name:s] `"}" ezone

dform color_df2 : except_mode[tex] :: color[name:s] =
   `""

dform pagecolor_df1 : mode[tex] :: pagecolor[name:s] =
   izone `"\\color{" slot[name:s] `"}" ezone

dform pagecolor_df2 : except_mode[tex] :: pagecolor[name:s] =
   `""

dform colorbox_df1 : mode[tex] :: colorbox[name:s]{'t} =
   izone `"\\colorbox{" slot[name:s] `"}{" ezone
   't
   izone `"}\n" ezone

dform colorbox_df2 : except_mode[tex] :: colorbox[name:s]{'t} =
   't

doc <:doc<
   @emph{Math mode} is entered with the @code{$text$} and @code{$$text$$}
   forms.  The @code{$text$} form produces a @tt{math} term, and the
   @code{$$text$$} form produces a @tt{centermath} term.
>>
declare math[s:s] : Dform
declare math{'t : Dform} : Dform
declare centermath[s:s] : Dform
declare centermath{'t : Dform} : Dform
doc docoff

dform math_df1 : math[s:s] = math{slot[s:s]}

dform math_df2 : mode[tex] :: math{'t} =
   lzone ensuremath{'t} ezone

dform math_df3 : except_mode[tex] :: math{'t} =
   it{'t}

dform centermath_df1 : centermath[s:s] = centermath{slot[s:s]}

dform centermath_df2 : mode[tex] :: centermath{'t} =
   izone `"$$" ezone lzone 't ezone izone `"$$" ezone

dform centermath_df3 : mode[prl] :: centermath{'t} =
   newline slot["raw", "    "] pushm[0] it{'t} popm newline

dform centermath_df4 : mode[html] :: centermath{'t} =
   izone `"<p class=\"centermath\">" ezone 't izone `"</p>" ezone

doc <:doc<
   The @code{@code} form produces literal text.
   The literal text is enclosed in curly brackets.
>>
declare code[text:s] : Dform
doc docoff

dform code_df1 : mode[tex] :: code[s:s] =
   izone `"\\verb}" slot[s:s] `"}" ezone

dform code_df2 : mode[html] :: code[s:s] =
   izone `"<pre>" slot[s:s] `"</pre>" ezone

dform code_df3 : except_mode[tex] :: except_mode[html] :: code[s:s] =
   tt[s:s]

doc <:doc<
     The @tt[minipage] term encloses a block of text that should be
     treated like a page.
>>
declare minipage[width:s]{'t : Dform} : Dform
declare minipage[width:s,pos:s]{'t : Dform} : Dform
doc docoff

dform minipage_df1 : mode[tex] :: minipage[width:s]{'t} =
   izone `"\\begin{minipage}{" slot[width:s] `"}" ezone
   't
   izone `"\\end{minipage}" ezone

dform minipage_df2 : mode[tex] :: minipage[width:s,pos:s]{'t} =
   izone `"\\begin{minipage}[" slot[pos:s] `"]{" slot[width:s] `"}" ezone
   't
   izone `"\\end{minipage}" ezone

dform minipage_df3 : except_mode[tex] :: minipage[width:s]{'t} =
   't

dform minipage_df4 : except_mode[tex] :: minipage[width:s,pos:s]{'t} =
   't

doc <:doc<
   The @tt[verbatim] term encloses a block of verbatim text.
>>
declare verbatim[text:s] : Dform
doc docoff

dform verbatim_df1 : mode[tex] :: verbatim[s:s] =
   izone `"\\begin{verbatim}\n" slot["raw", s:s] `"\n\\end{verbatim}" ezone

dform verbatim_df2 : except_mode[tex] :: verbatim[s:s] =
   tt[s:s]

doc <:doc<
   The @tt[iverbatim] term encloses a block of verbatim text, indented.
>>
declare iverbatim[text:s] : Dform
doc docoff

dform iverbatim_df1 : mode[tex] :: iverbatim[s:s] =
   izone `"\n\n\\begin{shadebox}\\renewcommand\\baselinestretch{1.0}\\begin{verbatim}\n"
   slot["raw", s:s]
   `"\n\\end{verbatim}\\end{shadebox}\n\n" ezone

dform iverbatim_df2 : except_mode[tex] :: iverbatim[s:s] =
   tt[s:s]

doc <:doc<
   The @code{@email} form is similar to the @code{@code} form,
   but it is normally used to represent an E-mail address.
>>
declare email[text:s] : Dform
doc docoff

dform email_df1 : mode[tex] :: email[s:s] =
   izone `"\\verb}" slot[s:s] `"}" ezone

dform email_df2 : except_mode[tex] :: email[s:s] =
   tt[s:s]

doc <:doc<
   Text can be centered with the @code{@center} form.
   The usual usage is as a begin/end pair.

   @begin[verbatim]
   @begin[center]
   text
   @end[center]
   @end[verbatim]

   Each line of the text in the @tt{center} block is centered.
>>
declare center{'t : Dform} : Dform
doc docoff

dform center_df1 : mode[tex] :: center{'t} =
   izone `"\\begin{center}" ezone 't izone `"\\end{center}" ezone

dform center_df2 : mode[html] :: center{'t} =
   izone `"<center>" ezone 't izone `"</center>" ezone

dform center_df3 : except_mode[tex] :: except_mode[html] :: center{'t} =
   hspace 't hspace

doc <:doc<
   A block of text can be placed in a figure.

   @begin[verbatim]
   @begin[figure,label]
   text
   @end[figure]
   @end[verbatim]
>>
declare figure[label:s]{'t : Dform} : Dform
declare figure[label:s,pos:s]{'t : Dform} : Dform
declare caption{'caption : Dform} : Dform
doc docoff

dform figure_df1 : mode[tex] :: figure[label:s]{'t} =
   izone `"\\begin{figure}" ezone
   't
   izone `"\\labelfigure{" ezone slot[label:s]
   izone `"}\\end{figure}" ezone

dform figure_df2 : mode[tex] :: figure[label:s,pos:s]{'t} =
   izone `"\\begin{figure}[" slot[pos:s] `"]" ezone
   't
   izone `"\\labelfigure{" ezone slot[label:s]
   izone `"}\\end{figure}" ezone

dform figure_df3 : except_mode[tex] :: figure[label:s]{'t} =
   hspace 't hspace

dform figure_df4 : except_mode[tex] :: figure[label:s,pos:s]{'t} =
   hspace 't hspace

dform caption_df1 : mode[tex] :: caption{'caption} =
   izone `"\\caption{" ezone
   'caption
   izone `"}" ezone

dform caption_df2 : except_mode[tex] :: caption{'caption} =
   `"caption: " 'caption

doc <:doc<
   Add extra indentation with the @code{@quote} form.
   The usual usage is as a begin/end pair.

   @begin[verbatim]
   @begin[indent]
   text
   @end[indent]
   @end[verbatim]

   Each line of the text in the @tt{indent} block is indented.
>>
declare indent{'t : Dform} : Dform
doc docoff

dform indent_df1 : mode[tex] :: indent{'t} =
   izone `"\\begin{indent}" ezone 't izone `"\\end{indent}" ezone

dform indent_df2 : except_mode[tex] :: indent{'t} =
   hspace 't hspace

doc <:doc<
   Quotations can be centered with the @code{@quote} form.
   The usual usage is as a begin/end pair.

   @begin[verbatim]
   @begin[quote]
   text
   @end[quote]
   @end[verbatim]

   Each line of the text in the @tt{quote} block is centered.
>>
declare quote{'t : Dform} : Dform
declare quotation{'t : Dform} : Dform
doc docoff

dform quote_df1 : mode[tex] :: quote{'t} =
   izone `"\\begin{quote}" ezone 't izone `"\\end{quote}" ezone

dform quote_df2 : except_mode[tex] :: quote{'t} =
   hspace 't hspace

dform quotation_df1 : mode[tex] :: quotation{'t} =
   izone `"\\begin{quotation}" ezone 't izone `"\\end{quotation}" ezone

dform quotation_df2 : except_mode[tex] :: quotation{'t} =
   hspace 't hspace

doc <:doc<
   Footnotes use the @code{@footnote} form.
>>
declare footnote{'t : Dform} : Dform
doc docoff

dform footnote_df1 : mode[tex] :: footnote{'t} =
   izone `"\\footnote{" ezone 't izone `"}" ezone

dform footnote_df2 : except_mode[tex] :: footnote{'t} =
   hspace 't hspace

doc <:doc<
   Lists can be declared in three forms.  The @tt{enumerate} form
   numbers the elements of the list; the @tt{itemize} form places a
   bullet before each item; and the @tt{description} form takes
   items that have optional labels.  The @code{@item} form
   is used to enclose each item in the list.  For example, the
   following code produces the first four letters of the alphabet
   preceded by the first four natural numbers (respectively).

   @begin[verbatim]
   @begin[enumerate]
   @item{A}
   @item{B}
   @item{C}
   @item{D}
   @end[enumerate]
   @end[verbatim]

   For the @tt{description} lists, the @code{@item} term
   takes two arguments; the first is the @emph{label} of the
   entry, and the second is the @emph{body}.
>>
declare item{'t : Dform} : Dform
declare item{'label; 'body} : Dform
doc docoff

dform item_df2 : except_mode[tex] :: item{'t} =
   com_hbreak pushm[3] `"o " 't popm

dform item_df2 : except_mode[tex] :: item{'t1; 't2} =
   com_hbreak pushm[3] `"[" 't1 `"]" 't2 popm

dform item_df1 : mode[tex] :: item{'t} =
   izone `"\\item{}" ezone 't

dform item_df1 : mode[tex] :: item{'t1; 't2} =
   izone `"\\item[" ezone 't1 izone `"]" ezone 't2

doc docon
declare enumerate{'t : Dform} : Dform
doc docoff
declare normal_enumerate{'count : Dform; 't : Dform} : Dform

dform enumerate_df2 : except_mode[tex] :: enumerate{'t} =
   normal_enumerate{xcons{xnil; xnil}; 't}

dform normal_enumerate_df1 : normal_enumerate{'count; xnil} =
   com_hbreak

dform normal_enumerate_df3 : normal_enumerate{'count; xcons{comment_white; 'tl}} =
   normal_enumerate{'count; 'tl}

dform normal_enumerate_df4 : normal_enumerate{'count; xcons{item{'t}; 'tl}} =
   com_hbreak pushm[3] df_length{'count} `"." 't popm
   normal_enumerate{xcons{xnil; 'count}; 'tl}

dform normal_enumerate_df5 : normal_enumerate{'count; xcons{item{'t1; 't2}; 'tl}} =
   com_hbreak pushm[3] 't1 `"." 't2 popm
   normal_enumerate{xcons{xnil; 'count}; 'tl}

dform enumerate_df1 : mode[tex] :: enumerate{'t} =
   izone `"\\begin{enumerate}" ezone 't izone `"\\end{enumerate}" ezone

doc docon
declare itemize{'t : Dform} : Dform
doc docoff

dform itemize_df1 : mode[tex] :: itemize{'t} =
   izone `"\\begin{itemize}" ezone 't izone `"\\end{itemize}" ezone

dform itemize_df2 : except_mode[tex] :: itemize{'t} =
   't

doc docon
declare description{'t : Dform} : Dform
doc docoff

dform description_df1 : mode[tex] :: description{'t} =
   izone `"\\begin{description}" ezone 't izone `"\\end{description}" ezone

dform description_df2 : except_mode[tex] :: description{'t} =
   normal_enumerate{xcons{xnil; xnil}; 't}

doc <:doc< Other macros >>
declare lbrace : Dform
declare rbrace : Dform
declare comment[who:s]{'e : Dform} : Dform
doc docoff

dform lbrace_df1 : mode[tex] :: lbrace =
   izone `"\\{" ezone

dform lbrace_df2 : except_mode[tex] :: lbrace =
   "{"

dform rbrace_df1 : mode[tex] :: rbrace =
   izone `"\\}" ezone

dform lbrace_df2 : except_mode[tex] :: rbrace =
   "}"

dform comment_df1 : mode[tex] :: comment[who:s]{'e} =
   izone `"\\comment{" ezone bf[who:s] `": " 'e izone `"}" ezone

dform comment_df2 : except_mode[tex] :: comment[who:s]{'e} =
   szone pushm[3] bf["Comment: "] bf[who:s] hspace 'e popm ezone

(************************************************************************
 * MATH MODE                                                            *
 ************************************************************************)

(*
 * Toplevel form.
 *)
declare math_misspelled{'t : Dform} : Dform

dform math_misspelled_df1 : math_misspelled{'t} =
   't

(*
 * Allow raw printing.
 *)
declare math_slot[tag:s]{'t : Dform} : Dform

dform math_slot_df1 : math_slot[tag:s]{'t} =
   slot[tag:s]{'t}

doc <:doc<
   @modsubsection{Math mode}

   Terms are formatted in @emph{math mode} if they are
   placed between matching @tt["$"] symbols (for inline
   math expressions), or matching @tt["$$"] symbols (for
   centered math expressions).  All terms in math mode
   have an @tt[opname] that begins with the prefix @tt{math_}.

   The following terms define standard forms in math mode.

   The @tt{math_mathop} and @tt[math_mathrel] terms give their
   contents the status of an ``operator'' or a ``relation.''  The
   significance has to do with spacing in math mode.  An operator is
   always followed by extra white space, and a relation is surrounded
   by extra white space.

   The @tt[tt] term displays its contents in a @tt{fixed-width} font;
   the @tt[bf] term displays the contents in a @bf{bold font}; the
   @tt[i] and @tt[it] terms display their contents in an
   @i[italic] font; and the @tt[emph] term @emph{emphasizes} its
   contents.
>>
declare math_mbox{'t : Dform} : Dform
declare math_hbox{'t : Dform} : Dform

declare math_mathop{'t : Dform} : Dform
declare math_mathop[text:s] : Dform
declare math_mathrel{'t : Dform} : Dform
declare math_mathrel[text:s] : Dform
declare math_bb{'T} : Dform
declare math_bb[TEXT:s] : Dform
declare math_tt{'t : Dform} : Dform
declare math_tt[text:s] : Dform
declare math_bf{'t : Dform} : Dform
declare math_bf[text:s] : Dform
declare math_i{'t : Dform} : Dform
declare math_i[text:s] : Dform
declare math_emph{'t : Dform} : Dform
doc docoff

dform math_hbox_df1 : mode[tex] :: math_hbox{'t} =
   izone `"\\hbox{" ezone 't izone `"}" ezone

dform math_hbox_df2 : except_mode[tex] :: math_hbox{'t} =
   't

dform math_mbox_df1 : mode[tex] :: math_mbox{'t} =
   izone `"\\mbox{" ezone 't izone `"}" ezone

dform math_mbox_df2 : except_mode[tex] :: math_mbox{'t} =
   slot{'t}

dform math_mathop_df1 : mode[tex] :: math_mathop{'t} =
   izone `"\\mathop{\\bf " ezone 't izone `"}" ezone

dform math_mathop_df1 : mode[tex] :: math_mathop[text:s] =
   izone `"\\mathop{\\bf " ezone slot[text:s] izone `"}" ezone

dform math_mathop_df2 : except_mode[tex] :: math_mathop{'t} =
   bf{'t}

dform math_mathop_df2 : except_mode[tex] :: math_mathop[text:s] =
   bf[text:s]

dform math_mathrel_df1 : mode[tex] :: math_mathrel[text:s] =
   izone `"\\mathrel{\\bf " ezone slot[text:s] izone `"}" ezone

dform math_mathrel_df2 : except_mode[tex] :: math_mathrel[text:s] =
   bf[text:s]

dform math_mathrel_df3 : mode[tex] :: math_mathrel{'t} =
   izone `"\\mathrel{\\bf " ezone 't izone `"}" ezone

dform math_mathrel_df4 : except_mode[tex] :: math_mathrel{'t} =
   bf{'t}

dform math_tt_df1 : mode[tex] :: math_tt{'t} =
   izone `"\\mathtt{" ezone 't izone `"}" ezone

dform math_tt_df1 : mode[tex] :: math_tt[text:s] =
   izone `"\\mathtt{" ezone slot[text:s] izone `"}" ezone

dform math_tt_df2 : except_mode[tex] :: math_tt{'t} =
   tt{'t}

dform math_tt_df2 : except_mode[tex] :: math_tt[text:s] =
   tt[text:s]

dform math_bb_df1 : mode[tex] :: math_bb{'t} =
   izone `"\\mathbb{" ezone 't izone `"}" ezone

dform math_bb_df1 : mode[tex] :: math_bb[text:s] =
   izone `"\\mathbb{" ezone slot[text:s] izone `"}" ezone

dform math_bb_df2 : except_mode[tex] :: math_bb{'t} =
   bf{'t}

dform math_bb_df2 : except_mode[tex] :: math_bb[text:s] =
   bf[text:s]

dform math_bf_df1 : mode[tex] :: math_bf{'t} =
   izone `"\\mathbf{" ezone 't izone `"}" ezone

dform math_bf_df1 : mode[tex] :: math_bf[text:s] =
   izone `"\\mathbf{ " ezone slot[text:s] izone `"}" ezone

dform math_bf_df2 : except_mode[tex] :: math_bf{'t} =
   bf{'t}

dform math_bf_df2 : except_mode[tex] :: math_bf[text:s] =
   bf[text:s]

dform math_i_df1 : math_i{'t} =
   math_it{'t}

dform math_i_df2 : math_i[text:s] =
   math_it[text:s]

dform math_emph_df1 : mode[tex] :: math_emph{'t} =
   izone `"\\mathit{" ezone 't izone `"}" ezone

dform math_emph_df2 : except_mode[tex] :: math_emph{'t} =
   emph{'t}

dform math_underline_df : math_underline{'t} = underline{'t}

(*
 * Math symbols.
 *)
declare math_Type : Dform

doc <:doc<
   The following terms define some common math symbols.
>>
declare math_colon : Dform
declare math_rightarrow : Dform
declare math_Rightarrow : Dform
declare math_leftarrow : Dform
declare math_Leftarrow : Dform
declare math_leftrightarrow : Dform
declare math_Leftrightarrow : Dform
declare math_longrightarrow : Dform
declare math_longleftrightarrow : Dform

declare math_le : Dform
declare math_ge : Dform
declare math_wedge : Dform
declare math_vee : Dform
declare math_phi : Dform
declare math_varphi : Dform
declare math_cap : Dform
declare math_cup : Dform
declare math_bigcap : Dform
declare math_bigcup : Dform
declare math_in : Dform
declare math_cdot : Dform
declare math_cdots : Dform
declare math_vdots : Dform
declare math_ldots : Dform
declare math_subset : Dform
declare math_subseteq : Dform
declare math_times : Dform
declare math_equiv : Dform
declare math_space : Dform
declare math_neg : Dform
declare math_neq : Dform
declare math_forall : Dform
declare math_exists : Dform
declare math_alpha : Dform
declare math_beta : Dform
declare math_lambda : Dform
declare math_epsilon : Dform
declare math_Gamma : Dform
declare math_Delta : Dform
declare math_vdash : Dform
declare math_int : Dform
declare math_lbrace : Dform
declare math_rbrace : Dform
declare math_lfloor : Dform
declare math_rfloor : Dform
declare math_quad : Dform
declare math_qquad : Dform
declare math_bullet : Dform
declare math_left[s] : Dform
declare math_right[s] : Dform

declare math_vec{'e : Dform} : Dform
declare math_underbrace{'e : Dform} : Dform
declare math_underbrace{'e1 : Dform; 'e2 : Dform} : Dform
doc docoff

dform math_Type_df1 : math_Type =
   math_space math_tt["Type"]

dform math_colon_df1 : mode[tex] :: math_colon =
   izone `"\\colon " ezone

dform math_colon_df2 : except_mode[tex] :: math_colon =
   `":"

dform math_vee_df1 : mode[tex] :: math_vee =
   izone `"\\vee " ezone

dform math_vee_df2 : except_mode[tex] :: math_vee =
   Mpsymbols!vee

dform math_phi_df1 : mode[tex] :: math_phi =
   izone `"\\phi " ezone

dform math_phi_df2 : except_mode[tex] :: math_phi =
   Mpsymbols!phi

dform math_varphi_df1 : mode[tex] :: math_varphi =
   izone `"\\varphi " ezone

dform math_varphi_df2 : except_mode[tex] :: math_varphi =
   Mpsymbols!phi

dform math_wedge_df1 : mode[tex] :: math_wedge =
   izone `"\\wedge " ezone

dform math_wedge_df2 : except_mode[tex] :: math_wedge =
   Mpsymbols!wedge

dform math_rightarrow_df1 : mode[tex] :: math_rightarrow =
   izone `"\\rightarrow " ezone

dform math_rightarrow_df2 : except_mode[tex] :: math_rightarrow =
   Mpsymbols!rightarrow

dform math_Rightarrow_df1 : mode[tex] :: math_Rightarrow =
   izone `"\\Rightarrow " ezone

dform math_Rightarrow_df2 : except_mode[tex] :: math_Rightarrow =
   Mpsymbols!Rightarrow

dform math_leftarrow_df1 : mode[tex] :: math_leftarrow =
   izone `"\\leftarrow " ezone

dform math_leftarrow_df2 : except_mode[tex] :: math_leftarrow =
   Mpsymbols!leftarrow

dform math_Leftarrow_df1 : mode[tex] :: math_Leftarrow =
   izone `"\\Leftarrow " ezone

dform math_Leftarrow_df2 : except_mode[tex] :: math_Leftarrow =
   Mpsymbols!Leftarrow

dform math_leftrightarrow_df1 : mode[tex] :: math_leftrightarrow =
   izone `"\\leftrightarrow " ezone

dform math_leftrightarrow_df2 : except_mode[tex] :: math_leftrightarrow =
   Mpsymbols!shortleftrightarrow

dform math_Leftrightarrow_df1 : mode[tex] :: math_Leftrightarrow =
   izone `"\\Leftrightarrow " ezone

dform math_Leftrightarrow_df2 : except_mode[tex] :: math_Leftrightarrow =
   Mpsymbols!Leftrightarrow

dform math_longrightarrow_df1 : mode[tex] :: math_longrightarrow =
   izone `"\\longrightarrow " ezone

dform math_longrightarrow_df2 : except_mode[tex] :: math_longrightarrow =
   Mpsymbols!longrightarrow

dform math_longleftrightarrow_df1 : mode[tex] :: math_longleftrightarrow =
   izone `"\\longleftrightarrow " ezone

dform math_longleftrightarrow_df2 : except_mode[tex] :: math_longleftrightarrow =
   Mpsymbols!longleftrightarrow

dform math_cap_df1 : mode[tex] :: math_cap =
   izone `"\\cap " ezone

dform math_cap_df2 : except_mode[tex] :: math_cap =
   Mpsymbols!cap

dform math_cup_df1 : mode[tex] :: math_cup =
   izone `"\\cup " ezone

dform math_cup_df2 : except_mode[tex] :: math_cup =
   Mpsymbols!cup

dform math_bigcap_df1 : mode[tex] :: math_bigcap =
   izone `"\\bigcap " ezone

dform math_bigcap_df2 : except_mode[tex] :: math_bigcap =
   Mpsymbols!cap

dform math_bigcup_df1 : mode[tex] :: math_bigcup =
   izone `"\\bigcup " ezone

dform math_bigcup_df2 : except_mode[tex] :: math_bigcup =
   Mpsymbols!cup

dform math_in_df1 : mode[tex] :: math_in =
   izone `"\\in " ezone

dform math_in_df2 : except_mode[tex] :: math_in =
   Mpsymbols!member

dform math_le_df1 : mode[tex] :: math_le =
   izone `"\\le " ezone

dform math_in_df2 : except_mode[tex] :: math_le =
   Mpsymbols!le

dform math_ge_df1 : mode[tex] :: math_ge =
   izone `"\\ge " ezone

dform math_in_df2 : except_mode[tex] :: math_ge =
   Mpsymbols!ge

dform math_cdot_df1 : mode[tex] :: math_cdot =
   izone `"\\cdot " ezone

dform math_cdot_df2 : except_mode[tex] :: math_cdot =
   Mpsymbols!cdot

dform math_cdots_df1 : mode[tex] :: math_cdots =
   izone `"\\cdots " ezone

dform math_cdots_df2 : except_mode[tex] :: math_cdots =
   `"..."

dform math_vdots_df1 : mode[tex] :: math_vdots =
   izone `"\\vdots " ezone

dform math_vdots_df2 : except_mode[tex] :: math_vdots =
   `"..."

dform math_ldots_df1 : mode[tex] :: math_ldots =
   izone `"\\ldots " ezone

dform math_ldots_df2 : except_mode[tex] :: math_ldots =
   `"..."

dform math_subset_df1 : mode[tex] :: math_subset =
   izone `"\\subset " ezone

dform math_subset_df2 : except_mode[tex] :: math_subset =
   Mpsymbols!"subset"

dform math_subseteq_df1 : mode[tex] :: math_subseteq =
   izone `"\\subseteq " ezone

dform math_subseteq_df2 : except_mode[tex] :: math_subseteq =
   Mpsymbols!subseteq

dform math_times_df1 : mode[tex] :: math_times =
   izone `"\\times " ezone

dform math_times_df2 : except_mode[tex] :: math_times =
   Mpsymbols!times

dform math_equiv_df1 : mode[tex] :: math_equiv =
   izone `"\\equiv " ezone

dform math_equiv_df2 : except_mode[tex] :: math_equiv =
   Mpsymbols!equiv

dform math_space_df1 : mode[tex] :: math_space =
   izone `"\\ " ezone

dform math_space_df2 : except_mode[tex] :: math_space =
   " "

dform math_lbrace_df1 : mode[tex] :: math_lbrace =
   izone `"\\{" ezone

dform math_lbrace_df2 : except_mode[tex] :: math_lbrace =
   "{"

dform math_rbrace_df1 : mode[tex] :: math_rbrace =
   izone `"\\}" ezone

dform math_rbrace_df2 : except_mode[tex] :: math_rbrace =
   "}"

dform math_lfloor_df1 : mode[tex] :: math_lfloor =
   izone `"\\lfloor" ezone

dform math_lfloor_df2 : except_mode[tex] :: math_lfloor =
   `"|_"

dform math_rfloor_df1 : mode[tex] :: math_rfloor =
   izone `"\\rfloor" ezone

dform math_rfloor_df2 : except_mode[tex] :: math_rfloor =
   `"_|"

dform math_left_df1 : mode[tex] :: math_left[s:s] =
   izone `"\\left" slot[s:s] `" " ezone

dform math_left_df2 : except_mode[tex] :: math_left[s:s] =
   slot[s:s]

dform math_right_df1 : mode[tex] :: math_right[s:s] =
   izone `"\\right" slot[s:s] `" " ezone

dform math_right_df2 : except_mode[tex] :: math_right[s:s] =
   slot[s:s]

dform math_quad_df1 : mode[tex] :: math_quad =
   izone `"\\quad " ezone

dform math_quad_df2 : except_mode[tex] :: math_quad =
   izone `"    " ezone

dform math_qquad_df1 : mode[tex] :: math_qquad =
   izone `"\\qquad " ezone

dform math_qquad_df2 : except_mode[tex] :: math_qquad =
   izone `"      " ezone

dform math_bullet_df1 : mode[tex] :: math_bullet =
   izone `"\\bullet " ezone

dform math_bullet_df2 : except_mode[tex] :: math_bullet =
   izone `"      " ezone

dform math_neg_df1 : mode[tex] :: math_neg =
   izone `"\\neg " ezone

dform math_neg_df2 : except_mode[tex] :: math_neg =
   Mpsymbols!tneg

dform math_neq_df1 : mode[tex] :: math_neq =
   izone `"\\neq " ezone

dform math_neq_df2 : except_mode[tex] :: math_neq =
   Mpsymbols!neq

dform math_forall_df1 : mode[tex] :: math_forall =
   izone `"\\forall " ezone

dform math_forall_df2 : except_mode[tex] :: math_forall =
   Mpsymbols!"forall"

dform math_exists_df1 : mode[tex] :: math_exists =
   izone `"\\exists " ezone

dform math_exists_df2 : except_mode[tex] :: math_exists =
   Mpsymbols!"exists"

dform math_alpha_df1 : mode[tex] :: math_alpha =
   izone `"\\alpha " ezone

dform math_alpha_df2 : except_mode[tex] :: math_alpha =
   Mpsymbols!alpha

dform math_beta_df1 : mode[tex] :: math_beta =
   izone `"\\beta " ezone

dform math_beta_df2 : except_mode[tex] :: math_beta =
   Mpsymbols!beta

dform math_Gamma_df1 : mode[tex] :: math_Gamma =
   izone `"\\Gamma " ezone

dform math_Gamma_df2 : except_mode[tex] :: math_Gamma =
   Mpsymbols!gamma

dform math_Delta_df1 : mode[tex] :: math_Delta =
   izone `"\\Delta " ezone

dform math_Delta_df2 : except_mode[tex] :: math_Delta =
   Mpsymbols!gamma

dform math_vdash_df1 : mode[tex] :: math_vdash =
   izone `"\\vdash " ezone

dform math_vdash_df2 : except_mode[tex] :: math_vdash =
   Mpsymbols!vdash

dform math_epsilon_df1 : mode[tex] :: math_epsilon =
   izone `"\\epsilon " ezone

dform math_epsilon_df2 : except_mode[tex] :: math_epsilon =
   Mpsymbols!epsilon

dform math_lambda_df1 : mode[tex] :: math_lambda =
   izone `"\\lambda " ezone

dform math_lambda_df2 : except_mode[tex] :: math_lambda =
   Mpsymbols!lambda

dform math_int_df1 : mode[tex] :: math_int =
   izone `"{\\mathbb Z}" ezone

dform math_int_df2 : except_mode[tex] :: math_int =
   Mpsymbols!mathbbZ

dform math_vec_df1 : mode[tex] :: math_vec{'e} =
   izone `"\\vec{" ezone 'e izone `"}" ezone

dform math_vec_df2 : except_mode[tex] :: math_vec{'e} =
   `"vector[" 'e `"]"

dform math_underbrace_df1 : mode[tex] :: math_underbrace{'e} =
   izone `"\\underbrace{" ezone 'e izone `"}" ezone

dform math_underbrace_df2 : except_mode[tex] :: math_underbrace{'e} =
   `"underbrace[" 'e `"]"

dform math_underbrace_df3 : mode[tex] :: math_underbrace{'e1; 'e2} =
   izone `"\\underbrace{" ezone 'e1 izone `"}_{" ezone 'e2 izone `"}" ezone

dform math_underbrace_df4 : except_mode[tex] :: math_underbrace{'e1; 'e2} =
   `"underbrace[" 'e1 `"," 'e2 `"]"

doc <:doc<
   The `_' and `^' characters are @emph{significant} in math mode
   (they are plain text in normal mode).  The expression @code{s_t}
   produces the @tt{subscript} term, and the expression @code{s^t}
   produces the @tt{superscript} term.
>>
declare math_subscript{'t1 : Dform; 't2 : Dform} : Dform
declare math_superscript{'t1 : Dform; 't2 : Dform} : Dform
doc docoff

dform tex_math_subscript_df1 : mode[tex] :: math_subscript{'t1; 't2} =
   izone `"{" ezone
   't1
   izone `"}_{" ezone
   't2
   izone `"}" ezone

dform tex_math_superscript_df1 : mode[tex] :: math_superscript{'t1; 't2} =
   izone `"{" ezone
   't1
   izone `"}^{" ezone
   't2
   izone `"}" ezone

dform normal_math_subscript_df1 : except_mode[tex] :: math_subscript{'t1; 't2} =
   't1 `"_" 't2

dform normal_math_superscript_df1 : except_mode[tex] :: math_superscript{'t1; 't2} =
   't1 `"^" 't2

doc <:doc<
   The @tt{array} and tabular forms produce formatted tables.
   All @emph{rows} in the table are labeled with the @tt{line}
   term, and all column elements are labeled with the @tt{item}
   term.  A typical definition looks as follows:

   @begin[verbatim]
   @begin[array, rcl]
   @line{@item{x} @item{y} @item{z}}
   ...
   @end[array]
   @end[verbatim]

   The @code{@hline} term can be used in place of a @code{@line}
   term to produce a horizontal rule spanning the width of the
   table.  The @code{@cr} term represents the line terminator;
   it is not necessary in normal usage.

   As usual, the @code{@line} form can have a @tt{block} definition.

   @begin[verbatim]
   @begin[array, lcl]
   @begin[line]
   @item{x}
   @item{y}
   @item{z}
   @end[line]
   ...
   @end[array]
   @end[verbatim]

   The @tt{line} and @tt{item} forms are not strictly necessary;
   arbitrary block definitions are sufficient.

   @begin[verbatim]
   @begin[array, rrl]
   {{x} {y} {z}}
   ...
   @end[array]
   @end[verbatim]

   However, the use of the @tt{line} and @tt{item} terms
   is encouraged.
>>
declare tabular[placement,tags]{'t : Dform} : Dform
declare tabular[tags]{'t : Dform} : Dform
declare line{'t : Dform} : Dform
declare cr : Dform
declare hline : Dform
declare cline[s] : Dform
declare arraystretch{'e : Dform} : Dform
declare multicolumn[cols,align]{'t : Dform} : Dform

declare math_array[placement,tags]{'t : Dform} : Dform
declare math_tabular[placement,tags]{'t : Dform} : Dform
declare math_array[tags]{'t : Dform} : Dform
declare math_tabular[tags]{'t : Dform} : Dform
declare math_line{'t : Dform} : Dform
declare math_item{'t : Dform} : Dform
declare math_cr : Dform
declare math_hline : Dform
declare math_cline[s] : Dform
declare math_arraystretch{'e : Dform} : Dform
declare math_multicolumn[cols,align]{'t : Dform} : Dform
doc docoff

(*
 * TeX display.
 *)
declare tex_strip_white{'l1 : Dform; 'l2 : Dform; 't : Dform} : Dform
declare tex_reverse{'l1 : Dform; 'l2 : Dform; 't : Dform} : Dform
declare tex_apply{'t : Dform; 'l : Dform} : Dform

declare tex_array_lines{'l : Dform} : Dform
declare tex_array_lns : Dform
declare tex_array_line{'l : Dform} : Dform
declare tex_array_ln : Dform

dform array_df1 : mode[tex] :: math_array[tags:s]{'t} =
   izone `"\\begin{array}{@{}" slot[tags:s] `"@{}}" ezone
   tex_strip_white{xnil; 't; tex_array_lns}
   izone `"\\end{array}" ezone

dform tabular_df1 : mode[tex] :: math_tabular[tags:s]{'t} =
   izone `"\\begin{tabular}{@{}" slot[tags:s] `"@{}}" ezone
   tex_strip_white{xnil; 't; tex_array_lns}
   izone `"\\end{tabular}" ezone

dform tabular_df2 : mode[tex] :: tabular[tags:s]{'t} =
   izone `"\\begin{tabular}{@{}" slot[tags:s] `"@{}}" ezone
   tex_strip_white{xnil; 't; tex_array_lns}
   izone `"\\end{tabular}" ezone

dform array_place_df1 : mode[tex] :: math_array[placement:s,tags:s]{'t} =
   izone `"\\begin{array}[" slot[placement:s] `"]{@{}" slot[tags:s] `"@{}}" ezone
   tex_strip_white{xnil; 't; tex_array_lns}
   izone `"\\end{array}" ezone

dform tabular_place_df1 : mode[tex] :: math_tabular[placement:s,tags:s]{'t} =
   izone `"\\begin{tabular}[" slot[placement:s] `"]{@{}" slot[tags:s] `"@{}}" ezone
   tex_strip_white{xnil; 't; tex_array_lns}
   izone `"\\end{tabular}" ezone

dform tabular_place_df2 : mode[tex] :: tabular[placement:s,tags:s]{'t} =
   izone `"\\begin{tabular}[" slot[placement:s] `"]{@{}" slot[tags:s] `"@{}}" ezone
   tex_strip_white{xnil; 't; tex_array_lns}
   izone `"\\end{tabular}" ezone

(*
 * Strip the whitespace from the second argument.
 *)
dform tex_strip_white_df1 : tex_strip_white{'l; xcons{comment_white; 'tl}; 't} =
   tex_strip_white{'l; 'tl; 't}

dform tex_strip_white_df2 : tex_strip_white{'l; xcons{'t1; 'tl}; 't2} =
   tex_strip_white{xcons{'t1; 'l}; 'tl; 't2}

dform tex_strip_white_df3 : tex_strip_white{'l; xcons{comment_string[text:s]; 'tl}; 't2} =
   tex_strip_white{xcons{comment_block{xcons{comment_string[text:s]; xnil}}; 'l}; 'tl; 't2}

dform tex_strip_white_df4 : tex_strip_white{'l; xcons{comment_block{'t}; 'tl}; 't2} =
   tex_strip_white{xcons{comment_block{'t}; 'l}; 'tl; 't2}

dform tex_strip_white_df3 : tex_strip_white{'l; xnil; 't} =
   tex_reverse{xnil; 'l; 't}

dform tex_reverse_df1 : tex_reverse{'l; xcons{'t1; 'tl}; 't2} =
   tex_reverse{xcons{'t1; 'l}; 'tl; 't2}

dform tex_reverse_df2 : tex_reverse{'l; xnil; 't} =
   tex_apply{'t; 'l}

(*
 * Array lines.
 *)
dform tex_apply_array_lines_df1 : tex_apply{tex_array_lns; 'l} =
   tex_array_lines{'l}

dform tex_array_lines_xnil_df1 : tex_array_lines{xcons{line{'l}; xnil}} =
   tex_strip_white{xnil; 'l; tex_array_ln}

dform tex_array_lines_xnil_df2 : tex_array_lines{xcons{math_line{'l}; xnil}} =
   tex_strip_white{xnil; 'l; tex_array_ln}

dform tex_array_lines_xnil_df3 : tex_array_lines{xcons{comment_block{'l}; xnil}} =
   tex_strip_white{xnil; 'l; tex_array_ln}

dform tex_array_lines_xnil_df4 : tex_array_lines{xcons{hline; xnil}} =
   hline

dform tex_array_lines_xnil_df5 : tex_array_lines{xcons{math_hline; xnil}} =
   math_hline

dform tex_array_lines_xnil_df6 : tex_array_lines{xcons{cline[s]; xnil}} =
   cline[s]

dform tex_array_lines_xnil_df7 : tex_array_lines{xcons{math_cline[s]; xnil}} =
   math_cline[s]

dform tex_array_lines_cons_df1 : tex_array_lines{xcons{line{'l}; xcons{'h; 't}}} =
   tex_strip_white{xnil; 'l; tex_array_ln}
   izone `"\\\\" ezone
   tex_array_lines{xcons{'h; 't}}

dform tex_array_lines_cons_df2 : tex_array_lines{xcons{math_line{'l}; xcons{'h; 't}}} =
   tex_strip_white{xnil; 'l; tex_array_ln}
   izone `"\\\\" ezone
   tex_array_lines{xcons{'h; 't}}

dform tex_array_lines_cons_df3 : tex_array_lines{xcons{comment_block{'l}; xcons{'h; 't}}} =
   tex_strip_white{xnil; 'l; tex_array_ln}
   izone `"\\\\" ezone
   tex_array_lines{xcons{'h; 't}}

dform tex_array_lines_cons_df4 : tex_array_lines{xcons{hline; xcons{'h; 't}}} =
   hline
   tex_array_lines{xcons{'h; 't}}

dform tex_array_lines_cons_df5 : tex_array_lines{xcons{math_hline; xcons{'h; 't}}} =
   math_hline
   tex_array_lines{xcons{'h; 't}}

dform tex_array_lines_cons_df5 : tex_array_lines{xcons{cline[s]; xcons{'h; 't}}} =
   cline[s]
   tex_array_lines{xcons{'h; 't}}

dform tex_array_lines_cons_df6 : tex_array_lines{xcons{math_cline[s]; xcons{'h; 't}}} =
   math_cline[s]
   tex_array_lines{xcons{'h; 't}}

dform tex_array_lines_df : tex_array_lines{xnil} =
   `""

(*
 * Math line.
 *)
dform tex_apply_array_line_df1 : tex_apply{tex_array_ln; 'l} =
   tex_array_line{'l}

dform tex_array_line_df1 : tex_array_line{xcons{'h; xnil}} =
   'h

dform tex_array_line_df2 : tex_array_line{xcons{'h; xcons{'hd; 'tl}}} =
   'h
   izone `"&" ezone
   tex_array_line{xcons{'hd; 'tl}}

dform tex_array_line_df3 : tex_array_line{xnil} =
   `""

(*
 * Multicolumn specification.
 *)
dform multicolumn_df : mode[tex] :: multicolumn[cols:s, align:s]{'e} =
   izone `"\\multicolumn{" ezone
   slot[cols:s]
   izone `"}{" ezone
   slot[align:s]
   izone `"}{" ezone
   'e
   izone `"}" ezone

dform multicolumn_df2 : except_mode[tex] :: multicolumn[cols:s, align:s]{'e} =
   'e

dform math_multicolumn_df : math_multicolumn[cols:s, align:s]{'e} =
   multicolumn[cols:s, align:s]{'e}

(*
 * Math item.
 *)
dform math_item_df1 : mode[tex] :: math_item{'t} =
   't

(*
 * Return.
 *)
dform math_cr : mode[tex] :: math_cr =
   izone `"\\\\" ezone

dform math_hline : mode[tex] :: math_hline =
   izone `"\\hline " ezone

dform math_hline : mode[tex] :: math_cline[s] =
   izone `"\\cline{" slot[s:s] `"}" ezone

dform cr_df : mode[tex] :: cr =
   izone `"\\\\" ezone

dform hline_df : mode[tex] :: hline =
   izone `"\\hline " ezone

dform hline_df : mode[tex] :: cline[s] =
   izone `"\\cline{" slot[s:s] `"}" ezone

dform arraystretch_df1 : mode[tex] :: arraystretch{'e} =
   izone `"\\renewcommand{\\arraystretch}{" ezone
   'e
   izone `"}" ezone

dform arraystretch_df2 : mode[tex] :: math_arraystretch{'e} =
   arraystretch{'e}

(*
 * Normal display.
 *)
dform normal_math_array_df1 : except_mode[tex] :: math_array[tags:s]{'t} =
   pushm[0] szone
   tex_strip_white{xnil; 't; tex_array_lns}
   ezone popm

dform normal_math_tabular_df1 : except_mode[tex] :: math_tabular[tags:s]{'t} =
   pushm[0] szone
   tex_strip_white{xnil; 't; tex_array_lns}
   ezone popm

dform normal_math_line_df1 : except_mode[tex] :: line{'l} =
   pushm[3] szone 'l ezone popm

dform normal_math_line_df2 : except_mode[tex] :: math_line{'l} =
   pushm[3] szone 'l ezone popm

dform normal_math_item_df1 : except_mode[tex] :: math_item{'l} =
   hspace 'l

dform normal_math_cr_df1 : except_mode[tex] :: math_cr =
   hspace

dform normal_math_cr_df1 : except_mode[tex] :: cr =
   hspace

dform normal_math_hline_df1 : except_mode[tex] :: math_hline =
   `"===="

dform normal_math_hline_df1 : except_mode[tex] :: hline =
   `"===="

dform normal_math_cline_df1 : except_mode[tex] :: math_cline[s] =
   `"===="

dform normal_math_cline_df1 : except_mode[tex] :: cline[s] =
   `"===="

doc <:doc<
   The following macros define higher-level macros.
   The @tt[defrule] term is used to format the output as a rule
   definition.  The @i[name] argument is the name of the rule; the
   @i[args] term represents the arguments; the @i[hyps] are the subgoals
   of the rule; and the @i[goal] is the goal.  The @code{@cr} term
   is allowed in the @i[hyps] and the @i[goal] to produce
   multi-line definitions.

   $$
   @defrule[name]{args; hyps; goal}
   $$

   The @tt[rulebox] macro represents the contents of a rule box.
   The @code{@cr} form is allowed in the @i[hyps] and @i[goal]
   arguments.
   $$
   @rulebox{tac; args; hyps; goal}
   $$

>>
declare math_defrule[name]{'args; 'hyps; 'goal} : Dform
declare math_rulebox{'tac; 'args; 'hyps; 'goal} : Dform
doc docoff

(*
 * TeX display.
 *)
dform tex_math_defrule_df1 : mode[tex] :: math_defrule[name:s]{'args; 'hyps; 'goal} =
   izone `"{\\defrule{" ezone
   slot[name:s]
   izone `"}{" ezone
   'args
   izone `"}{" ezone
   'hyps
   izone `"}{" ezone
   'goal
   izone `"}}" ezone

dform tex_math_rulebox_df1 : mode[tex] :: math_rulebox{'name; 'args; 'hyps; 'goal} =
   izone `"{\\xtac{" ezone
   'name
   izone `"}{" ezone
   'args
   izone `"}{" ezone
   'hyps
   izone `"}{" ezone
   'goal
   izone `"}}" ezone

dform normal_math_defrule_df1 : except_mode[tex] :: math_defrule[name:s]{'args; 'hyps; 'goal} =
   pushm[3] szone
   keyword["rule"] `" " slot[name:s] `" " 'args `"=" hspace
   'hyps `"-->" hspace
   'goal
   ezone popm

dform normal_math_rulebox_df1 : except_mode[tex] :: math_rulebox{'name; 'args; 'hyps; 'goal} =
   pushm[3] szone
   'hyps hspace
   keyword["BY"] `" " 'name `" " 'args hspace
   'goal
   ezone popm

doc <:doc<
   The @tt{dummy_arg} sequent argument is usually used for sequents
   that occur in comments.
>>
declare sequent [dummy_arg] { Dform : Dform >- Dform } : Dform
doc docoff

dform dummy_arg_df : dummy_arg = `""

doc <:doc<
   The following terms display standard representations
   of the usual names.
>>
declare "MetaPRL" : Dform
declare "Nuprl" : Dform
declare "NuPRL" : Dform
declare "OCaml" : Dform
declare "LaTeX" : Dform
declare "MartinLof" : Dform
doc docoff

dform metaprl_df1 : mode[tex] :: "MetaPRL" =
   izone `"\\MetaPRL{}" ezone

dform metaprl_df2 : except_mode[tex] :: "MetaPRL" =
   it["MetaPRL":s]

dform metaprl_df1 : mode[tex] :: "MartinLof" =
   izone `"\\mbox{\\MartinLof{}}" ezone

dform metaprl_df2 : except_mode[tex] :: "MartinLof" =
   it["Martin-Lof":s]

dform nuprl_df1 : mode[tex] :: "Nuprl" =
   izone `"\\Nuprl{}" ezone

dform nuprl_df2 : except_mode[tex] :: "Nuprl" =
   it["Nuprl":s]

dform nuprl_df3 : "NuPRL" = "Nuprl"

dform ocaml_df1 : "OCaml" =
   math_mbox{slot["OCaml"]}

dform latex_df1 : mode[tex] :: "LaTeX" =
   izone `"\\mbox{\\LaTeX{}}" ezone

dform latex_df2 : except_mode[tex] :: "LaTeX" =
   `"LaTeX"

(*
 * Additional forms.
 *)
declare includegraphics{'file : Dform } : Dform

dform includegraphics_df : mode[tex] :: includegraphics{'file} =
   izone `"\\includegraphics{" 'file `"}" ezone

(*
 * Some terms would not display "inline" (as the display form for "declare"
 * would try to display them)
 *)
declare special : Dform

dform special_df : special =
   math_it{slot["special display"]}

dform theory_sp : slot["decl"]{"doc"{'t}} = special
dform theory_sp : slot["decl"]{docon} = special
dform theory_sp : slot["decl"]{docoff} = special
dform theory_sp : slot["decl"]{"theory"{'t}} = special
dform mod_sp    : slot["decl"]{"module"[name:s]} = special
dform mod_sp    : slot["decl"]{"module"{'name}} = special
dform chap_sp   : slot["decl"]{chapter[name:s]{'t}} = special
dform sec_sp    : slot["decl"]{section[name:s]{'t}} = special
dform subsec_sp : slot["decl"]{subsection[name:s]{'t}} = special
dform sbssec_sp : slot["decl"]{subsubsection[name:s]{'t}} = special
dform prgr_sp   : slot["decl"]{paragraph{'t}} = special
dform sprgr_sp  : slot["decl"]{subparagraph{'t}} = special
dform modsec_sp : slot["decl"]{modsection{'t}} = special
dform msbsec_sp : slot["decl"]{modsubsection{'t}} = special
dform parent_sp : slot["decl"]{parents} = special
dform terms_sp  : slot["decl"]{terms} = special
dform rewrite_sp: slot["decl"]{rewrites} = special
dform rules_sp  : slot["decl"]{rules} = special
dform convs_sp  : slot["decl"]{convs} = special
dform tacts_sp  : slot["decl"]{tactics} = special
dform rsrss_sp  : slot["decl"]{resources} = special
dform refcha_sp : slot["decl"]{refchapter[name:s]} = special
dform refsec_sp : slot["decl"]{refsection[name:s]} = special
dform refsbs_sp : slot["decl"]{refsubsection[name:s]} = special
dform refsss_sp : slot["decl"]{refsubsubsection[name:s]} = special
dform reffig_sp : slot["decl"]{reffigure[name:s]} = special
dform refmod_sp : slot["decl"]{refmodule[name:s]} = special
dform refterm_sp: slot["decl"]{refterm[name:s]} = special
dform refres_sp : slot["decl"]{refresource[name:s]} = special
dform refrew_sp : slot["decl"]{refrewrite[name:s]} = special
dform reftac_sp : slot["decl"]{reftactic[name:s]} = special
dform refcnv_sp : slot["decl"]{refconv[name:s]} = special
dform refrul_sp : slot["decl"]{refrule[name:s]} = special
dform code_sp   : slot["decl"]{code[name:s]} = special
dform verb_sp   : slot["decl"]{verbatim[name:s]} = special
dform email_sp  : slot["decl"]{email[name:s]} = special
dform center_sp : slot["decl"]{center{'t}} = special
dform fig_sp    : slot["decl"]{figure[name]{'t}} = special
dform fig2_sp   : slot["decl"]{figure[name, pos]{'t}} = special
dform capt_sp   : slot["decl"]{caption{'t}} = special
dform qout_sp   : slot["decl"]{quote{'t}} = special
dform quot_sp   : slot["decl"]{quotation{'t}} = special
dform item_sp   : slot["decl"]{item{'t}} = special
dform item_sp   : slot["decl"]{item{'t1;'t2}} = special
dform cite_sp   : slot["decl"]{cite[s]} = special
dform color_sp  : slot["decl"]{color[s]} = special
dform pgcolor_sp: slot["decl"]{pagecolor[s]} = special
dform colorbx_sp: slot["decl"]{colorbox[s]{'t}} = special
dform minipg_sp : slot["decl"]{minipage[m]{'t}} = special
dform minipg2_sp: slot["decl"]{minipage[m, n]{'t}} = special
dform iverbat_sp: slot["decl"]{iverbatim[s]} = special
dform enum_sp   : slot["decl"]{enumerate{'t}} = special
dform items_sp  : slot["decl"]{itemize{'t}} = special
dform descr_sp  : slot["decl"]{description{'t}} = special
dform comm_sp   : slot["decl"]{comment[s]{'t}} = special
dform left_sp   : slot["decl"]{math_left[s]} = special
dform right_sp  : slot["decl"]{math_right[s]} = special
dform tabular_sp: slot["decl"]{math_tabular[s1,s2]{'t}} = special
dform tabular_sp: slot["decl"]{math_tabular[s]{'t}} = special
dform tabular_sp: slot["decl"]{tabular[s1,s2]{'t}} = special
dform tabular_sp: slot["decl"]{tabular[s]{'t}} = special
dform cr_sp     : slot["decl"]{cr} = special
dform cr_sp     : slot["decl"]{math_cr} = special
dform hline_sp  : slot["decl"]{hline} = special
dform hline_sp  : slot["decl"]{math_hline} = special
dform cline_sp  : slot["decl"]{cline[s]} = special
dform cline_sp  : slot["decl"]{math_cline[s]} = special
dform multi_sp  : slot["decl"]{multicolumn[s1,s2]{'t}} = special
dform multi_sp  : slot["decl"]{math_multicolumn[s1,s2]{'t}} = special
dform array_sp  : slot["decl"]{math_array[s]{'t}} = special
dform array_sp  : slot["decl"]{math_array[s1,s2]{'t}} = special
dform foot_sp   : slot["decl"]{footnote{'t}} = special
dform target_sp : slot["decl"]{target[name]{'t}} = special
dform target_sp : slot["decl"]{hreftarget[name]} = special
dform hrefm_sp  : slot["decl"]{hrefmodule[name:s]} = special
dform hrefm_sp  : slot["decl"]{hrefmodule_internal[name:s]} = special
dform hreft_sp  : slot["decl"]{hrefterm[name:s]} = special
dform hrefr_sp  : slot["decl"]{hrefresource[name:s]} = special
dform hrefrw_sp : slot["decl"]{hrefrewrite[name:s]} = special
dform hrefrl_sp : slot["decl"]{hrefrule[name:s]} = special
dform hrefta_sp : slot["decl"]{hreftactic[name:s]} = special
dform hrefco_sp : slot["decl"]{hrefconv[name:s]} = special

(*
 * -*-
 * Local Variables:
 * fill-column: 100
 * End:
 * -*-
 *)
