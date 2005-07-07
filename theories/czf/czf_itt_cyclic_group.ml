doc <:doc<
   @module[Czf_itt_cyclic_group]

   The @tt{Czf_itt_cyclic_group} module defines cyclic groups.
   A group $g$ is @emph{cyclic} if there exists $a @in @car{g}$
   such that for every $x @in @car{g}$ there is an integer $n$
   such that $@eq{x; a^n}$.

   @docoff
   ----------------------------------------------------------------

   @begin[license]
   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/htmlman/default.html or visit http://metaprl.org/
   for more information.

   Copyright (C) 2002 Xin Yu, Caltech

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

   Author: Xin Yu
   @email{xiny@cs.caltech.edu}
   @end[license]
>>

doc <:doc< @parents >>
extends Czf_itt_group
extends Czf_itt_cyclic_subgroup
extends Czf_itt_abel_group
doc docoff

open Lm_debug
open Lm_printf

open Tactic_type.Conversionals

open Dtactic

let _ =
   show_loading "Loading Czf_itt_cyclic_group%t"

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

doc terms
declare cycg{'g}
declare cycgroup{'g; 'a}
doc docoff

(************************************************************************
 * REWRITES                                                             *
 ************************************************************************)

doc <:doc<
   @rewrites

   A group $g$ is cyclic if it has a generator.
   A cyclic group generated by $a$ can be defined with separation.
>>
prim_rw unfold_cycg : cycg{'g} <-->
   (group{'g} & (exst a: set. (mem{'a; car{'g}} & all x: set. (mem{'x; car{'g}} => (exst n: int. eq{'x; power{'g; 'a; 'n}})))))

prim_rw unfold_cycgroup : cycgroup{'g; 'a} <-->
   (group{'g} & mem{'a; car{'g}} & equal{car{'g}; sep{car{'g}; x. (exst n: int. eq{'x; power{'g; 'a; 'n}})}})
doc docoff

let fold_cycgroup = makeFoldC << cycgroup{'g; 'a} >> unfold_cycgroup

(************************************************************************
 * DISPLAY FORMS                                                        *
 ************************************************************************)

dform cyclic_g_df : except_mode[src] :: cycg{'g} =
   `"cyclic_group(" slot{'g} `")"

dform cyclic_group_df : except_mode[src] :: cycgroup{'g; 'a} =
   `"cyclic_group(" slot{'g} `"; " slot{'a} `")"

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

doc <:doc<
   @rules
   @modsubsection{Typehood}

   The $@cycg{g}$ is well-formed if $g$ is a label;
   the $@cycgroup{g; a}$ is well-formed if $g$ is a label
   and $a$ is a set.
>>
interactive cycg_wf {| intro [] |} :
   sequent { <H> >- 'g IN label } -->
   sequent { <H> >- "type"{cycg{'g}} }

interactive cycgroup_wf {| intro [] |} :
   sequent { <H> >- 'g IN label } -->
   sequent { <H> >- isset{'a} } -->
   sequent { <H> >- "type"{cycgroup{'g; 'a}} }

doc <:doc<
   @modsubsection{Functionality}

   The @tt{cycgroup} is functional in its set argument.
>>
interactive cycgroup_fun {| intro [] |} :
   sequent { <H> >- 'g IN label } -->
   sequent { <H> >- group{'g} } -->
   sequent { <H> >- fun_prop{z. cycgroup{'g; 'z}} }
doc docoff

doc <:doc<
   @modsubsection{Introduction}

   The proposition $@cycg{g}$ is true if it is well-formed, and there is
   an element $a$ in its carrier set such that any element in the carrier
   set is to some power of $a$.
   @cr
   The proposition $@cycgroup{g; a}$ is true if it is well-formed,
   $@mem{a; @car{g}}$, and @cr
   $@equal{@car{g}; @sep{x; @car{g}; @exists{n; @int; @eq{x; @power{g; a; x}}}}}$.
>>
interactive cycg_intro {| intro [] |} 'a :
   sequent { <H> >- 'g IN label } -->
   sequent { <H> >- group{'g} } -->
   sequent { <H> >- isset{'a} } -->
   sequent { <H> >- mem{'a; car{'g}} } -->
   sequent { <H>; x: set; u: mem{'x; car{'g}} >- exst n: int. eq{'x; power{'g; 'a; 'n}} } -->
   sequent { <H> >- cycg{'g} }

interactive cycgroup_intro {| intro [] |} :
   sequent { <H> >- 'g IN label } -->
   sequent { <H> >- group{'g} } -->
   sequent { <H> >- isset{'a} } -->
   sequent { <H> >- mem{'a; car{'g}} } -->
   sequent { <H> >- equal{car{'g}; sep{car{'g}; x. (exst n: int. eq{'x; power{'g; 'a; 'n}})}} } -->
   sequent { <H> >- cycgroup{'g; 'a} }

doc <:doc<
   @modsubsection{Theorems}

   $@cycg{g}$ is equivalent to there exists $a @in @car{g}$ such that $@cycgroup{g; a}$.
>>
interactive cycg1 :
   sequent { <H> >- 'g IN label } -->
   sequent { <H> >- group{'g} } -->
   sequent { <H> >- cycg{'g} } -->
   sequent { <H> >- "dexists"{car{'g}; a. cycgroup{'g; 'a}} }

interactive cycg2 :
   sequent { <H> >- 'g IN label } -->
   sequent { <H> >- group{'g} } -->
   sequent { <H> >- "dexists"{car{'g}; a. cycgroup{'g; 'a}} } -->
   sequent { <H> >- cycg{'g} }

doc <:doc<
   @modsubsection{Theorems}

   Every cyclic group is abelian.
>>
interactive cycg_abel :
   sequent { <H> >- 'g IN label } -->
   sequent { <H> >- group{'g} } -->
   sequent { <H> >- cycg{'g} } -->
   sequent { <H> >- abel{'g} }

doc docoff
(************************************************************************
 * TACTICS                                                              *
 ************************************************************************)

doc <:doc<
   @tactics

   @begin[description]
   @item{@tactic[cycgroupAbelT];
      The tactic applies the @hrefrule[cycgroup_abel] rule
      and proves a group is abelian by showing it is cyclic.}
   @end[description]
   @docoff
>>
let cycgAbelT = cycg_abel

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
