(*!
 * @spelling{cons prec precind unrollings}
 *
 * @begin[doc]
 * @theory[Itt_prec]
 *
 * The @tt{Itt_prec} module define the @emph{parameterized}
 * recursive type.  The parameter allows values to be passed
 * as the recursive type is unrolled.  The syntax of the type is
 * $@prec{T; x; B[T, x]; a}$, there $T$ is the type that is
 * being defined, $x$ represents the parameter, with initial
 * value $a$, and $B[T, x]$ is the definition of the type.
 * The body $B[T, x]$ must be monotone in the type argument $T$.
 *
 * The following type definition defines the
 * list of strictly increasing positive integers.
 *
 * $$@prec{T; i; @unit + @prod{k; @set{j; @int; j > i}; T(k)}; 0}.$$
 *
 * The @i{nil} element is $@inl{@it}$, and the @i{cons}
 * operation is the right injection contains a pair of an integer $k$ that
 * is larger that the parameter and a list increasing integers
 * larger than $k$.
 * @end[doc]
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
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
 * @email{jyh@cs.cornell.edu}
 * @end[license]
 *)

(*!
 * @begin[doc]
 * @parents
 * @end[doc]
 *)
include Itt_equal
include Itt_subtype
include Itt_void
include Itt_fun
include Itt_prod
include Itt_struct
(*! @docoff *)

open Printf
open Mp_debug
open String_set
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermSubst
open Mp_resource

open Base_dtactic

open Itt_equal
open Itt_void
open Itt_struct
open Itt_rfun

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Itt_prec%t"

(* debug_string DebugLoad "Loading itt_prec..." *)

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

(*!
 * @begin[doc]
 * @terms
 *
 * The @tt{prec} term defines the parameterized recursive type.
 * The @tt{precind} term is the induction combinator, for computation
 * over the elements in the recursive type.
 * @end[doc]
 *)
declare "prec"{T, x. 'B['T; 'x]; 'a}
declare precind{'a; p, h. 'g['p; 'h]}

(************************************************************************
 * REWRITES                                                             *
 ************************************************************************)

(*!
 * @begin[doc]
 * @rewrites
 *
 * The @tt{precind} term takes two arguments.  The argument $a$
 * is the term over which the computation is being performed, and
 * the $g[p, h]$ term defines the body of the computation.  The
 * first parameter in the body, $p$, represents the result of
 * the recursive computation, and the second parameter $h$
 * represents the argument $a$ itself.
 * @end[doc]
 *)
prim_rw reducePrecind : precind{'a; p, h. 'g['p; 'h]} <-->
   'g[lambda{a. precind{'a; p, h. 'g['p; 'h]}}; 'a]
(*! @docoff *)

(************************************************************************
 * DISPLAY                                                              *
 ************************************************************************)

dform prec_df : except_mode[src] :: "prec"{T, x. 'B; 'a} =
   szone mu `"{" slot{'T} `"," slot{'x} `"." pushm[0] slot{'B} `";" hspace slot{'a} `"}" popm ezone

dform precind_df : except_mode[src] :: precind{'a; p, h. 'g} =
   szone pushm[3]
   `"precind(" slot{'a} `";" hspace
   slot{'p} `"," slot{'p} `"." hspace
   slot{'g}
   popm ezone

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

(*!
 * @begin[doc]
 * @rules
 * @thysubsection{Equality}
 *
 * The @tt{prec} type $@prec{T; x; B[T, x]; a}$ is well-formed if: 1) there
 * is a type of parameters $A$; 2) the initial parameter $a$ has type $A$;
 * and 3) the body $B[T, x]$ is well-formed for any argument $x @in A$ and
 * @emph{any} type $T @in @univ{i}$.  In addition, $B[T, x]$ must be
 * @emph{monotone} in the type argument $T$.  If $T_1 @subseteq T_2$, then
 * $B[T_1, x] @subseteq B[T_2, x]$.
 * @end[doc]
 *)
prim precEquality {| intro []; eqcd |} 'H 'A 'x 'y 'z 'T 'P1 'P2 :
   [wf] sequent [squash] { 'H >- 'a1 = 'a2 in 'A } -->
   [wf] sequent [squash] { 'H; x: 'A; T: 'A -> univ[i:l] >- 'B1['T; 'x] = 'B2['T; 'x] in univ[i:l] } -->
   [wf] sequent [squash] { 'H;
             P1: 'A -> univ[i:l];
             P2: 'A -> univ[i:l];
             z: x:'A -> subtype{('P1 'x); ('P2 'x)};
             x: 'A;
             y: 'B1['P1; 'x]
           >- 'y = 'y in 'B1['P2; 'x]
           } -->
   sequent ['ext] { 'H >- "prec"{A1, x1. 'B1['A1; 'x1]; 'a1}
                   = "prec"{A2, x2. 'B2['A2; 'x2]; 'a2}
                   in univ[i:l]
           } =
   it

(*!
 * @docoff
 *)
prim precMemberFormation {| intro [] |} 'H :
   [main] ('t : sequent ['ext] { 'H >- 'B[lambda{z. "prec"{T, x. 'B['T; 'x]; 'z}}; 'a] }) -->
   [wf] sequent [squash] { 'H >- "type"{("prec"{T, x. 'B['T; 'x]; 'a})} } -->
   sequent ['ext] { 'H >- "prec"{T, x. 'B['T; 'x]; 'a} } =
   't

(*!
 * @begin[doc]
 * @thysubsection{Membership}
 *
 * The elements of the parameterized recursive type $@prec{T; x; B[T, x]; a}$ are the
 * elements in the body $B[@lambda{a'; @prec{T; x; B[T, x]; a'}}, a]$, where the
 * definition of the type has been unrolled.
 * @end[doc]
 *)
prim precMemberEquality {| intro []; eqcd |} 'H 'z :
   [wf] sequent [squash] { 'H >- "type"{("prec"{T, x. 'B['T; 'x]; 'a})} } -->
   [wf] sequent [squash] { 'H >- 'a1 = 'a2 in 'B[lambda{z. "prec"{T, x. 'B['T; 'x]; 'z}}; 'a] } -->
   sequent ['ext] { 'H >- 'a1 = 'a2 in "prec"{T, x. 'B['T; 'x]; 'a} } =
   it

(*!
 * @begin[doc]
 * @thysubsection{Elimination}
 *
 * The elimination form performs induction on the recursive type
 * definition.  The conclusion $G[p]$ holds on any element $p$ of the
 * recursive type $@prec{T; x; B[T, x]; a}$ if, given that it holds
 * on all the unrollings of $p$, it also holds on $p$.
 * @end[doc]
 *)
prim precElimination {| elim [ThinOption thinT] |} 'H 'J lambda{z. 'G['z]} 'a 'A 'Z 'r 'p 'u 'h univ[i:l] :
   [wf] sequent [squash] { 'H; r: "prec"{T, x. 'B['T; 'x]; 'a}; 'J['r] >- 'a = 'a in 'A } -->
   [main] ('g['r; 'u; 'p; 'h] : sequent ['ext] { 'H; r: "prec"{T, x. 'B['T; 'x]; 'a}; 'J['r];
      Z: 'A -> univ[i:l];
      u: subtype{(a: 'A * 'Z 'a); (a: 'A * "prec"{T, x. 'B['T; 'x]; 'a})};
      h: p: (a: 'A * 'Z 'a) -> 'G['p];
      p: a: 'A * 'B['Z; 'a]
   >- 'G['p]
   }) -->
   sequent ['ext] { 'H; r: "prec"{T, x. 'B['T; 'x]; 'a}; 'J['r] >- 'G['a] } =
   precind{'a; p, h. 'g['r; lambda{x. it}; 'p; 'h]}

(*!
 * @begin[doc]
 * The second form of elimination performs an unrolling of the
 * type definition of the parameterized recursive type.
 * @end[doc]
 *)
prim precUnrollElimination {| elim [ThinOption thinT] |} 'H 'J 'z 'y 'u :
   ('g['z; 'y; 'u] : sequent ['ext] { 'H; r: "prec"{T, x. 'B['T; 'x]; 'a}; 'J['r];
             y: 'B[lambda{z. "prec"{T, x. 'B['T; 'x]; 'z}}; 'a];
             u: 'r = 'y in 'B[lambda{z. "prec"{T, x. 'B['T; 'x]; 'z}}; 'a]
             >- 'G['y]
           }) -->
   sequent ['ext] { 'H; r: "prec"{T, x. 'B['T; 'x]; 'a}; 'J['r] >- 'G['r] } =
   'g['z; 'z; it]

(*!
 * @begin[doc]
 * @thysubsection{Combinator equality}
 *
 * The @hrefterm[precind] term $@precind{r; h; z; t[h, z]}$ produces
 * values of type $S$ if the argument $r$ is the pair of a parameter $a$
 * and a term in some parameterized recursive type $@prec{T; y; B[T, y]; a}$,
 * and the body $t[h, z]$ produces values of type $S$ given the
 * argument $r$, and a function $h$ that computes the result on the
 * unrollings.
 * @end[doc]
 *)
prim precindEquality {| intro []; eqcd |} 'H lambda{x. 'S['x]} (a:'A * "prec"{T, y. 'B['T; 'y]; 'a}) 'Z 'u 'h 'z univ[i:l] :
   [wf] sequent [squash] { 'H >- 'r1 = 'r2 in a: 'A * "prec"{T, y. 'B['T; 'y]; 'a} } -->
   [wf] sequent [squash] { 'H; Z: 'A -> univ[i:l];
             u: subtype{(a: 'A * 'Z 'a); (a: 'A * "prec"{T, x. 'B['T; 'x]; 'a})};
             h: z: (a: 'A * 'Z 'a) -> 'S['z];
             z: a: 'A * 'B['Z; 'a]
             >- 't1['h; 'z] = 't2['h; 'z] in 'S['z]
           } -->
   sequent ['ext] { 'H >- precind{'r1; h1, z1. 't1['h1; 'z1]}
                   = precind{'r2; h2, z2. 't2['h2; 'z2]}
                   in 'S['r1]
           } =
   it
(*! @docoff *)

(************************************************************************
 * TACTICS                                                              *
 ************************************************************************)

let prec_term = << "prec"{T, x. 'B['T; 'x]; 'a} >>
let prec_opname = opname_of_term prec_term
let is_prec_term = is_dep2_dep0_term prec_opname
let dest_prec = dest_dep2_dep0_term prec_opname
let mk_prec_term = mk_dep2_dep0_term prec_opname

let precind_term = << precind{'a; p, h. 'g['p; 'h]} >>
let precind_opname = opname_of_term precind_term
let is_precind_term = is_dep0_dep2_term precind_opname
let dest_precind = dest_dep0_dep2_term precind_opname
let mk_precind_term = mk_dep0_dep2_term precind_opname

(************************************************************************
 * TYPE INFERENCE                                                       *
 ************************************************************************)

(*
 * Type of prec.
 *)
let inf_prec inf consts decls eqs opt_eqs defs t =
   let a, b, body, arg = dest_prec t in
   let consts = StringSet.add a (StringSet.add b consts) in
   let eqs', opt_eqs', defs', arg' = inf consts decls eqs opt_eqs defs arg in
      inf consts ((b,arg')::(a,mk_fun_term arg' univ1_term)::decls)
          eqs' opt_eqs' defs' body

let resource typeinf += (prec_term, inf_prec)

(*
 * Type of precind.
 * HACK!!! WRONG! (according to jyh)
let inf_precind f decl t =
   let p, h, a, g = dest_precind t in
   let decl', a' = f decl a in
      f (eqnlist_append_var_eqn p a' (eqnlist_append_var_eqn h a' decl')) g

let resource typeinf += (precind_term, inf_precind)
 *)

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
