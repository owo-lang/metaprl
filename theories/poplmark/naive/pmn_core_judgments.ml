(*
 * The judgments for FSub.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
extends Itt_hoas_sequent_native
extends Pmn_core_terms

open Lm_printf

open Simple_print
open Basic_tactics

(*
 * Judgments include subtyping.
 *)
define unfold_isJudgment : isJudgment{'e} <--> <:xterm<
   (fix is_judgment e ->
       dest_bterm e with
          l, r ->
             "false"
        | d, o, s ->
             if is_same_op{o; $"fsub_subtype"{t1; t2}} then
                isTyExp{nth{s; 0}} && isTyExp{nth{s; 1}}
             else if is_same_op{o; $"fsub_member"{e; ty}} then
                isExp{nth{s; 0}} && isTyExp{nth{s; 1}}
             else
                "false") e
>>

let fold_isJudgment = makeFoldC << isJudgment{'e} >> unfold_isJudgment

interactive isJudgment_wf {| intro [] |} : <:xrule<
   "wf" : <H> >- e IN "BTerm" -->
   <H> >- isJudgment{e} Type
>>

define unfold_JudgmentExp : JudgmentExp <--> <:xterm<
   { e: "BTerm" | isJudgment{e} }
>>

interactive wf_JudgmentExp {| intro [] |} : <:xrule<
   <H> >- "JudgmentExp" Type
>>

(*
 * Define all the parts of a sequent.
 *)
define unfold_SOVar : SOVar{'d} <--> SOVar{'d; TyExp}
define unfold_CVar : CVar{'d} <--> CVar{'d; TyExp}

define unfold_Sequent : Sequent <--> Sequent{0; BTerm; TyExp; JudgmentExp}
define unfold_ProofStep : ProofStep <--> ProofStep{Sequent}

(*
 * Define the rules.
 *)
define unfold_sa_top : sa_top <--> <:xquoterule<
   <H> >- T <: "TyTop"
>>

define unfold_sa_tvar : sa_tvar <--> <:xquoterule<
   <H>; X: T; <J[X]> >- X <: X
>>

define unfold_sa_trans_tvar : sa_trans_tvar <--> <:xquoterule<
   <H>; X: U; <J[X]> >- U <: T -->
   <H>; X: U; <J[X]> >- X <: T
>>

define unfold_sa_arrow : sa_arrow <--> <:xquoterule<
   <H> >- T1 <: S1 -->
   <H> >- S2 <: T2 -->
   <H> >- TyFun{S1; S2} <: TyFun{T1; T2}
>>

define unfold_sa_all : sa_all <--> <:xquoterule<
   <H> >- T1 <: S1 -->
   <H>; X: T1 >- S2[X] <: T2[X] -->
   <H> >- TyAll{S1; X. S2[X]} <: TyAll{T1; X. T2[X]}
>>

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)