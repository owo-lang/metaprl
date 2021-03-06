(* This file implements terms' recursive hashing module
 * based on weak arrays
 *
 * -----------------------------------------------------------------
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998 Yegor Bryukhov, Moscow State University
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
 * Author: Yegor Bryukhov, Alexey Nogin
 *)
open Lm_symbol

open Opname

module TermHash (ToTerm : Termmod_sig.TermModuleInternalSig) =
struct
   module WM = Weak_memo.TheWeakMemo
   module TTerm = ToTerm.Term
   module TType = ToTerm.TermType
   module Refinex = ToTerm.Refine

   type hashed_param = TType.param
   type param = TType.param
   type param' = TType.param'
   type term = TType.term
   type meta_term = TType.meta_term
   type msequent = Refinex.msequent

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type term_index = TType.term WM.descriptor
   type meta_term_index = TType.meta_term WM.descriptor
   type msequent_index = Refinex.msequent WM.descriptor

   type term_weak_index = TType.term WM.weak_descriptor
   type meta_term_weak_index = TType.meta_term WM.weak_descriptor
   type msequent_weak_index = Refinex.msequent WM.weak_descriptor

   type hypothesis_header =
      Hypothesis of var * term_index
    | Context of var * var list * term_index list

   type hypothesis_weak_header =
      Hypothesis_weak of var * TType.term WM.weak_descriptor
    | Context_weak of var * var list * TType.term WM.weak_descriptor list

   type bound_term_header =
      { bvars: var list;
        bterm: term_index
      }

   type bound_term_weak_header =
      { bvars_weak: var list;
        bterm_weak: TType.term WM.weak_descriptor
      }

   type true_term_header =
      { op_name: opname;
        op_params: TType.param list;
        term_terms: bound_term_header list
      }

   type true_term_weak_header =
      { op_name_weak: opname;
        op_params_weak: TType.param list;
        term_terms_weak: bound_term_weak_header list
      }

   type seq_header =
      { seq_arg: term_index;
        seq_hyps: hypothesis_header list;
        seq_concl: term_index
      }

   type seq_weak_header =
      { seq_arg_weak: TType.term WM.weak_descriptor;
        seq_hyps_weak: hypothesis_weak_header list;
        seq_concl_weak: TType.term WM.weak_descriptor
      }

   type term_header =
      Term of true_term_header
    | Seq of seq_header
    | FOVar of var
    | SOVar of var * var list * term_index list

   type term_weak_header =
      Term_weak of true_term_weak_header
    | Seq_weak of seq_weak_header
    | FOVar_weak of var
    | SOVar_weak of var * var list * TType.term WM.weak_descriptor list

   type meta_term_header =
      MetaTheorem of term_index
    | MetaImplies of meta_term_index * TType.meta_term WM.descriptor
    | MetaFunction of term_index * meta_term_index * TType.meta_term WM.descriptor
    | MetaIff of meta_term_index * TType.meta_term WM.descriptor
    | MetaLabeled of string * meta_term_index

   type meta_term_weak_header =
      MetaTheorem_weak of TType.term WM.weak_descriptor
    | MetaImplies_weak of TType.meta_term WM.weak_descriptor * TType.meta_term WM.weak_descriptor
    | MetaFunction_weak of TType.term WM.weak_descriptor * TType.meta_term WM.weak_descriptor * TType.meta_term WM.weak_descriptor
    | MetaIff_weak of TType.meta_term WM.weak_descriptor * TType.meta_term WM.weak_descriptor
    | MetaLabeled_weak of string * TType.meta_term WM.weak_descriptor

   type msequent_header = term_index list * term_index

   type msequent_weak_header = TType.term WM.weak_descriptor list * TType.term WM.weak_descriptor

   type t =
      { param_hash     : (TType.param', TType.param) Hashtbl.t;
        term_hash      : (t, term, term_header, term_weak_header, TType.term) WM.t;
        meta_term_hash : (t, meta_term, meta_term_header, meta_term_weak_header, TType.meta_term) WM.t;
        msequent_hash  : (t, msequent, msequent_header, msequent_weak_header, Refinex.msequent) WM.t
      }

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   let weaken_term = WM.weaken
   let weaken_meta_term = WM.weaken
   let weaken_msequent = WM.weaken

(* unused
   let dest_hparam p = p
*)

   let weak_bterm_header () { bvars=bvs; bterm=term_index } =
      { bvars_weak=bvs; bterm_weak= WM.weaken term_index }

   let weak_bterm_header' = weak_bterm_header ()

   let weak_hyp_header () hyp =
      match hyp with
         Hypothesis (v, t) -> Hypothesis_weak (v, WM.weaken t)
       | Context (v, conts, trms) -> Context_weak (v, conts, List.map WM.weaken trms)

   let weak_hyp_header' = weak_hyp_header ()

   let weak_tterm_header () { op_name = op; op_params = params; term_terms = bterms } =
      { op_name_weak = op;
        op_params_weak = params;
        term_terms_weak = List.map weak_bterm_header' bterms }

   let weak_tterm_header' = weak_tterm_header ()

   let weak_term_header _ th =
      match th with
         Seq { seq_arg = arg; seq_hyps = hyps; seq_concl = concl } ->
            Seq_weak { seq_arg_weak = WM.weaken arg;
                       seq_hyps_weak = List.map weak_hyp_header' hyps;
                       seq_concl_weak = WM.weaken concl
            }
       | Term th -> Term_weak (weak_tterm_header' th)
       | FOVar v -> FOVar_weak v
       | SOVar (v, conts, trms) -> SOVar_weak (v, conts, List.map WM.weaken trms)

   let weak_meta_term_header _ mt =
      match mt with
         MetaTheorem t ->
            MetaTheorem_weak (WM.weaken t)
       | MetaImplies (t1, t2) ->
            MetaImplies_weak (WM.weaken t1, WM.weaken t2)
       | MetaFunction (t1, mt1, mt2) ->
            MetaFunction_weak (WM.weaken t1, WM.weaken mt1, WM.weaken mt2)
       | MetaIff (mt1, mt2) ->
            MetaIff_weak (WM.weaken mt1, WM.weaken mt2)
       | MetaLabeled (l, mt) ->
            MetaLabeled_weak (l, WM.weaken mt)

   let weak_msequent_header _ (hyps, goal) =
      List.map WM.weaken hyps, WM.weaken goal

   (*
    * Compare that the elements on the lists are equal.
    *)
   let list_mem_eq = Lm_list_util.compare_eq

   (*
    * Compare lists with cmp
   *)
   let list_compare = Lm_list_util.compare_cmp

   let compare_bterm_header { bvars_weak=bvars1; bterm_weak=bterm1 } { bvars_weak=bvars2; bterm_weak=bterm2 } =
      list_compare Lm_symbol.eq bvars1 bvars2 & bterm1 == bterm2

   let compare_hyp_header hyp1 hyp2 =
      match hyp1, hyp2 with
         Hypothesis_weak (v1,t1),  Hypothesis_weak (v2,t2) ->
            Lm_symbol.eq v1 v2 && t1 == t2
       | Context_weak (v1,conts1,ts1), Context_weak (v2,conts2,ts2) ->
            Lm_symbol.eq v1 v2 && list_compare Lm_symbol.eq conts1 conts2 && list_mem_eq ts1 ts2
       | _ -> false

   let compare_term_header t1 t2 =
      match (t1,t2) with
         Term_weak t1, Term_weak t2 ->
            Opname.eq t1.op_name_weak t2.op_name_weak
            && list_mem_eq t1.op_params_weak t2.op_params_weak
            && list_compare (compare_bterm_header) t1.term_terms_weak t2.term_terms_weak
       | Seq_weak { seq_arg_weak = arg1; seq_hyps_weak = hyp1; seq_concl_weak = concl1 },
         Seq_weak { seq_arg_weak = arg2; seq_hyps_weak = hyp2; seq_concl_weak = concl2 } ->
            (arg1 == arg2)
            && (concl1 == concl2)
            && list_compare (compare_hyp_header) hyp1 hyp2
       | FOVar_weak v1, FOVar_weak v2 ->
            Lm_symbol.eq v1 v2
       | SOVar_weak (v1,conts1,ts1), SOVar_weak (v2,conts2,ts2) ->
            Lm_symbol.eq v1 v2 && conts1 = conts2 && list_mem_eq ts1 ts2
       | _ -> false

   let compare_meta_term_header mt_a mt_b =
      match mt_a, mt_b with
         MetaTheorem_weak t1, MetaTheorem_weak t2 -> t1==t2
       | MetaImplies_weak (t11, t12), MetaImplies_weak (t21, t22) -> t11==t21 && t12==t22
       | MetaFunction_weak (t1, mt11, mt12), MetaFunction_weak (t2, mt21, mt22) -> t1==t2 && mt11==mt21 && mt12==mt22
       | MetaIff_weak (mt11, mt12), MetaIff_weak (mt21, mt22) -> mt11==mt21 && mt12==mt22
       | MetaLabeled_weak (l1, mt1), MetaLabeled_weak (l2, mt2) -> l1=l2 && mt1==mt2
       | _ -> false

   let compare_msequent_header (hyps_a, goal_a) (hyps_b, goal_b) =
      goal_a == goal_b && list_mem_eq hyps_a hyps_b

   let p_constr_param info param =
      try
         Hashtbl.find info.param_hash param
      with Not_found ->
            let result = TTerm.make_param param in
               Hashtbl.add info.param_hash param result;
               result

   let p_constr_operator _info (opname_index, params) =
      TTerm.make_op
      { Term_sig.op_name = opname_index;
        Term_sig.op_params = params
      }

   let p_constr_bterm info { bvars=bvs; bterm=term_index } =
      TTerm.make_bterm
      { Term_sig.bvars = bvs;
        Term_sig.bterm = WM.retrieve info.term_hash info term_index
      }

   let p_constr_hyp info hyp =
      match hyp with
         Hypothesis (v, t) -> Term_sig.Hypothesis (v, WM.retrieve info.term_hash info t)
       | Context (v, conts, trms) -> Term_sig.Context (v, conts, List.map (WM.retrieve info.term_hash info) trms)

   let p_constr_tterm info { op_name = op; op_params = params; term_terms = bterms } =
      TTerm.make_term
      { Term_sig.term_op = p_constr_operator info (op, params);
        Term_sig.term_terms = List.map (p_constr_bterm info) bterms }

   let p_constr_term info th =
      match th with
         Seq { seq_arg = arg; seq_hyps = hyps; seq_concl = concl } ->
            ToTerm.TermMan.mk_sequent_term
            { TType.sequent_args = WM.retrieve info.term_hash info arg;
              TType.sequent_hyps  = TTerm.SeqHyp.of_list  (List.map (p_constr_hyp info) hyps);
              TType.sequent_concl = WM.retrieve info.term_hash info concl
            }
       | Term th -> p_constr_tterm info th
       | FOVar v -> TTerm.mk_var_term v
       | SOVar (v, conts, trms) -> ToTerm.TermMan.mk_so_var_term v conts (List.map (WM.retrieve info.term_hash info) trms)

   let p_constr_meta_term info mt =
      match mt with
         MetaTheorem t ->
            Term_sig.MetaTheorem (WM.retrieve info.term_hash info t)
       | MetaImplies (t1, t2) ->
            Term_sig.MetaImplies (WM.retrieve info.meta_term_hash info t1,
                               WM.retrieve info.meta_term_hash info t2)
       | MetaFunction (t1, mt1, mt2) ->
            Term_sig.MetaFunction (WM.retrieve info.term_hash info t1,
                                WM.retrieve info.meta_term_hash info mt1,
                                WM.retrieve info.meta_term_hash info mt2)
       | MetaIff (mt1, mt2) ->
            Term_sig.MetaIff (WM.retrieve info.meta_term_hash info mt1,
                           WM.retrieve info.meta_term_hash info mt2)
       | MetaLabeled (l, mt) ->
            Term_sig.MetaLabeled (l, WM.retrieve info.meta_term_hash info mt)

   let p_constr_msequent info (hyps, goal) =
      Refinex.mk_msequent (WM.retrieve info.term_hash info goal) (List.map (WM.retrieve info.term_hash info) hyps)

   (*
    * WeakMemo.apply is not allowed.
    *)
   let make_term_header _ _ =
      raise (Invalid_argument "Term_hash.make_term_header")

   let make_meta_term_header _ _ =
      raise (Invalid_argument "Term_hash.make_meta_term_header")

   let make_msequent_header _ _ =
      raise (Invalid_argument "Term_hash.make_msequent_header")

   let p_create hash_size =
      { param_hash = Hashtbl.create hash_size;
        term_hash = WM.create hash_size 20 "Term_hash.term_hash" make_term_header weak_term_header compare_term_header p_constr_term;
        meta_term_hash = WM.create hash_size 20 "Term_hash.meta_term_hash" make_meta_term_header weak_meta_term_header compare_meta_term_header p_constr_meta_term;
        msequent_hash = WM.create hash_size 20 "Term_hash.msequent_hash" make_msequent_header weak_msequent_header compare_msequent_header p_constr_msequent
      }

   let p_lookup info th = WM.lookup info.term_hash info th

   let p_retrieve info ti = WM.retrieve info.term_hash info ti

   let p_lookup_meta info mth = WM.lookup info.meta_term_hash info mth

   let p_retrieve_meta info mti = WM.retrieve info.meta_term_hash info mti

   let p_lookup_msequent info mseq = WM.lookup info.msequent_hash info mseq

   let p_retrieve_msequent info mseq = WM.retrieve info.msequent_hash info mseq

   let global_hash = p_create 17

   let constr_param p = p_constr_param global_hash p
   let lookup th = p_lookup global_hash th
   let retrieve ti = p_retrieve global_hash ti
   let lookup_meta mth = p_lookup_meta global_hash mth
   let retrieve_meta mti = p_retrieve_meta global_hash mti
   let lookup_msequent mseq = p_lookup_msequent global_hash mseq
   let retrieve_msequent mseq = p_retrieve_msequent global_hash mseq

   let lookup_term make_term_header t =
      match ToTerm.Term.dest_descriptor t with
         Some d ->
            if WM.retrieve_check global_hash.term_hash d then
               d
            else
               lookup (make_term_header t)
       | None ->
            lookup (make_term_header t)

   let compare_terms = WM.compare
   let compare_meta_terms = WM.compare
   let compare_msequents = WM.compare

   module HashedTerm =
   struct
      type t=term_index
      let equal t1 t2 = (WM.weaken t1) == (WM.weaken t2)
      let hash t = (WM.wd_hash (WM.weaken t)) land 0x3FFFFFFF
   end

   module HashTerm = Hashtbl.Make (HashedTerm)

   module HashedBTerm =
   struct

      type t = bound_term_header

      let equal bt1 bt2 =
         (bt1.bvars = bt2.bvars) && HashedTerm.equal bt1.bterm bt2.bterm

      let hash bt =
         ((Hashtbl.hash bt.bvars) + (65599 * (HashedTerm.hash bt.bterm))) land 0x3FFFFFFF
   end

   module HashBTerm = Hashtbl.Make (HashedBTerm)

   module HashedHyp =
   struct

      type t = hypothesis_header

      let equal h1 h2 = match h1,h2 with
         Hypothesis (v1,t1), Hypothesis (v2,t2) ->
            Lm_symbol.eq v1 v2 && HashedTerm.equal t1 t2
       | Context (v1, conts1, ts1), Context (v2, conts2, ts2) ->
            Lm_symbol.eq v1 v2 && conts1 = conts2 && Lm_list_util.for_all2 HashedTerm.equal ts1 ts2
       | _ -> false

      let hash = function
         Hypothesis (v,t) ->
            ((Hashtbl.hash v) + (65599 * (HashedTerm.hash t))) land 0x3FFFFFFF
       | Context (v,conts,ts) ->
            (((Hashtbl.hash v) + (Hashtbl.hash conts)) * 65599 + Hashtbl.hash (List.map HashedTerm.hash ts)) land 0x3FFFFFFF
   end

   module HashHyp = Hashtbl.Make (HashedHyp)
end

(* unused
module TermHashInternal = TermHash
*)

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_norm, term_copy_weak"
 * End:
 * -*-
 *)
