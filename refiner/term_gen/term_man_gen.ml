(*
 * Manifest terms.
 *)

open Opname
open Term_simple_sig
open Term_op_sig
open Term_addr_sig
open Term_subst_sig

module TermMan (**)
   (Term : TermSimpleSig)
   (TermOp : TermOpSig
    with type term = Term.term)
   (TermAddr : TermAddrSig
    with type term = Term.term)
   (TermSubst : TermSubstSig
    with type term = Term.term
    with type param = Term.param) =
struct
   open Term
   open TermOp
   open TermAddr
   open TermSubst

   type term = Term.term
   type operator = Term.operator
   type level_exp = Term.level_exp
   type address = TermAddr.address

   type hypothesis =
      Hypothesis of string * term
    | Context of string * term list

   type esequent =
      { sequent_hyps : hypothesis list;
        sequent_goals : term list
      }

   (*
    * Manifest terms are injected into the "perv" module.
    *)
   let xperv = make_opname ["Perv"]

   (************************************************************************
    * Level expressions                                                    *
    ************************************************************************)

   (* Simplified level expression constructors *)
   let mk_const_level_exp i =
      { le_const = i; le_vars = [] }

   let mk_var_level_exp v =
      { le_const = 0; le_vars = [{ le_var = v; le_offset = 0 }] }

   (*
    * Increment a level exp
    *)
   let incr_level_exp = function
      ({ le_const = c; le_vars = vars } : level_exp) ->
         let add1 = function
            { le_var = v; le_offset = o } ->
               { le_var = v; le_offset = o + 1 }
         in
            { le_const = c + 1; le_vars = List.map add1 vars }

   (*
    * Build a level expression out of the max of two level
    * expressions.
    *)
   let max_level_exp = fun
      ({ le_const = c1; le_vars = l1 } : level_exp)
      ({ le_const = c2; le_vars = l2 } : level_exp) ->
         (* Max of two expressions; sort the variables *)
         let rec join = function
            ({ le_var = v1; le_offset = o1 }::t1 as l1),
            ({ le_var = v2; le_offset = o2 }::t2 as l2) ->
               if v1 = v2 then
                  { le_var = v1; le_offset = max o1 o2 }::(join (t1, t2))
               else if v1 < v2 then
                  { le_var = v1; le_offset = o1 }::(join (t1, l2))
               else
                  { le_var = v2; le_offset = o2 }::(join (l1, t2))
          | [], l2 -> l2
          | l1, [] -> l1
         in
            { le_const = max c1 c2; le_vars = join (l1, l2) }

   (*
    * See if the first level is contained in the second.
    *)
   let level_cumulativity = fun
      { le_const = const1; le_vars = vars1 }
      { le_const = const2; le_vars = vars2 } ->
         let rec caux = function
            ({ le_var = v1; le_offset = o1 }::t1 as l1),
            { le_var = v2; le_offset = o2 }::t2 ->
               if v1 = v2 then
                  if o1 <= o2 then
                     caux (t1, t2)
                  else
                     false
               else if v1 < v2 then
                  caux (l1, t2)
               else
                  false
          | [], _ -> true
          | _, [] -> false
         in
            if const1 <= const2 then
               caux (vars1, vars2)
            else
               false

   (************************************************************************
    * PRIMITIVE FORMS                                                      *
    ************************************************************************)

   let xperv = make_opname ["Perv"]

   (*
    * Lists.
    *)
   let xnil_opname = mk_opname "nil" xperv
   let xcons_opname = mk_opname "cons" xperv

   let xnil_term = mk_simple_term xnil_opname []
   let is_xnil_term t = t = xnil_term

   let is_xcons_term = is_dep0_dep0_term xcons_opname
   let mk_xcons_term = mk_dep0_dep0_term xcons_opname
   let dest_xcons = dest_dep0_dep0_term xcons_opname

   let rec is_xlist_term t =
      match dest_term t with
         { term_op = { op_name = opname; op_params = [] };
           term_terms = [bterm1; bterm2]
         } when opname == xcons_opname ->
            begin
               match (dest_bterm bterm1, dest_bterm bterm2) with
                  ({ bvars = []; bterm = _ }, { bvars = []; bterm = b }) ->
                     is_xlist_term b
                | _ ->
                     false
            end
       | { term_op = { op_name = opname; op_params = [] }; term_terms = [] } when opname == xnil_opname ->
            true
       | _ ->
            false

   let dest_xlist t =
      let rec aux trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] };
              term_terms = [bterm1; bterm2]
            } when opname == xcons_opname ->
               begin
                  match (dest_bterm bterm1, dest_bterm bterm2) with
                     ({ bvars = []; bterm = a },
                      { bvars = []; bterm = b }) -> a::(aux b)
                   | _ -> raise (TermMatch ("dest_xlist", t, "not a list"))
               end
          | { term_op = { op_name = opname; op_params = [] }; term_terms = [] } when opname == xnil_opname ->
               []
          | _ ->
               raise (TermMatch ("dest_xlist", t, "not a list"))
      in
         aux t

   let rec mk_xlist_term = function
      h::t ->
         mk_term (**)
            { op_name = xcons_opname; op_params = [] }
            [mk_simple_bterm h; mk_simple_bterm (mk_xlist_term t)]
    | [] ->
         xnil_term

   (*
    * Strings.
    *)
   let string_opname = mk_opname "string" xperv

   let is_xstring_term t =
      match dest_term t with
         { term_op = { op_name = opname; op_params = [String _] };
           term_terms = []
         } when opname == string_opname ->
            true
       | _ ->
            false

   let dest_xstring t =
      match dest_term t with
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = []
         } when opname == string_opname ->
            s
       | _ ->
            raise (TermMatch ("dest_xstring", t, "not a string"))

   let mk_xstring_term s =
      let op = { op_name = string_opname; op_params = [String s] } in
         mk_term op []

   (****************************************
    * LAMBDA                               *
    ****************************************)

   let xlambda_opname =
      mk_opname "lambda" xperv

   let mk_xlambda_term =
      mk_dep1_term xlambda_opname

   (*************************
    * Sequents              *                                              *
    *************************)

   (* Sequent operator names *)
   let hyp_opname = mk_opname "hyp" xperv
   let concl_opname = mk_opname "concl" xperv
   let sequent_opname = mk_opname "sequent" xperv

   (* Dependent hypotheses *)
   let is_hyp_term = is_dep0_dep1_term hyp_opname
   let mk_hyp_term = mk_dep0_dep1_term hyp_opname
   let dest_hyp = dest_dep0_dep1_term hyp_opname

   (* Conclusions *)
   let is_concl_term = is_dep0_dep0_term concl_opname
   let mk_concl_term = mk_dep0_dep0_term concl_opname
   let dest_concl = dest_dep0_dep0_term concl_opname

   (* Sequent wrapper *)
   let is_sequent_term = is_simple_term_opname sequent_opname
   let mk_sequent_term = mk_simple_term sequent_opname
   let dest_sequent = dest_simple_term_opname sequent_opname
   let null_concl = mk_simple_term concl_opname []

   (*
    * Helper function to unwrap the surrounding sequent term.
    *)
   let goal_of_sequent t =
      match dest_term t with
         { term_op = { op_name = name; op_params = [] };
           term_terms = bt::_
         } when name == sequent_opname ->
            dest_simple_bterm t bt
       | _ ->
            raise (TermMatch ("goal_of_sequent", t, "not a sequent"))

   (*
    * Get the second term in the hyp.
    *)
   let match_hyp name t = function
      [bterm1; bterm2] ->
         begin
            match dest_bterm bterm1, dest_bterm bterm2 with
               ({ bvars = [] }, { bvars = [_]; bterm = term }) ->
                  term
             | _ ->
                  raise (TermMatch (name, t, "malformed hypothesis"))
         end
    | _ ->
         raise (TermMatch (name, t, "malformed hypothesis"))

   let match_hyp_all name t = function
      [bterm1; bterm2] ->
         begin
            match dest_bterm bterm1, dest_bterm bterm2 with
               ({ bvars = []; bterm = t }, { bvars = [x]; bterm = term }) ->
                  t, x, term
             | _ ->
                  raise (TermMatch (name, t, "malformed hypothesis"))
         end
    | _ ->
         raise (TermMatch (name, t, "malformed hypothesis"))

   let match_context name t = function
      [bterm] ->
         begin
            match dest_bterm bterm with
               { bvars = []; bterm = term } ->
                  term
             | _ ->
                  raise (TermMatch (name, t, "malformed context"))
         end
    | _ ->
         raise (TermMatch (name, t, "malformed context"))

   let match_concl name t = function
      [bterm1; bterm2] ->
         begin
            match dest_bterm bterm1, dest_bterm bterm2 with
               ({ bvars = [] }, { bvars = []; bterm = term }) ->
                  term
             | _ ->
                  raise (TermMatch (name, t, "malformed conclusion"))
         end
    | _ ->
         raise (TermMatch (name, t, "malformed conclusion"))

   let match_concl_all name t = function
      [bterm1; bterm2] ->
         begin
            match dest_bterm bterm1, dest_bterm bterm2 with
               ({ bvars = []; bterm = t }, { bvars = []; bterm = term }) ->
                  t, term
             | _ ->
                  raise (TermMatch (name, t, "malformed conclusion"))
         end
    | _ ->
         raise (TermMatch (name, t, "malformed conclusion"))

   (*
    * Explode the sequent into a list of hyps and concls.
    *)
   let explode_sequent_name = "explode_sequent"
   let explode_sequent t =
      let rec collect hyps concls term =
         match dest_term term with
            { term_op = { op_name = opname; op_params = [] }; term_terms = bterms } ->
               if opname == hyp_opname then
                  let t, x, term = match_hyp_all explode_sequent_name t bterms in
                     collect (Hypothesis (x, t) :: hyps) concls term
               else if opname == context_opname then
                  let name, term, args = dest_context term in
                     collect (Context (name, args) :: hyps) concls term
               else if opname == concl_opname then
                  if bterms = [] then
                     { sequent_hyps =  List.rev hyps;
                       sequent_goals = List.rev concls
                     }
                  else
                     let goal, term = match_concl_all explode_sequent_name t bterms in
                        collect hyps (goal :: concls) term
               else
                  raise (TermMatch (explode_sequent_name, t, "malformed sequent"))
          | _ ->
               raise (TermMatch (explode_sequent_name, t, "malformed sequent"))
      in
         collect [] [] t

   (*
    * Find the address of the hyp.
    * We just check to make sure the address is valid.
    *)
   let nth_hyp_addr_name = "nth_hyp_addr"
   let nth_hyp_addr t n =
      let addr = nth_address n true in
      let rec skip_hyps i term =
         match dest_term term with
            { term_op = { op_name = opname; op_params = [] }; term_terms = bterms } ->
               if opname = hyp_opname then
                  let term = match_hyp nth_hyp_addr_name t bterms in
                     if i = 0 then
                        addr
                     else
                        skip_hyps (i - 1) term
               else if opname == context_opname then
                  let term = match_context nth_hyp_addr_name t bterms in
                     if i = 0 then
                        addr
                     else
                        skip_hyps (i - 1) term
               else
                  raise (TermMatch (nth_hyp_addr_name, t, "not enough hyps"))
          | _ ->
               raise (TermMatch (nth_hyp_addr_name, t, "malformed sequent"))
      in
         skip_hyps n (goal_of_sequent t)

   (*
    * Find the address of the conclusion.
    * This is the address of the concl term whose car is the desired conclusion
    * not the conclusion itself.
    *)
   let nth_concl_addr_name = "nth_concl_addr"
   let nth_concl_addr t n =
      let rec skip_concl i n term =
         if n = 0 then
            nth_address i false
         else
            match dest_term term with
               { term_op = { op_name = opname; op_params = [] };
                 term_terms = [bterm1; bterm2]
               } when opname == concl_opname ->
                  begin
                     match dest_bterm bterm1, dest_bterm bterm2 with
                        ({ bvars = [] }, { bvars = []; bterm = term }) ->
                           skip_concl (i + 1) (n - 1) term
                      | _ ->
                           raise (TermMatch (nth_concl_addr_name, t, "malformed conclusion"))
                  end
             | _ ->
                  raise (TermMatch (nth_concl_addr_name, t, "malformed conclusion"))
      in
      let rec skip_hyps i term =
         match dest_term term with
            { term_op = { op_name = opname; op_params = [] }; term_terms = bterms } ->
               if opname == hyp_opname then
                  let term = match_hyp nth_concl_addr_name t bterms in
                     skip_hyps (i + 1) term
               else if opname == context_opname then
                  let term = match_context nth_concl_addr_name t bterms in
                     skip_hyps (i - 1) term
               else if opname == concl_opname then
                  let term = match_concl nth_concl_addr_name t bterms in
                     skip_concl i n term
               else
                  raise (TermMatch (nth_concl_addr_name, t, "malformed sequent"))
          | _ ->
               raise (TermMatch (nth_concl_addr_name, t, "malformed sequent"))
      in
         skip_hyps 0 (goal_of_sequent t)

   (*
    * Conclusion is number 0,
    * negative numbers index from last hyp towards first.
    *)
   let nth_clause_addr_name = "nth_clause_addr"
   let nth_clause_addr_aux make_address t =
      let rec aux i term =
         match dest_term term with
            { term_op = { op_name = opname; op_params = [] }; term_terms = bterms } ->
               if opname == hyp_opname then
                  let term = match_hyp nth_clause_addr_name t bterms in
                     aux (i + 1) term
               else if opname == context_opname then
                  let term = match_context nth_clause_addr_name t bterms in
                     aux (i + 1) term
               else if opname == concl_opname then
                  make_address i
               else
                  raise (TermMatch (nth_clause_addr_name, t, "malformed sequent"))
          | _ ->
               raise (TermMatch (nth_clause_addr_name, t, "malformed sequent"))
      in
         aux 0 (goal_of_sequent t)
   
   let make_nth_clause_addr count i =
      if i < 0 then
         nth_address (count + i) true
      else if i = 0 then
         nth_address count false
      else
         nth_address i true

   let nth_clause_addr t i =
      nth_clause_addr_aux (fun count -> make_nth_clause_addr count i) t
   
   let nth_clause_addrs t il =
      nth_clause_addr_aux (fun count -> Array.map (make_nth_clause_addr count) il) t

   (*
    * Fast access to hyp and concl.
    *)
   let nth_hyp_name = "nth_hyp"
   let nth_hyp t i =
      let rec aux i trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] }; term_terms = bterms } ->
               if opname == hyp_opname then
                  let t, x, term = match_hyp_all nth_hyp_name t bterms in
                     if i = 0 then
                        x, t
                     else
                        aux (i - 1) term
               else if opname == context_opname then
                  let term = match_context nth_hyp_name t bterms in
                     if i = 0 then
                        raise (TermMatch (nth_hyp_name, t, "nth hyp is a context var"))
                     else
                        aux (i - 1) term
               else if opname == concl_opname then
                  raise Not_found
               else
                  raise (TermMatch (nth_hyp_name, t, "malformed sequent"))
          | _ ->
               raise (TermMatch (nth_hyp_name, t, "malformed sequent"))
      in
         aux i (goal_of_sequent t)

   let nth_concl_name = "nth_concl"
   let nth_concl t i =
      let rec aux i term =
         match dest_term term with
            { term_op = { op_name = opname; op_params = [] }; term_terms = bterms } ->
               if opname == hyp_opname then
                  let term = match_hyp nth_concl_name t bterms in
                     aux i term
               else if opname == context_opname then
                  let term = match_context nth_concl_name t bterms in
                     aux i term
               else if opname == concl_opname then
                  let t, term = match_concl_all nth_concl_name t bterms in
                     if i = 0 then
                        t
                     else
                        aux (i - 1) term
               else
                  raise (TermMatch (nth_concl_name, t, "malformed sequent"))
          | _ ->
               raise (TermMatch (nth_concl_name, t, "malformed sequent"))
      in
         aux i (goal_of_sequent t)

   (*
    * Count the hyps.
    *)
   let num_hyps_name = "num_hyps"

   let num_hyps t =
      let rec aux i trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] }; term_terms = bterms } ->
               if opname == hyp_opname then
                  let term = match_hyp num_hyps_name t bterms in
                     aux (i + 1) term
               else if opname == context_opname then
                  let term = match_context num_hyps_name t bterms in
                     aux (i + 1) term
               else if opname == concl_opname then
                  i
               else
                  raise (TermMatch (num_hyps_name, t, "malformed sequent"))
          | _ ->
               raise (TermMatch (num_hyps_name, t, "malformed sequent"))
      in
         aux 0 (goal_of_sequent t)

   (*
    * Collect the vars.
    *)
   let declared_vars_name = "declared_vars"
   let declared_vars t =
      let rec aux vars trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] }; term_terms = bterms } ->
               if opname == hyp_opname then
                  let _, x, term = match_hyp_all declared_vars_name t bterms in
                     aux (x :: vars) term
               else if opname == context_opname then
                  let term = match_context declared_vars_name t bterms in
                     aux vars term
               else if opname == concl_opname then
                  vars
               else
                  raise (TermMatch (declared_vars_name, t, "malformed sequent"))
          | _ ->
               raise (TermMatch (declared_vars_name, t, "malformed sequent"))
      in
         aux [] (goal_of_sequent t)

   (*
    * Get the number of the hyp with the given var.
    *)
   let get_decl_number_name = "get_decl_number"

   let get_decl_number t v =
      let rec aux i trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] }; term_terms = bterms } ->
               if opname == hyp_opname then
                  let _, x, term = match_hyp_all get_decl_number_name t bterms in
                     if x = v then
                        i
                     else
                        aux (i + 1) term
               else if opname == context_opname then
                  let term = match_context get_decl_number_name t bterms in
                     aux (i + 1) term
               else if opname == concl_opname then
                  raise (TermMatch (get_decl_number_name, t, "declaration not found"))
               else
                  raise (TermMatch (get_decl_number_name, t, "malformed sequent"))
          | _ ->
               raise (TermMatch (get_decl_number_name, t, "malformed sequent"))
      in
         aux 0 (goal_of_sequent t)

   (*
    * See if a var is free in the rest of the sequent.
    *)
   let is_free_seq_var_name = "is_free_seq_var"
   let is_free_seq_var i v t =
      let rec aux i t =
         if i = 0 then
            is_free_var v t
         else
            match dest_term t with
               { term_op = { op_name = opname; op_params = [] }; term_terms = bterms } ->
                  if opname == hyp_opname then
                     let term = match_hyp is_free_seq_var_name t bterms in
                        aux (i - 1) term
                  else if opname == context_opname then
                     let term = match_context is_free_seq_var_name t bterms in
                        aux (i - 1) term
                  else if opname == concl_opname then
                     raise (TermMatch (is_free_seq_var_name, t, "index is not a hypothesis"))
                  else
                     raise (TermMatch (is_free_seq_var_name, t, "malformed sequent"))
             | _ ->
                  raise (TermMatch (is_free_seq_var_name, t, "malformed sequent"))
      in
         aux i (goal_of_sequent t)

   (*
    * Generate a list of sequents with replaced goals.
    *)
   let replace_concl_name = "replace_concl"
   let rec replace_concl seq goal =
      match dest_term seq with
         { term_op = ({ op_name = opname; op_params = [] } as op); term_terms = bterms } ->
            if opname == hyp_opname then
               let t, x, term = match_hyp_all replace_concl_name seq bterms in
                  mk_term op [mk_simple_bterm t; mk_bterm [x] (replace_concl term goal)]
            else if opname == context_opname then
               let name, term, args = dest_context seq in
                  mk_context_term name (replace_concl term goal) args
            else if opname == concl_opname then
               goal
            else
               raise (TermMatch (replace_concl_name, seq, "malformed sequent"))
       | _ ->
            raise (TermMatch (replace_concl_name, seq, "malformed sequent"))
   
   let replace_goal_name = "replace_goal"
   let replace_goal seq goal =
      match dest_sequent seq with
         seq :: args ->
            mk_sequent_term (replace_concl (mk_concl_term goal null_concl) seq :: args)
       | [] ->
            raise (TermMatch (replace_goal_name, seq, "malformed sequent"))

   (*
    * Rewrite
    *)
   let xrewrite_op = mk_opname "rewrite" xperv

   let is_xrewrite_term = is_dep0_dep0_term xrewrite_op
   let mk_xrewrite_term = mk_dep0_dep0_term xrewrite_op
   let dest_xrewrite = dest_dep0_dep0_term xrewrite_op

   (************************************************************************
    * Rewrite rules                                                        *
    ************************************************************************)

   (*
    * Build a redex.
    *)
   let construct_redex vars params terms =
      let t = mk_xlist_term (params @ terms) in
      let l = Array.length vars in
      let rec aux i =
         if i < l then
            mk_xlambda_term vars.(i) (aux (i + 1))
         else
            t
      in
         aux 0

end
