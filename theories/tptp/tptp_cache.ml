(*
 * Create tables for success/failure/cycle-detection
 * classes.
 *)

open Printf
open Nl_debug
open String_set

open Refiner.Refiner.Term
open Refiner.Refiner.TermShape

open Splay_table

let debug_cache =
   create_debug (**)
      { debug_name = "cache";
        debug_description = "Show TPTP cache operations";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * This type is used for comparing terms.
 *)
type comparison =
   LessThan
 | GreaterThan
 | Equal of (string * string) list

(*
 * The subsumed exception is raised when a clause
 * is already in the cache.
 *)
exception Subsumed

(************************************************************************
 * TERM TABLE                                                           *
 ************************************************************************)

(*
 * Second value in the association list.
 *)
let rec rev_assoc v = function
   (v1, v2) :: subst ->
      if v = v2 then
         v1
      else
         rev_assoc v subst
 | [] ->
      raise Not_found

(*
 * This compares clauses, where all variables are
 * free.  Terms are sorted from the more general to
 * the less general.  This acts a little like unification,
 * but we just match variables.
 *)
let rec compare_terms constantp subst term1 term2 =
   if is_var_term term1 then
      if is_var_term term2 then
         let v1 = dest_var term1 in
         let v2 = dest_var term2 in
            if constantp v1 then
               if v2 = v1 then
                  Equal subst
               else if constantp v2 then
                  if v1 < v2 then
                     LessThan
                  else
                     GreaterThan
               else
                  GreaterThan
            else if constantp v2 then
               LessThan
            else
               compare_vars subst v1 v2
      else
         LessThan
   else if is_var_term term2 then
      GreaterThan
   else
      let shape1 = shape_of_term term1 in
      let shape2 = shape_of_term term2 in
         match compare shape1 shape2 with
            0 ->
               compare_term_lists constantp subst (subterms_of_term term1) (subterms_of_term term2)
          | ord ->
               if ord > 0 then
                  GreaterThan
               else
                  LessThan

and compare_term_lists constantp subst terms1 terms2 =
   match terms1, terms2 with
      term1 :: terms1, term2 :: terms2 ->
         begin
            match compare_terms constantp subst term1 term2 with
               Equal subst ->
                  compare_term_lists constantp subst terms1 terms2
             | ord ->
                  ord
         end
    | [], [] ->
         Equal subst
    | [], _ ->
         LessThan
    | _, [] ->
         GreaterThan

and compare_vars subst v1 v2 =
   try
      let v2' = List.assoc v1 subst in
      let ord = compare v2 v2' in
         if ord = 0 then
            Equal subst
         else if ord < 0 then
            LessThan
         else
            GreaterThan
   with
      Not_found ->
         try
            let v1' = rev_assoc v2 subst in
               if v1 < v1' then
                  LessThan
               else
                  GreaterThan
         with
            Not_found ->
               Equal ((v1, v2) :: subst)

(*
 * See if one list is a sublist of the other.
 * Clauses are always sorted.
 *)
let rec compare_term_sub_list constantp subst terms1 terms2 =
   match terms1, terms2 with
      term1 :: terms1', term2 :: terms2' ->
         begin
            match compare_terms constantp subst term1 term2 with
               Equal subst ->
                  compare_term_sub_list constantp subst terms1' terms2'
             | LessThan ->
                  (*
                   * The first list contains a term not in the
                   * second list.  It may be subsumed.
                   *)
                  compare_term_sub_list constantp subst terms1' terms2
             | GreaterThan ->
                  (*
                   * The first list does not contain a term in
                   * the second list, so its not a sublist.
                   *)
                  false
         end
    | [], []
    | _, [] ->
         true
    | [], _ ->
         false

(*
 * Sort a list of terms.
 * All variables are treated as constant.
 *)
let sort_term_list terms =
   let compare term1 term2 =
      match compare_terms (fun v -> true) [] term1 term2 with
         LessThan ->
            true
       | GreaterThan
       | Equal _ ->
            false
   in
      Sort.list compare terms

(*
 * Merge two term lists, and remove duplicates.
 * All variables are treated as constants.
 * The two lists have had a substitution performed on
 * them, so they may not quite be in sorted order, but
 * a bubble sort will be probably be faster than
 * a quicksort.
 *)
let merge_term_lists =
   (*
    * All variables are treated as constants.
    *)
   let merge_compare = compare_terms (fun v -> true) [] in

   (*
    * Bubble function.
    *)
   let rec insert term = function
      term' :: terms ->
         begin
            match merge_compare term term' with
               Equal _ ->
                  term' :: terms
             | LessThan ->
                  term :: term' :: terms
             | GreaterThan ->
                  term' :: insert term terms
         end
    | [] ->
         [term]
   in

   (*
    * Bubble sort.
    *)
   let rec merge_term_lists_aux terms terms1 terms2 =
      match terms1, terms2 with
         term1 :: terms1', term2 :: terms2' ->
            begin
               match merge_compare term1 term2 with
                  Equal _ ->
                     merge_term_lists_aux (insert term1 terms) terms1' terms2'
                | LessThan ->
                     merge_term_lists_aux (insert term1 terms) terms1' terms2
                | GreaterThan ->
                     merge_term_lists_aux (insert term2 terms) terms1 terms2'
            end
       | term1 :: terms1, [] ->
            merge_term_lists_aux (insert term1 terms) terms1 []
       | [], term2 :: terms2 ->
            merge_term_lists_aux (insert term2 terms) terms2 []
       | [], [] ->
            terms
   in
      merge_term_lists_aux []

(*
 * The order on terms.
 *)
module Ord =
struct
   type set = StringSet.t
   type t = term

   let union = StringSet.union
   let compare set term1 term2 =
      match compare_terms (StringSet.mem set) [] term1 term2 with
         Equal _ ->
            0
       | LessThan ->
            -1
       | GreaterThan ->
            1
end

(*
 * The term set.
 *)
module TermTable = MakeSplayTable (Ord)

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

module TptpCache =
struct
   (*
    * A cache is a term cache.
    *)
   type t =
      { cache_constants : StringSet.t;
        cache_table : term list TermTable.t
      }

   (*
    * Create an empty table.
    *)
   let rec set_of_list t = function
      s :: tl ->
         set_of_list (StringSet.add s t) tl
    | [] ->
         t

   let create constants =
      { cache_constants = constants;
        cache_table = TermTable.create constants
      }

   (*
    * Check if a clause is subsumed by an existing entry.
    * Check each part of the clause.  Remember clauses
    * by pointer equality.
    *)
   let rec subsumed_goals constants memo clause = function
      goal :: goals ->
         if List.memq goal memo then
            subsumed_goals constants memo clause goals
         else if compare_term_sub_list constants [] clause goal then
            raise Subsumed
         else
            subsumed_goals constants (goal :: memo) clause goals
    | [] ->
         memo

   let rec subsumed constants memo t clause = function
      term :: terms ->
         let memo =
            try
               let goals = TermTable.find_all t term in
                  subsumed_goals constants memo clause goals
            with
               Not_found ->
                  memo
         in
            subsumed constants memo t clause terms
    | [] ->
         ()

   (*
    * See if a goal is subsumed.
    *)
   let subsumed { cache_constants = constants; cache_table = t } clause =
      if !debug_cache then
         eprintf "Tptp_cache.subsumed: %a%t" print_term_list clause eflush;
      let flag =
         try
            subsumed (StringSet.mem constants) [] t clause clause;
            false
         with
            Subsumed ->
               true
      in
         if !debug_cache then
            eprintf "Tptp_cache.subsumed: %b%t" flag eflush;
         flag

   (*
    * Add a goal to the table, unless it is subsumed by
    * and existing entry.
    *)
   let rec add_clause clause t = function
      term :: terms ->
         add_clause clause (TermTable.add t term clause) terms
    | [] ->
         t

   let insert { cache_constants = constants; cache_table = t } clause =
      { cache_constants = constants;
        cache_table = add_clause clause t clause
      }
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
