include Czf_itt_group
include Czf_itt_cyclic_subgroup
include Czf_itt_subgroup
include Czf_itt_subset
include Itt_logic

open Printf
open Mp_debug
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermSubst
open Refiner.Refiner.Refine
open Refiner.Refiner.RefineError
open Mp_resource
open Simple_print

open Tactic_type
open Tactic_type.Tacticals
open Tactic_type.Sequent
open Tactic_type.Conversionals
open Mptop
open Var

open Base_dtactic
open Base_auto_tactic

declare cyclic_group{'a}

prim_rw unfold_cyclic_group : cyclic_group{'a} <-->
   cyclic_subgroup{'a}

let fold_cyclic_group = makeFoldC << cyclic_group{'a} >> unfold_cyclic_group

dform cyclic_group_df : except_mode[src] :: cyclic_group{'a} =
   `"<" slot{'a} `">" subc

(* axioms *)
interactive cyclic_group_def {| intro [] |} 'H :
   sequent ['ext] { 'H >- equal{cyclic_group{elem_in_G}; car} }

interactive cyclic_group_def1 {| intro [] |} 'H :
   sequent ['ext] { 'H >- isset{'a} } -->
   sequent ['ext] { 'H >- mem{'a; car} } -->
   sequent ['ext] { 'H >- equal{cyclic_group{'a}; car} }

(* basic properties of cyclic groups *)
interactive cyclic_group_wf {| intro [] |} 'H :
   sequent ['ext] { 'H >-  isset{cyclic_group{elem_in_G}} }

interactive cyclic_group_op_wf {| intro[] |} 'H :
   sequent [squash] { 'H >- isset{'s1} } -->
   sequent [squash] { 'H >- isset{'s2} } -->
   sequent ['ext] { 'H >- mem{'s1; cyclic_group{elem_in_G}} } -->
   sequent ['ext] { 'H >- mem{'s2; cyclic_group{elem_in_G}} } -->
   sequent ['ext] { 'H >- mem{op{'s1; 's2}; cyclic_group{elem_in_G}} }

interactive cyclic_group_id_wf {| intro[] |} 'H :
   sequent ['ext] { 'H >- mem{id; cyclic_group{elem_in_G}} }

interactive cyclic_group_inv_wf {| intro[] |} 'H :
   sequent [squash] { 'H >- isset{'s} } -->
   sequent ['ext] { 'H >- mem{'s; cyclic_group{elem_in_G}} } -->
   sequent ['ext] { 'H >- mem{inv{'s}; cyclic_group{elem_in_G}} }

interactive cyclic_group_wf1 {| intro [] |} 'H :
   sequent ['ext] { 'H >- isset{'a} } -->
   sequent ['ext] { 'H >- mem{'a; car} } -->
   sequent ['ext] { 'H >-  isset{cyclic_group{'a}} }

interactive cyclic_group_op_wf1 {| intro[] |} 'H :
   sequent ['ext] { 'H >- isset{'a} } -->
   sequent ['ext] { 'H >- mem{'a; car} } -->
   sequent [squash] { 'H >- isset{'s1} } -->
   sequent [squash] { 'H >- isset{'s2} } -->
   sequent ['ext] { 'H >- mem{'s1; cyclic_group{'a}} } -->
   sequent ['ext] { 'H >- mem{'s2; cyclic_group{'a}} } -->
   sequent ['ext] { 'H >- mem{op{'s1; 's2}; cyclic_group{'a}} }

interactive cyclic_group_id_wf1 {| intro[] |} 'H :
   sequent ['ext] { 'H >- isset{'a} } -->
   sequent ['ext] { 'H >- mem{'a; car} } -->
   sequent ['ext] { 'H >- mem{id; cyclic_group{'a}} }

interactive cyclic_group_inv_wf1 {| intro[] |} 'H :
   sequent ['ext] { 'H >- isset{'a} } -->
   sequent ['ext] { 'H >- mem{'a; car} } -->
   sequent [squash] { 'H >- isset{'s} } -->
   sequent ['ext] { 'H >- mem{'s; cyclic_group{'a}} } -->
   sequent ['ext] { 'H >- mem{inv{'s}; cyclic_group{'a}} }

(* Every cyclic group is abelian *)
interactive cyclic_group_abel {| intro[] |} 'H :
   sequent [squash] { 'H >- isset{'s1} } -->
   sequent [squash] { 'H >- isset{'s2} } -->
   sequent ['ext] { 'H >- mem{'s1; cyclic_group{elem_in_G}} } -->
   sequent ['ext] { 'H >- mem{'s2; cyclic_group{elem_in_G}} } -->
   sequent ['ext] { 'H >- equal{op{'s1; 's2}; op{'s2; 's1}} }

(* A subgroup of a cyclic group is cyclic *)
(* NOTE: The proofs for this theorem is pretty straight forward: discuss the cases of 'subg; if it is not {id}, then choose 'm as the smallest positive integer such that power{elem_in_G; 'm} is in 'subg; then apply the division algorithm to show that power{elem_in_G; 'm} is the generator for 'subg. However, it is really hard to be implemented here. *)
interactive subgroup_of_cyclic_group {| intro[] |} 'H :
   sequent ['ext] { 'H >- isset{'subg} } -->
   sequent ['ext] { 'H >- subset{'subg; cyclic_group{elem_in_G}} } -->
   sequent ['ext] { 'H; s1: set; s2: set; a: mem{'s1; 'subg}; b: mem{'s2; 'subg} >- mem{op{'s1; 's2}; 'subg} } -->
   sequent ['ext] { 'H >- mem{id; 'subg} } -->
   sequent ['ext] { 'H; s: set; c: mem{'s; 'subg} >- mem{inv{'s}; 'subg} } -->
   sequent ['ext] { 'H >- exists{int; x. equal{'subg; cyclic_subgroup{power{elem_in_G; 'x}}}} }
