(*
 * Functional Intermediate Representation formalized in MetaPRL.
 *
 * A test program to test MC <--> MetaPRL FIR translation code.
 * This module tests the FIR types.
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
 * Author: Brian Emre Aydemir
 * Email:  emre@its.caltech.edu
 *)

(* Make someone happy... *)

include Base_theory

(* Open MC namespaces. *)

open Fir

(* Open MetaPRL namespaces. *)

open Mp_mc_connect_ty
open Simple_print.SimplePrint

(* Open ML namespaces. *)

open Printf

(*************************************************************************
 * Define test functions.
 *************************************************************************)

let test_tyEnum () =
   printf "test_tyEnum: (TyEnum 12)\n";
   let t = term_of_ty (TyEnum 12) in
   let t' = ty_of_term t in
      print_simple_term t;
      match t' with
         TyEnum 12 -> printf "\nTest passes.\n\n"
       | _ -> printf "\nTest FAILS.\n\n"

(*************************************************************************
 * Define a function to run all the above tests.
 *************************************************************************)

let run_tests () =
   test_tyEnum ()
