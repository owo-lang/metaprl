(*
 * Free variable calculations.
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
 *
 *)

include Refl_term
include Refl_var_set

open Tactic_type.Conversionals
open Base_dtactic

(************************************************************************
 * SYNTAX                                                               *
 ************************************************************************)

declare free_vars{'t}

(************************************************************************
 * DISPLAY                                                              *
 ************************************************************************)

dform free_vars_df : mode[prl] :: free_vars{'t} =
   `"FV(" pushm[0] slot{'t} popm `")"

(************************************************************************
 * DEFINITIONS                                                          *
 ************************************************************************)

prim_rw unfold_free_vars : free_vars{'t} <-->
   match_term{'t; v, tl. ifthenelse{is_nil{'tl};
                            vsingleton{'v};
                            list_ind{'tl; vempty; h, t, g. vunion{free_vars{'h}; 'g}}};
                  op, bterms.
                     list_ind{'bterms; vempty; h, t, g.
                        match_bterm{'h; sl, t. vunion{vsub{free_vars{'t}; voflist{'sl}}; 'g}}}}

let fold_free_vars = makeFoldC << free_vars{'t} >> unfold_free_vars

(************************************************************************
 * REDUCTIONS                                                           *
 ************************************************************************)

interactive_rw reduce_free_vars_bvar :
   free_vars{bvar{'v; 'tl}} <-->
      ifthenelse{is_nil{'tl};
                 vsingleton{'v};
                 list_ind{'tl; vempty; h, t, g. vunion{free_vars{'h}; 'g}}}

interactive_rw reduce_free_vars_term :
   free_vars{term{'op; 'bterms}} <-->
      list_ind{'bterms; vempty; h, t, g.
                  match_bterm{'h; sl, t. vunion{vsub{free_vars{'t}; voflist{'sl}}; 'g}}}

let resource reduce += [
   << free_vars{bvar{'v; 'tl}} >>, reduce_free_vars_bvar;
   << free_vars{term{'op; 'bterms}} >>, reduce_free_vars_term
]

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

(*
 * Well-formedness.
 *)
interactive free_vars_wf1 {| intro [] |} 'H :
   [wf] sequent [squash] { 'H >- 't IN raw_term_type } -->
   sequent ['ext] { 'H >- free_vars{'t} IN var_set }

(*
 * Functionality.
 *)
interactive free_vars_fun1 {| intro [] |} 'H 't1 'f 'v1 :
   [wf] sequent [squash] { 'H >- 't1 IN raw_term_type } -->
   [wf] sequent [squash] { 'H >- 't2 IN raw_term_type } -->
   [wf] sequent [squash] { 'H >- 'f IN vmap_type } -->
   [main] sequent [squash] { 'H >- "assert"{eq_alpha_term{'f; 't1; 't2}} } -->
   [wf] sequent [squash] { 'H >- 'v1 IN var_type } -->
   [wf] sequent [squash] { 'H >- 'v2 IN var_type } -->
   [main] sequent [squash] { 'H >- "assert"{vmember{'v1; free_vars{'t1}}} } -->
   [main] sequent [squash] { 'H >- "assert"{vmap_compare{'v1; 'v2; 'f}} } -->
   sequent ['ext] { 'H >- "assert"{vmember{'v2; free_vars{'t2}}} }

interactive free_vars_wf2 {| intro [] |} 'H :
   [wf] sequent [squash] { 'H >- 't IN term_type } -->
   sequent ['ext] { 'H >- free_vars{'t} IN var_set }

(************************************************************************
 * TACTICS                                                              *
 ************************************************************************)

(*
 * -*-
 * Local Variables:
 * Caml-master: "mp.run"
 * End:
 * -*-
 *)
