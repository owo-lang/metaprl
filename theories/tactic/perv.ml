(*
 * These are the public pervasive terms.
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
 *)

open Printf
open Mp_debug

open Refiner.Refiner

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Perv%t" eflush

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

declare "nil"
declare "cons"{'car; 'cdr}
declare "string"[s:s]
declare "lambda"{x. 'b}
declare "hyp"{'A; x. 'B}
declare "concl"{'A; 'B}
(* declare "sequent"{'ext; 'A} *)
declare "rewrite"{'redex; 'contractum}

(************************************************************************
 * DISPLAY FORMS                                                        *
 ************************************************************************)

(*
 * Pervasive display forms.
 *)
dform perv_nil_df : "nil" = `""

dform perv_cons_df : "cons"{'car; 'cdr} =
   slot{'car} slot{'cdr}

dform perv_string_df : "string"[s:s] =
   `"\"" slot[s:s] `"\""

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
