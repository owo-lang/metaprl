(*
 * Define a resource that can be used to initialize tactic_arg.
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
open Lm_printf

open Tactic_type.Tactic
open Tactic_type
open Mp_resource

(*
 * Add a resource to initialize tactic arguments.
 *)
let resource_info =
   Functional (**)
      { fp_empty    = [];
        fp_add      = (fun options x -> x :: options);
        fp_retr     = (fun x -> x)
      }

type initialize_entry = tactic_arg -> tactic_arg

let resource (initialize_entry, initialize_entry list) proof_initialize =
   resource_info

let initialize_arg p =
   let funs = Sequent.get_resource_arg p get_proof_initialize_resource in
      (* eprintf "Initializer functions: %d@." (List.length funs); *)
      List.fold_left (fun p f -> f p) p funs

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
