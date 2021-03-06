(*
 * Debugging operations.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Rewrite_sig

type out_channel = Lm_printf.out_channel

module type RewriteDebugSig =
sig
   type rwcontractum
   type rwterm
   type rstack
   type stack
   type varname

   val print_varname : out_channel -> varname -> unit
   val print_prog : out_channel -> rwterm -> unit
   val print_rstack : out_channel -> rstack array -> unit
   val print_stack_item : out_channel -> stack -> unit
   val print_stack : out_channel -> stack array -> unit
   val print_contractum : out_channel -> rwcontractum -> unit
   val print_strict : out_channel -> strict -> unit
   val rstack_item_str : rstack -> string
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
