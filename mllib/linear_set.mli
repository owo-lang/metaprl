(*
 * This module provides a linearly ordered numbered set implementation
 * with lazy functions based on splay trees
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
 * Author: Alexey Nogin
 *)
module type LinearSetSig =
sig
   type elt
   type t
   type index = int

   val length : t -> int
   val get : t -> index -> elt
   val make : int -> elt -> t
   val create : int -> elt -> t
   val to_list : t -> elt list
   val of_list : elt list -> t
   val iter : (elt -> unit) -> t -> unit
   val split : t -> index -> t * elt * t
   val append : t -> elt -> t -> t
   val append_list : t -> elt list -> t -> t
   val lazy_apply : (elt -> elt) -> t -> t
   val lazy_sub_map : (elt -> elt) -> t -> index -> index -> t

   val mapi : (index -> elt -> elt) -> t -> t
   val init : int -> (index -> elt) -> t
   val collect : (elt, t) Array_util.array_part list -> t
end

module type TypeSig =
sig
   type t
end

module Make (Type : TypeSig) : LinearSetSig with type elt = Type.t
