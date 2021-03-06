(*
 * This is just abstract interface to an array.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 1999 Jason Hickey, Cornell University
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

(*
 * Normal arrays.
 *)
module type ArraySig =
sig
   type 'a t

   val create : 'a -> 'a t
   val set : 'a t -> int -> 'a -> unit
   val get : 'a t -> int -> 'a
   val length : 'a t -> int
end

(*
 * Weak arrays return an option.
 *)
module type WeakArraySig =
sig
   type 'a t

   val create : unit -> 'a t
   val set : 'a t -> int -> 'a -> unit
   val get : 'a t -> int -> 'a option
   val length : 'a t -> int
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
