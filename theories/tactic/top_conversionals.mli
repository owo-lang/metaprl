(*
 * These are the basic rewriting operations.
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

include Mptop

open Refiner.Refiner.Term
open Refiner.Refiner.RefineError

open Tactic_type.Conversionals

(*
 * Toploop values.
 *)

topval rw : conv -> int -> tactic
topval rwc : conv -> int -> int -> tactic
topval rwAll : conv -> tactic
topval rwcAll : conv -> int -> tactic
topval rwAllAll : conv -> tactic

topval rwh : conv -> int -> tactic
topval rwch : conv -> int -> int -> tactic
topval rwhAll : conv -> tactic
topval rwchAll : conv -> int -> tactic
topval rwhAllAll : conv -> tactic

topval rwa : conv list -> int -> tactic
topval rwca : conv list -> int -> int -> tactic
topval rwaAll : conv list -> tactic
topval rwcaAll : conv list -> int -> tactic
topval rwaAllAll : conv list -> tactic


topval prefix_thenC : conv -> conv -> conv
topval prefix_orelseC : conv -> conv -> conv
topval addrC : int list -> conv -> conv
topval clauseC : int -> conv -> conv
topval idC : conv
topval foldC : term -> conv -> conv
topval makeFoldC : term -> conv -> conv
topval cutC : term -> conv
topval failC : conv
topval failWithC : string -> conv
topval tryC : conv -> conv
topval someSubC : conv -> conv
topval allSubC : conv -> conv
topval higherC : conv -> conv
topval lowerC : conv -> conv
topval sweepUpC : conv -> conv
topval sweepDnC : conv -> conv
topval firstC : conv list -> conv
topval applyAllC : conv list -> conv
topval whileProgressC : conv -> conv
topval untilFailC : conv -> conv
topval repeatC : conv -> conv
topval repeatForC : int -> conv -> conv

(************************************************************************
 * REDUCTION RESOURCE                                                   *
 ************************************************************************)

type reduce_data

resource (term * conv, conv, reduce_data, conv) reduce

topval reduceTopC : conv
topval reduceC : conv

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
