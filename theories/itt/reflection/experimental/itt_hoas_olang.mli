(*
 * Some utilities for simplifying the reflection theory.
 * These should eventually be migrated into the reflection
 * theory proper as necessary.
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
extends Itt_hoas_sequent_native

open Basic_tactics

(************************************************************************
 * A version of languages based on operator lists.
 *)
declare olang{'ops}

topval fold_olang : conv

(************************************************************************
 * Grammar.
 *)
declare tok_Lang          : Terminal

lex_token xterm : "Lang" --> tok_Lang

lex_prec nonassoc [tok_Lang] = prec_not

production xterm_term{olang{'ops}} <--
   tok_Lang; xterm_term{'ops}

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)