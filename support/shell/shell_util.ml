(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
open Printf

open Refiner.Refiner.RefineError

(*
 * Options to the "ls" command.
 *)
type ls_option =
   LsRewrites
 | LsRules
 | LsUnjustified
 | LsDisplay
 | LsDefault
 | LsFormal
 | LsInformal
 | LsParent
 | LsAll
 | LsNone

   (*
    * Browser-only modes:
    *   LsHandles: display handles to allow selection of arbitrary subterms.
    *)
 | LsHandles

(*
 * LS options.
 *)
module LsOptionCompare =
struct
   type t = ls_option
   let compare = Pervasives.compare
end

module LsOptionSet = Lm_set.LmMake (LsOptionCompare)

(*
 * Folding.
 *)
let string_fold f x s =
   let len = String.length s in
   let rec collect x i =
      if i = len then
         x
      else
         collect (f x s.[i]) (succ i)
   in
      collect x 0

(*
 * Get the option for a char.
 *)
let option_of_char c =
   match c with
      'R' ->
         LsRules
    | 'r' ->
         LsRewrites
    | 'u' ->
         LsUnjustified
    | 'f' ->
         LsFormal
    | 'i' ->
         LsInformal
    | 'd' ->
         LsDisplay
    | 'p' ->
         LsParent
    | 'a' ->
         LsAll
    | 'D' ->
         LsDefault
    | '!' ->
         LsNone
    | 'H' ->
         LsHandles
    | _ ->
         raise (RefineError ("ls", StringError (sprintf "unrecognized option '%s'" (Char.escaped c))))

(*
 * Inverse-operation.
 *)
let char_of_option option =
   match option with
      LsRules -> 'R'
    | LsRewrites -> 'r'
    | LsUnjustified -> 'u'
    | LsFormal -> 'f'
    | LsInformal -> 'i'
    | LsAll -> 'a'
    | LsDisplay -> 'd'
    | LsParent -> 'p'
    | LsDefault -> 'D'
    | LsNone -> '!'
    | LsHandles -> 'H'

(*
 * Translate string options to LS options.
 *)
let ls_options_of_string s =
   string_fold (fun options c ->
         LsOptionSet.add options (option_of_char c)) LsOptionSet.empty s

let string_of_ls_options options =
   let buf = Buffer.create 10 in
      LsOptionSet.iter (fun option ->
            Buffer.add_char buf (char_of_option option)) options;
      Buffer.contents buf

(*
 * Set some additional options.
 * Make sure the set is consistent.
 *)
let ls_options_add options s =
   string_fold (fun options c ->
         let option = option_of_char c in
            match option with
               LsRules ->
                  let options = LsOptionSet.remove options LsUnjustified in
                     LsOptionSet.add options LsRules
             | LsRewrites ->
                  let options = LsOptionSet.remove options LsUnjustified in
                     LsOptionSet.add options LsRules
             | LsUnjustified ->
                  LsOptionSet.singleton LsUnjustified
             | LsFormal ->
                  let options = LsOptionSet.add options LsRules in
                  let options = LsOptionSet.add options LsRewrites in
                     LsOptionSet.add options LsFormal
             | LsInformal ->
                  let options = LsOptionSet.add options LsDisplay in
                     LsOptionSet.add options LsInformal
             | LsDisplay
             | LsParent
             | LsNone
             | LsHandles ->
                  LsOptionSet.add options option
             | LsAll
             | LsDefault ->
                  raise (RefineError ("ls", StringError (sprintf "can't set option '%s'" (Char.escaped c))))) options s

(*
 * Clear some additional options.
 * Make sure the set is consistent.
 *)
let ls_options_clear options s =
   string_fold (fun options c ->
         let option = option_of_char c in
            match option with
               LsRules ->
                  let options = LsOptionSet.remove options LsFormal in
                     LsOptionSet.remove options LsRules
             | LsRewrites ->
                  let options = LsOptionSet.remove options LsFormal in
                     LsOptionSet.remove options LsRules
             | LsDisplay ->
                  let options = LsOptionSet.remove options LsInformal in
                     LsOptionSet.remove options LsDisplay
             | LsUnjustified
             | LsFormal
             | LsInformal
             | LsParent
             | LsHandles ->
                  LsOptionSet.remove options option
             | LsNone
             | LsAll
             | LsDefault ->
                  options) options s

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)