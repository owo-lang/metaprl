(* This file implements memoize function based on weak array of results
 *
 * -----------------------------------------------------------------
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998 Yegor Bryukhov, Moscow State University
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
 * Author: Yegor Bryukhov
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

module WeakMemo (Hash : Hash_with_gc_sig.HashWithGCSig) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type 'a weak_descriptor = int * int

IFDEF DEBUG_WEAK_MEMO THEN
   type 'a descriptor =
      { descriptor : 'a weak_descriptor;
        desc_name : string;
        anchor : 'a
      }

   exception Inconsistency of string

ELSE
   type 'a descriptor =
      { descriptor : 'a weak_descriptor;
        anchor : 'a
      }
END

(* unused
   exception Cell_is_full
*)

   type ('param, 'arg, 'header, 'weak_header, 'image) t =
      { make_header : 'param -> 'arg -> 'header;
        header_weaken : 'param -> 'header -> 'weak_header;
        compare_header : 'weak_header -> 'weak_header -> bool;
        make_result : 'param -> 'header -> 'image;
        index_table : ('weak_header, 'image weak_descriptor) Hash.t;
        mutable image_array : 'image Weak.t;

        (*
         * All entries image_array.(count) or after are guaranteed to be free.
         *)
        mutable count : int;
        mutable global_count : int;

        (*
         * Keep track of GC status.
         *)
        mutable gc_on : bool;

        (*
         * For debugging, this is the total number of tables
         * that have been created before this table.
         *)
        name : string;
      }

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Hash code for the descriptor.
    *)
   let wd_hash = fst

   (*
    * Release the anchor for possible GC.
    *)
   let weaken d =
      d.descriptor

   (*
    * Descriptors remain static and anchored, and they are positive,
    * so this comparison is always valid.
    * NOTE: if desciptor becomes mutable, fix this.
    *)
   let compare d1 d2 =
      (fst d1.descriptor) - (fst d2.descriptor)

   (*
    * Has the value been collected by GC?
    *)
   let gc_tst image_array (_, (index, _)) =
      match Weak.get image_array index with
          Some _ -> false
        | None -> true

   (*
    * Create a new table.
    *)
   let create halfsize critical_level name make_header header_weaken compare_header make_result =
      let image_array = Weak.create (2 * halfsize) in
         { make_header = make_header;
           header_weaken = header_weaken;
           compare_header = compare_header;
           make_result = make_result;
           image_array = image_array;
           index_table = Hash.create halfsize critical_level Hashtbl.hash compare_header;
           count = 0;
           gc_on = false;
           name = name;
           global_count = 0;
         }

   let create_default name weaken compare make =
      let fail _ _ =
         raise (Invalid_argument "Weak_memo.create_default: applications are not allowed")
      in
         create 17 17 name fail weaken compare make

   let make_descriptor _info i item =
IFDEF DEBUG_WEAK_MEMO THEN
      { descriptor = i;
        desc_name = _info.name;
        anchor = item
      }
ELSE
      { descriptor = i;
        anchor = item
      }
END

   (*
    * Grow the weak array so we can add more entries.
    *)
   let expand_weak_array wa new_size =
      let old_ar_length = Weak.length wa in
         if new_size <= old_ar_length then
            invalid_arg "Can't expand weak array to less size"
         else
            let new_ar = Weak.create new_size in
               Weak.blit wa 0 new_ar 0 old_ar_length;
               new_ar

   (*
    * Store a new value in the weak array.
    * For debugging, check that the entry does not exist.
    *)
   let set info index item =
      DEFINE body =
         Weak.set info.image_array index (Some item);
         let count = info.global_count in
            info.global_count <- count + 1;
            make_descriptor info (index, count) item
      IN
      IFDEF DEBUG_WEAK_MEMO THEN
         match Weak.get info.image_array index with
            Some _ -> invalid_arg "WeakMemo.set: entry is not empty"
          | None ->
                body
      ELSE
         body
      END

   let insert info hash weak_header index item =
      let item = set info index item in
         Hash.insert info.index_table hash weak_header item.descriptor;
         item

   (*
    * Find a value in the table.
    *)
   let lookup info param header =
      let weak_header = info.header_weaken param header in
      let table = info.index_table in
      let hash = Hash.hash table weak_header in
         match Hash.seek table hash weak_header with
            Some ((index, _) as wd) ->
               begin
                  (* The result was previously stored in the table *)
                  match Weak.get info.image_array index with
                     Some item ->
                        (* It has not been collected, so return the value *)
                        make_descriptor info wd item
                   | None ->
                        (* It has been collected, so create a new value *)
                        let item = info.make_result param header in
                           set info index item
               end
          | None ->
               (* This is a new call to the function *)
               let result = info.make_result param header in
                  if info.gc_on then
                     begin
                        (* GC is only triggered when the weak table gets full *)
                        IFDEF DEBUG_WEAK_MEMO THEN
                           if Weak.length info.image_array <> info.count then
                              raise (Inconsistency "weak table length does not match expected value")
                        END;

                        (* Search for a free location in the hash table *)
                        match Hash.gc_iter (gc_tst info.image_array) info.index_table with
                           Some (_, (index, _)) ->
                              (* Found a free entry in the hash table *)
                              insert info hash weak_header index result
                         | None ->
                              (*
                               * The weak array is totally full.
                               * Double the array size, and store the entry
                               * at the first free entry that was created.
                               *)
                              let length = Weak.length info.image_array in
                                 info.gc_on <- false;
                                 info.image_array <- expand_weak_array info.image_array ((length * 2) + 1);
                                 info.count <- succ length;
                                 insert info hash weak_header length result
                     end
                  else
                     (* Store the entry in the next free position in the array *)
                     let count = info.count in
                     let count' = succ count in
                        info.count <- count';
                        if count' = Weak.length info.image_array then
                           begin
                              (* Table looks full, so trigger GC analysis on the next allocation *)
                              Hash.gc_start table;
                              info.gc_on <- true
                           end;
                        insert info hash weak_header count result

   (*
    * Get the value associated with a descriptor.
    * For debugging, check that the value saved in the table
    * still exists and matches the value passed in the index.
    *)
   IFDEF DEBUG_WEAK_MEMO THEN
      let retrieve info _ dsc =
         let index = fst (dsc.descriptor) in
         if Weak.length info.image_array <= index then
            invalid_arg "WeakMemo.retrieve: out of range";
         if dsc.desc_name <> info.name then
             invalid_arg (Lm_printf.sprintf "WeakMemo.retrieve: try to retrieve from wrong table: %s from %s" dsc.desc_name info.name);
         match Weak.get info.image_array index with
            Some item ->
               if dsc.anchor == item then
                  item
               else
                  raise (Inconsistency "descriptor does not match item")
          | None ->
               raise (Inconsistency "item was lost")
   ELSE
      let retrieve _ _ d =
         d.anchor
   END

   let retrieve_hack d =
      d.anchor

   (*
    * Check that the descriptor is valid in this table.
    *)
   let retrieve_check info index =
      match Weak.get info.image_array (fst index.descriptor) with
         Some item ->
            item == index.anchor
       | None ->
            false

   (*
    * Compose the lookup and result functions.
    *)
   let apply info param arg =
      retrieve info param (lookup info param (info.make_header param arg))

end

module MakeMemo = WeakMemo

module TheWeakMemo = WeakMemo (Hash_with_gc.HashWithGC)

module TheMemo = TheWeakMemo

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_hash, term_header_constr"
 * End:
 * -*-
 *)

