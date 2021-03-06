(*
 * This is a typed interface to the FileBase,
 * where the contents of the files are just typed,
 * marshaled objects.
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
 * Modified by: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open File_base_type

exception Bad_magic of string
exception Bad_version of string * int list * int

(*
 * Type used for common info.
 *)
type ('arg, 'select, 'cooked) common_info

(*
 * Version number represents a major, minor, revision tripple.
 *)
val pack_version : int -> int -> int -> int
val unpack_version : int -> int * int * int

(*
 * Combo construction.
 *)
module MakeIOSingletonCombo (IO : IOSig) (Info : FileTypeInfoSig
                                          with type raw = IO.t) :
   FileTypeComboSig
   with type cooked = Info.cooked
   with type select = Info.select
   with type arg = Info.arg
   with type info = (Info.arg, Info.select, Info.cooked) common_info

module MakeSingletonCombo (Info : FileTypeInfoSig) :
   FileTypeComboSig
   with type cooked = Info.cooked
   with type select = Info.select
   with type arg = Info.arg
   with type info = (Info.arg, Info.select, Info.cooked) common_info

module CombineCombo (Types : FileTypeSummarySig)
   (Info : FileTypeComboSig
            with type cooked = Types.cooked
            with type select = Types.select
            with type arg = Types.arg
            with type info = (Types.arg, Types.select, Types.cooked) common_info)
   (Combo : FileTypeComboSig
            with type cooked = Types.cooked
            with type select = Types.select
            with type arg = Types.arg
            with type info = (Types.arg, Types.select, Types.cooked) common_info) :
   FileTypeComboSig
   with type cooked = Types.cooked
   with type select = Types.select
   with type arg = Info.arg
   with type info = (Types.arg, Types.select, Types.cooked) common_info

(*
 * Create a summary base for a specific format.
 *)
module MakeFileBase (Types : FileTypeSummarySig)
   (Combo : FileTypeComboSig
            with type select = Types.select
            with type cooked = Types.cooked
            with type arg = Types.arg
            with type info = (Types.arg, Types.select, Types.cooked) common_info) :
   FileBaseSig
   with type select = Types.select
   with type cooked = Types.cooked
   with type arg = Types.arg

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
