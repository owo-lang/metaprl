(*
 * Process a binary file.
 * This file is taken from a previously parsed file,
 * or from the library.
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

open Arg
open Printf

open Mp_debug

open File_base_type

open Refiner.Refiner.TermType

open Filter_type
open Filter_summary
open Filter_summary_type
open Filter_cache
open Filter_prog

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_bin%t" eflush


(*
 * Include directories.
 *)
let include_path = ref ["."]

let add_include dir =
   include_path := !include_path @ [dir]

(*
 * Need some info about types and extraction.
 *)
module type CompileInfoSig =
sig
   type proof
   type expr
   type ctyp
   type sig_item
   type str_item

   val extract :
      (unit -> (term, meta_term, unit, ctyp, expr, sig_item) module_info) ->
      (term, meta_term, proof, ctyp, expr, str_item) module_info ->
      (module_path * ctyp resource_info) list ->
      string -> (str_item * (int * int)) list
   val compile : (str_item * (int * int)) list -> unit
end

(*
 * Compile from a pre-parsed file.
 *)
module MakeCompile (**)
   (Info : CompileInfoSig)
   (FilterCache : SummaryCacheSig
    with type sig_expr  = Info.expr
    with type sig_ctyp  = Info.ctyp
    with type sig_item  = Info.sig_item
    with type sig_proof = unit
    with type str_proof = Info.proof
    with type str_expr  = Info.expr
    with type str_ctyp  = Info.ctyp
    with type str_item  = Info.str_item
    with type select    = select_type) =
struct
   let compile name =
      let inline_hook cache (path, info) () =
         ()
      in
      let kind, path =
         if Filename.check_suffix name ".cmiz" then
            InterfaceType, Filename.chop_suffix name ".cmiz"
         else if Filename.check_suffix name ".cmit" then
            InterfaceType, Filename.chop_suffix name ".cmit"
         else if Filename.check_suffix name ".cmoz" then
            ImplementationType, Filename.chop_suffix name ".cmoz"
         else if Filename.check_suffix name ".cmot" then
            ImplementationType, Filename.chop_suffix name ".cmot"
         else
            raise (Failure "Filter_parse.compile: invalid suffix")
      in
      let cache = FilterCache.create !include_path in
      let info, _ = FilterCache.load cache path kind InterfaceType inline_hook () NeverSuffix in
      let check () =
         FilterCache.check info InterfaceType
      in
      let items = Info.extract check (FilterCache.info info) (**)
                     (FilterCache.resources info) path
      in
         Info.compile items
end

(*
 * Proof conversions are always void, since
 * we don't have any interactive proofs.
 *)
module Convert : ConvertProofSig =
struct
   type raw = Obj.t
   type t =
      Term of term
    | Raw of Obj.t

   let to_raw _ = function
      Raw t ->
         t
    | Term t ->
         raise (Failure "Filter_bin.Convert.to_raw: interactive term proof can't be converted to raw")

   let of_raw _ t =
      Raw t

   let to_expr _ t =
      let loc = 0, 0 in
      let body =
         <:expr< raise ( $uid: "Failure"$ $str: "Filter_bin.Convert.to_expr: not implemented"$ ) >>
      in
      let patt = <:patt< _ >> in
         <:expr< fun [ $list: [ patt, None, body ]$ ] >>

   let to_term _ = function
      Term t ->
         t
    | Raw t ->
         raise (Failure "Filter_bin.Convert.to_raw: interactive raw proof can't be converted to term")

   let of_term _ t =
      Term t
end

(*
 * Extractors.
 *)
module Extract = MakeExtract (Convert)

(*
 * Caches.
 *)
module Cache = MakeCaches (Convert)

(*
 * The two caches.
 *)
module SigCompileInfo =
struct
   type proof = unit
   type expr  = MLast.expr
   type ctyp  = MLast.ctyp
   type sig_item = MLast.sig_item
   type str_item = MLast.sig_item

   let extract _ = Extract.extract_sig
   let compile items =
      (!Pcaml.print_interf) items
end

module StrCompileInfo =
struct
   type proof = Convert.t proof_type
   type expr  = MLast.expr
   type ctyp  = MLast.ctyp
   type sig_item = MLast.sig_item
   type str_item = MLast.str_item

   let extract check = Extract.extract_str (check ())
   let compile items =
      (!Pcaml.print_implem) items;
      eprintf "Writing output file%t" eflush;
      flush stdout
end

module SigCompile = MakeCompile (SigCompileInfo) (Cache.SigFilterCache)
module StrCompile = MakeCompile (StrCompileInfo) (Cache.StrFilterCache)

(*
 * Utility to replace a suffix.
 *)
let replace_suffix name suffix =
   let i = String.rindex name '.' in
   let j = String.length suffix in
   let s = String.create (i + j) in
      String.blit name 0 s 0 i;
      String.blit suffix 0 s i j;
      s

(*
 * Compile a file.
 *)
let process_file file =
   if Filename.check_suffix file ".cmiz" or
      Filename.check_suffix file ".cmit"
   then
      begin
         (* An interface file *)
         (* Pcaml.input_file := replace_suffix file ".ppi"; *)
         SigCompile.compile file
      end
   else if Filename.check_suffix file ".cmoz" or
           Filename.check_suffix file ".cmot"
   then
      begin
         (* An implementation file *)
         (* Pcaml.input_file := replace_suffix file ".ppo"; *)
         StrCompile.compile file
      end
   else
      raise (Bad "Filter_bin.main: file has a bogus suffix")

(*
 * Argument specification.
 *)
let spec =
   ["-I", Arg.String add_include, "add an directory to the path for include files";
    "-intf", Arg.String process_file, "compile an interface file";
    "-impl", Arg.String process_file, "compile an implementation file"]

(*
 * Everything standard is taken care of.
 * There should be a single file argument, so
 * process it.
 *)
let main () =
   Pcaml.input_file := "/dev/null";
   Arg.parse spec process_file "Compile a MetaPRL binary file"

let _ = Printexc.catch main ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
