(*
 * Print exceptions.
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
 * Copyright (C) 1998-2006 MetaPRL Group, Cornell University and
 * California Institute of Technology
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
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_rformat
open Lm_rprintf

open Refiner.Refiner.TermShape
open Refiner.Refiner.RefineError
open Dform
open Simple_print
open Simple_print.SimplePrint
open File_type_base

open Filter_ocaml
open Filter_grammar
open Filter_type

(*
 * Print an argument list.
 *)
(* unused
let rec format_arg_list db buf = function
   (sl, t) :: tl ->
      let rec format = function
         h::t ->
            format_string buf h;
            format_space buf;
            format t
       | [] ->
            format_term db buf t
      in
         format sl;
         format_space buf;
         format_arg_list db buf tl
 | [] ->
      ()
*)

let format_version buf i =
   let major, minor, rev = unpack_version i in
      format_int buf major;
      format_char buf '.';
      format_int buf minor;
      format_char buf '.';
      format_int buf rev

(*
 * Convert an exception to a string.
 *)
let rec format_exn db buf exn =
   match exn with
      FormatError (name, t) ->
         format_string buf "FormatError:";
         format_space buf;
         format_string buf name;
         format_space buf;
         format_term db buf t
    | BadParamCast (p, s) ->
         format_string buf "Bad param cast:";
         format_space buf;
         format_simple_param buf p;
         format_space buf;
         format_string buf "to";
         format_space buf;
         format_string buf s
    | ParseError s ->
         format_string buf "Parse error:";
         format_space buf;
         format_string buf s
    | PrecNotFound v ->
         format_szone buf;
         format_pushm buf 3;
         format_string buf "Precedence not found:";
         format_hspace buf;
         format_string buf (string_of_shape v);
         format_hspace buf;
         format_string buf "(have you forgotten to write a lex_prec with this precedence on the left hand side?)";
         format_popm buf;
         format_ezone buf;
    | IterfImplemMismatch s ->
         format_pushm buf 3;
         format_string buf "Mismatch between the interface and implementation:";
         format_newline buf;
         format_string buf s;
         format_popm buf
    | BadCommand s ->
         format_string buf "Bad command:";
         format_string buf s
    | EmptyModulePath s ->
         format_string buf "Empty module path:";
         format_space buf;
         format_string buf s
    | Bad_magic s ->
         format_string buf "! File ";
         format_string buf s;
         format_string buf " has a wrong magic number.";
         format_newline buf;
         format_string buf "! This means that is is either not a MetaPRL file";
         format_newline buf;
         format_string buf "! or is not compatible with the version of the MetaPRL you are trying to use.";
         format_newline buf;
         format_string buf "! If you are sure this file does not contain any unsaved data, delete it.";
         format_newline buf;
         format_string buf "! If it does contain unsaved data, you might need to get a different version of MetaPRL";
         format_newline buf;
         format_string buf "! and possibly export the data to a different format."
    | Bad_version (s, versions, version) ->
         format_string buf "! File ";
         format_string buf s;
         format_string buf " has an unsupported version.";
         format_newline buf;
         format_string buf "! MetaPRL currently supports version(s) ";
         List.iter (fun v -> format_version buf v; format_string buf ", ") versions;
         format_newline buf;
         format_string buf "! but the file has version ";
         format_version buf version;
         format_newline buf;
         format_string buf "! If you are sure this file does not contain any unsaved data, delete it.";
         format_newline buf;
         format_string buf "! If it does contain unsaved data, you might need to get a different version of MetaPRL";
         format_newline buf;
         format_string buf "! and possibly export the data to a different format."
    | Ploc.Exc (loc, exn) ->
         format_pushm buf 3;
         format_loc buf loc;
         format_string buf ": ";
         format_newline buf;
         format_szone buf;
         format_exn db buf exn;
         format_ezone buf;
         format_popm buf;
(* TODO[jyh]
    | Pcaml.Qerror (name, where, exn) ->
         let name =
            if name = "" then
               !Quotation.default
            else
               name
         in
            format_pushm buf 3;
            format_string buf "While ";
            (match where with
                Pcaml.Finding -> format_string buf "finding quotation"
              | Pcaml.Expanding -> format_string buf "expanding quotation"
              | Pcaml.ParsingResult _ -> format_string buf "parsing result of quotation");
            format_string buf (" \"" ^ String.escaped name ^ "\":");
            format_space buf;
            format_szone buf;
            format_exn db buf exn;
            format_ezone buf;
            format_popm buf
*)
    | Lm_parser.ParseError (loc, s) ->
         format_pushm buf 3;
         format_string buf "Syntax error:";
         format_space buf;
         format_string buf (Lm_location.string_of_location loc);
         format_space buf;
         format_string buf s;
         format_popm  buf
    | exn ->
         Exn_boot.format_exn db buf exn

(*
 * Print an exception if it occurs, then reraise it.
 *)
let format_message buf s =
   match s with
      None ->
         format_pushm buf 0;
         format_szone buf
    | Some s ->
         format_pushm buf 3;
         format_string buf s;
         format_szone buf;
         format_space buf

let print_stderr_exn db s exn =
   let buf = new_buffer () in
      format_message buf s;
      format_exn db buf exn;
      format_ezone buf;
      format_popm buf;
      format_newline buf;
      output_rbuffer stderr buf;
      flush stderr;
      begin
         match exn with
            RefineError(s, _) ->
               raise(RefineError(s, ToploopIgnoreError))
          | _ ->
               raise (Refine_exn.ToploopIgnoreExn exn)
      end

let print_exn db s f x =
   if Refine_exn.backtrace then
      f x
   else
      try f x with
         exn ->
            print_stderr_exn db s exn

let stderr_exn s exn =
   print_stderr_exn Dform.null_base (Some s) exn

let handle_exn db s loc f =
   if Refine_exn.backtrace then
      f ()
   else
      try f () with
         exn ->
            let exn =
               match exn with
                  Ploc.Exc _ ->
                     exn
                | _ ->
                     Ploc.Exc (loc, exn)
            in
            let buf = new_buffer () in
            let () = format_message buf s in
            let buf =
               try format_exn db buf exn; buf with
                  exn ->
                     let buf = new_buffer () in
                        format_message buf s;
                        format_string buf "!!! Filter_exn.handle_exn: error during exception printing !!!";
                        format_space buf;
                        format_loc buf loc;
                        format_space buf;
                        format_string buf (Printexc.to_string exn);
                        buf
            in
               format_ezone buf;
               format_popm buf;
               format_newline buf;
               output_rbuffer stderr buf;
               flush stderr;
               exit 2

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
