(*
 * A basic web server.
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
open Http_server_type

(*
 * Type of web servers.
 *)
type t

(*
 * Channels.
 *)
module Input :
sig
   type t
end

module Output :
sig
   type t

   val close : t -> unit
   val output_char : t -> char -> unit
   val output_string : t -> string -> unit
   val flush : t -> unit
end

(*
 * Info about the local connection.
 *)
type http_info =
   { http_host     : string;
     http_port     : int
   }

(*
 * Helper function for decoding uri's that contain hex chars.
 *)
val decode_hex : string -> string
val encode_hex : string -> string

(*
 * Decode the filename in the URI.
 *)
val decode_uri : string -> string list

(*
 * The body is a list of key/value pairs.
 *)
val parse_post_body : content_type -> string -> (string * string) list

(*
 * Start a synchronous web server on the specified port.
 * The function argument handle client connections.  This
 * version is synchronous, and not threaded.
 *)
type 'a start_handler   = t -> 'a -> 'a
type 'a connect_handler = t -> 'a -> Output.t -> Input.t -> string list -> request_header_entry list -> string -> 'a

val serve_http : 'a start_handler -> 'a connect_handler -> 'a -> int -> unit

(*
 * Save/close the HTTP socket.
 *)
val save_http  : t -> int
val close_http : t -> unit

(*
 * Get the info for the server.
 *)
val http_info : t -> http_info

(*
 * Responses.
 *)
val print_success_header  : Output.t -> response_code -> unit
val print_success_page    : Output.t -> response_code -> Buffer.t -> unit
val print_content_page    : Output.t -> response_code -> string -> Buffer.t -> unit
val print_success_channel : Output.t -> response_code -> in_channel -> unit
val print_content_channel : Output.t -> response_code -> string -> in_channel -> unit
val print_error_page      : Output.t -> response_code -> unit
val print_redirect_page   : Output.t -> response_code -> string -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
