(*
 * Implement an HTTP services for serving web pages.
 *
 * ------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright C 1999 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or at your option any later version.
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
open Printf

open Lm_debug
open Lm_threads

open Http_server_type

let debug_http =
   create_debug (**)
      { debug_name = "http";
        debug_description = "HTTP server operations";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The server info is just the socket.
 *)
type t = Lm_inet.server

(*
 * Info about the local connection.
 *)
type http_info =
   { http_host     : string;
     http_port     : int
   }

(*
 * Handler types.
 *)
type 'a start_handler = t -> 'a -> 'a
type 'a connect_handler = t -> 'a -> out_channel -> in_channel -> string list -> request_header_entry list -> string -> 'a

(************************************************************************
 * CONSTANTS                                                            *
 ************************************************************************)

(*
 * HTTP response codes.
 *)
let continue_code               = 100, "Continue"
let ok_code                     = 200, "OK"
let created_code                = 201, "Created"
let accepted_code               = 202, "Accepted"
let no_content_code             = 204, "No Content"
let moved_perm_code             = 301, "Moved Permanently"
let moved_temp_code             = 302, "Moved Temporarily"
let see_other_code              = 303, "See Other"
let not_modified_code           = 304, "Not modified"
let bad_request_code            = 400, "Bad request"
let unauthorized_code           = 401, "Unauthorized"
let forbidden_code              = 402, "Forbidden"
let not_found_code              = 404, "Not found"
let server_error_code           = 500, "Internal server error"
let not_implemented_code        = 501, "Not implemented"
let bad_gateway_code            = 502, "Bad gateway"
let service_unavailable_code    = 503, "Service unavailable"

let get_code = function
   ContinueCode ->           continue_code
 | OkCode ->                 ok_code
 | CreatedCode ->            created_code
 | AcceptedCode ->           accepted_code
 | NoContentCode ->          no_content_code
 | MovedPermCode ->          moved_perm_code
 | MovedTempCode ->          moved_temp_code
 | SeeOtherCode ->           see_other_code
 | NotModifiedCode ->        not_modified_code
 | BadRequestCode ->         bad_request_code
 | UnauthorizedCode ->       unauthorized_code
 | ForbiddenCode ->          forbidden_code
 | NotFoundCode ->           not_found_code
 | ServerErrorCode ->        server_error_code
 | NotImplementedCode ->     not_implemented_code
 | BadGatewayCode ->         bad_gateway_code
 | ServiceUnavailableCode -> service_unavailable_code

(*
 * The protocol we use when on 0.9
 *)
let http_protocol = "HTTP/1.0"

(*
 * Print a success code.
 *)
let print_success_page out code buf =
   let code, msg = get_code code in
      fprintf out "%s %d %s\r\n" http_protocol code msg;
      fprintf out "Content-Length: %d\r\n\r\n" (Buffer.length buf);
      Buffer.output_buffer out buf

(*
 * Print a file.
 *)
let copy outx inx =
   let buffer = String.create 8192 in
   let rec copy () =
      let count = input inx buffer 0 8192 in
         if count <> 0 then
            begin
               output outx buffer 0 count;
               copy ()
            end
   in
      copy ()

let print_gmtime outx time =
   let { Unix.tm_sec = sec;
         Unix.tm_min = min;
         Unix.tm_hour = hour;
         Unix.tm_mday = mday;
         Unix.tm_mon  = mon;
         Unix.tm_year = year;
         Unix.tm_wday = wday
       } = Unix.gmtime time
   in
   let mon_names = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|] in
   let day_names = [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|] in
      fprintf outx "%s, %02d %s %d %02d:%02d:%02d GMT" day_names.(wday) mday mon_names.(mon) year hour min sec

let print_success_channel out code inx =
   let fd = Unix.descr_of_in_channel inx in
   let { Unix.st_size = st_size;
         Unix.st_mtime = st_mtime
       } = Unix.fstat fd
   in
   let code, msg = ok_code in
      fprintf out "%s %d %s\r\n" http_protocol code msg;
      fprintf out "Last-Modified: %a\r\n" print_gmtime st_mtime;
      fprintf out "Cache-Control: public\r\n";
      fprintf out "Content-Length: %d\r\n\r\n" st_size;
      copy out inx

(*
 * For errors, construct a message body.
 *)
let print_error_page out code =
   let code, msg = get_code code in
   let buf = sprintf "<html>
<head>
<title>%s</title>
</head>
<body>
<h1>%s</h1>
</body>
</html>" msg msg in
      fprintf out "%s %d %s\r\n" http_protocol code msg;
      fprintf out "Content-Length: %d\r\n\r\n%s" (String.length buf) buf

(*
 * Redirect.
 *)
let print_redirect_page out code where =
   let code, msg = get_code code in
   let buf = sprintf "<html>
<head>
<title>%s</title>
</head>
<body>
You are being redirected to <a href=\"%s\"><tt>%s</tt></a>
</body>
</html>" msg where where
   in
      fprintf out "%s %d %s\r\n" http_protocol code msg;
      fprintf out "Location: %s\r\n" where;
      fprintf out "Content-Length: %d\r\n\r\n%s" (String.length buf) buf

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Convert two hex chars into a new 8-bit char.
 *)
let unhex c1 c2 =
   let i1 = Lm_string_util.unhex c1 in
   let i2 = Lm_string_util.unhex c2 in
      Char.chr (i1 * 16 + i2)

(*
 * Decode hex characters in the URI.
 *)
let decode_hex uri =
   let len = String.length uri in
   let buf = String.create len in
   let rec convert i j =
      if j = len then
         if i = len then
            buf
         else
            String.sub buf 0 i
      else if uri.[j] = '+' then
         begin
            buf.[i] <- ' ';
            convert (i + 1) (j + 1)
         end
      else if uri.[j] = '%' & j < len - 2 then
         begin
            buf.[i] <- unhex uri.[j + 1] uri.[j + 2];
            convert (i + 1) (j + 3)
         end
      else
         begin
            buf.[i] <- uri.[j];
            convert (i + 1) (j + 1)
         end
   in
      convert 0 0

(*
 * Encode a string into hex.
 *)
let hex code =
   if code < 10 then
      Char.chr (code + (Char.code '0'))
   else
      Char.chr (code - 10 + (Char.code 'a'))

let encode_hex uri =
   let len = String.length uri in
   let buf = String.create (3 * len) in
   let rec convert i j =
      if i = len then
         String.sub buf 0 j
      else
         match uri.[i] with
            ('0'..'9' | 'A'..'Z' | 'a'..'z') as c ->
               buf.[j] <- c;
               convert (succ i) (succ j)
          | c ->
               let code = Char.code c in
                  buf.[j] <- '%';
                  buf.[j + 1] <- hex ((code lsr 4) land 15);
                  buf.[j + 2] <- hex (code land 15);
                  convert (succ i) (j + 3)
   in
      convert 0 0

let decode_uri uri =
   Lm_filename_util.simplify_path (Lm_filename_util.split_path (decode_hex uri))

(*
 * Command line parsing is simple because there
 * is no quoting.
 *)
let parse_args line =
   Lm_string_util.split " \t" line

(************************************************************************
 * Input handling.
 *)

(*
 * Form handler.
 *)
let colon = ':'

let header_line inx =
   let line = input_line inx in
      try
         let index = String.index line colon in
         let length = String.length line in
            if index < length then
               let tag = String.lowercase (String.sub line 0 index) in
               let field = String.sub line (succ index) (length - index - 1) in
                  Some (tag, Lm_string_util.trim field)
            else
               None
      with
         Not_found ->
            None

let rec read_header inx lines =
   try
      match header_line inx with
         Some (tag, field) ->
            if !debug_http then
               eprintf "Http_simple.header line: %s: %s%t" tag field eflush;
            read_header inx ((tag, field) :: lines)
       | None ->
            lines
   with
      End_of_file ->
         lines

(*
 * Header parsing.
 *)
let parse_int s =
   try int_of_string s with
      Failure _ ->
         0

let parse_host s =
   match Lm_string_util.parse_args s with
      [host] ->
         host, None
    | host :: port :: _ ->
         host, Some (parse_int port)
    | _ ->
         "unknown-host", None

let parse_if_match s =
   match Lm_string_util.parse_args s with
      ["*"] ->
         None
    | args ->
         Some args

let parse_request_cache_control field =
   match Lm_string_util.parse_args (String.lowercase field) with
      ["no-cache"] ->
         CacheRequestNoCache
    | ["no-store"] ->
         CacheRequestNoStore
    | ["max-age"; field] ->
         CacheRequestMaxAge (parse_int field)
    | ["max-stale"; field] ->
         CacheRequestMaxStale (parse_int field)
    | ["min-fresh"; field] ->
         CacheRequestMinFresh (parse_int field)
    | ["no-transform"] ->
         CacheRequestNoTransform
    | ["only-if-cached"] ->
         CacheRequestOnlyIfCached
    | [token] ->
         CacheRequestExtension1 token
    | [token1; token2] ->
         CacheRequestExtension2 (token1, token2)
    | _ ->
         CacheRequestExtension0 field

type uri_state =
   StateProto
 | StateHost
 | StatePort
 | StateFile

let colon = ':'

let parse_uri s =
   let len = String.length s in
   let proto = Buffer.create 7 in
   let host = Buffer.create 64 in
   let port = Buffer.create 6 in
   let file = Buffer.create 128 in
   let rec search state j =
      if j <> len then
         let c = s.[j] in
         let j' = succ j in
            match state with
               StateProto ->
                  if c = colon then
                     search StateHost j'
                  else
                     (Buffer.add_char proto c; search StateProto j')
             | StateHost ->
                  if c = colon then
                     search StatePort j'
                  else if c = '/' then
                     search StateFile j'
                  else
                     (Buffer.add_char host c; search StateHost j')
             | StatePort ->
                  if c = '/' then
                     search StateFile j'
                  else
                     (Buffer.add_char port c; search StatePort j')
             | StateFile ->
                  Buffer.add_char file c;
                  search StateFile j'
   in
   let _ = search StateProto 0 in
      { uri_proto = Buffer.contents proto;
        uri_host = Buffer.contents host;
        uri_port =
           (let i = parse_int (Buffer.contents port) in
               if i = 0 then
                  None
               else
                  Some i);
        uri_path = Buffer.contents file
      }

let parse_eq_list sep body =
   let split arg =
      let arg = Lm_string_util.trim arg in
         try
            let index = String.index arg '=' in
            let length = String.length arg in
               if index < length then
                  let tag = String.sub arg 0 index in
                  let tag = decode_hex tag in
                  let field = String.sub arg (succ index) (length - index - 1) in
                  let field = decode_hex field in
                     if !debug_http then
                        eprintf "Http_simple.post_body: %s=%s%t" tag (String.escaped field) eflush;
                     tag, field
               else
                  arg, ""
         with
            Not_found ->
               arg, ""
   in
      if !debug_http then
         eprintf "parse_post_body: \"%s\"%t" (String.escaped body) eflush;
      List.map split (Lm_string_util.split sep body)

let parse_cookies body =
   parse_eq_list ";" body

let parse_post_body body =
   parse_eq_list "&" body

let parse_header_entry (tag, field) =
   match tag with
      "accept" ->
         RequestAccept field
    | "accept-charset" ->
         RequestAcceptCharset field
    | "accept-encoding" ->
         RequestAcceptEncoding field
    | "accept-language" ->
         RequestAcceptLanguage field
    | "authorization" ->
         RequestAuthorization field
    | "cache-control" ->
         RequestCacheControl (parse_request_cache_control field)
    | "connection" ->
         RequestConnection field
    | "content-length" ->
         RequestContentLength (parse_int field)
    | "cookie" ->
         RequestCookies (parse_cookies field)
    | "date" ->
         RequestDate (float_of_int (Ctime.parse_time field))
    | "expect" ->
         RequestExpect
    | "from" ->
         RequestFrom field
    | "host" ->
         let host, port = parse_host field in
            RequestHost (host, port)
    | "if-match" ->
         RequestIfMatch (parse_if_match field)
    | "if-modified-since" ->
         RequestIfModifiedSince (float_of_int (Ctime.parse_time field))
    | "if-none-match" ->
         RequestIfNoneMatch field
    | "if-range" ->
         RequestIfRange field
    | "if-unmodified-since" ->
         RequestIfUnmodifiedSince (float_of_int (Ctime.parse_time field))
    | "max-forwards" ->
         RequestMaxForwards (parse_int field)
    | "pragma" ->
         RequestCacheControl CacheRequestNoCache
    | "proxy-authorization" ->
         RequestProxyAuthorization field
    | "referer" ->
         RequestReferer (parse_uri field)
    | "te" ->
         RequestTE field
    | "trailer" ->
         RequestTrailer field
    | "transfer-encoding" ->
         RequestTransferEncoding
    | "upgrade" ->
         RequestUpgrade (Lm_string_util.parse_args field)
    | "user-agent" ->
         RequestUserAgent field
    | "via" ->
         RequestVia field
    | "warning" ->
         RequestWarning field
    | _ ->
         RequestExtension (tag, field)

let input_header inx =
   List.map parse_header_entry (read_header inx [])

(*
 * POST body.
 *)
let rec find_content_length = function
   h :: t ->
      (match h with
          RequestContentLength i ->
             i
        | _ ->
             find_content_length t)
 | [] ->
      raise Not_found

let rec read_body inx header =
   try
      let length = find_content_length header in
      let body = String.create length in
         if !debug_http then
            eprintf "Http_simple.read_body: trying to read %d chars%t" length eflush;
         really_input inx body 0 length;
         if !debug_http then
            eprintf "Http_simple.read_body: %d, %s%t" length (String.escaped body) eflush;
         body
   with
      Not_found
    | Failure _
    | End_of_file ->
         ""

(************************************************************************
 * Server main.
 *)

(*
 * Handle a connection to the server.
 *)
let handle server connect info client =
   let fd = Lm_inet.file_descr_of_client client in
   let inx = Unix.in_channel_of_descr fd in
   let outx = Unix.out_channel_of_descr fd in
   let line = input_line inx in
   let _ =
      if !debug_http then
         eprintf "Http_simple.handle: %s%t" line eflush
   in
   let args =
      match parse_args line with
         command :: args ->
            String.lowercase command :: args
       | [] ->
            []
   in
   let header = input_header inx in
   let body = read_body inx header in
   let _ =
      if !debug_http then
         eprintf "Httpd_simple.handle: %s%t" line eflush
   in
   let state = connect server info outx inx args header body in
      flush outx;
      state

(*
 * Do the web service.
 *)
let serve connect fd info =
   if !debug_http then
      eprintf "Httpd_simpleimple_httpd: starting web services%t" eflush;

   let rec serve info =
      let client = Lm_inet.accept fd in
      let fd' = Lm_inet.file_descr_of_client client in
      let () =
         if !debug_http then
            eprintf "Httpd_simple: connection on fd=%d%t" (Obj.magic fd') eflush
      in
      let info =
         (* Ignore errors when the connection is handled *)
         try handle fd connect info client with
            Unix.Unix_error _
          | Sys_error _ ->
               if !debug_http then
                  eprintf "Httpd_simple: stopping web services%t" eflush;
               info
      in
         Unix.close fd';
         serve info
   in
      try serve info with
         Unix.Unix_error _
       | Sys_error _ ->
            if !debug_http then
               eprintf "Httpd_simple: service closed%t" eflush

(*
 * Server without threads.
 *)
let serve_http start connect info port =
   let inet = Lm_inet.serve port in
   let info = start inet info in
      serve connect inet info

(*
 * Get the actual port number.
 *)
let http_info inet =
   let host, port = Lm_inet.get_server_host inet in
      { http_host = host;
        http_port = port
      }

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)