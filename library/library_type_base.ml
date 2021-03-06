(*
 * This implements a filesystem interface to the library.
 *)

open Lm_debug

open Refiner.Refiner.TermType
open Basic

open Utils
open Library

let _ =
   show_loading "Loading Library_type_base%t"

let library = null_oref()
let connection = null_oref()

exception LibraryException of string

let library_close () =
  if oref_p library
  then (leave (oref_val library);
	disconnect (oref_val connection))
  else raise (LibraryException "Close: No library open.")

open Lm_printf

let library_open host localport remoteport =

  eprintf "%s %d %d %t" host localport remoteport eflush;

  if oref_p library then
    raise (LibraryException "Open: Library already open.")
  else
    (let _ = oref_set library
	(join (oref_set connection (connect "metaprl" host remoteport)) ["metaprl"]) in
    at_exit library_close)   (* nogin: something is strange here *)

let maybe_lib_open () =
  if not (oref_p library) then
    let host = Sys.getenv "NUPRLLIB_HOST"
    and port = int_of_string (Sys.getenv "NUPRLLIB_PORT")
    in
      library_open host port (port+2)
  else ()


let lib_get () =
   maybe_lib_open ();
   oref_val library


(*
 * Save a term to the library.
 * "Magic" is a magic number that is sued to identify the
 * type of the file.
 * nogin: BUG? does the library have a notion of a propocol version?
 *)

let library_set magics magic versions filename term =
  let _ =
    (with_transaction (lib_get())

       (function t ->
         let root = (root t "Files") in

	  (* make filename dir if none *)
	 let dir = try (descendent t root [filename])
	 with _ -> (let ndir = make_directory t root filename in
	 put_property t ndir "NAME" (itoken_term filename);
	 ndir)
	 in

	    (* store term in filename at magic *)
	 ninsert_leaf t dir (string_of_int (List.nth magics magic)) "TERM" term))

  in ()



(*
 * Get a term from the library.
 * BUG: Rich, we need to search the magic numbers from the end.
 * nogin: BUG? does the library have a notion of a propocol version?
 *)

let library_get magics versions filename =

  with_transaction (lib_get())

    (function t ->
      let root = (root t "Files") in
      let magic = List.hd magics in
      let obid = (descendent t root [filename; (string_of_int magic)]) in
      get_term t obid, magic)

(*
 * This "combo" is the module that defines how to fetch
 * an object from the library.  We are passed an argument
 * that describes how to marshal and unmarshal objects to terms.
 *)
module IO =
struct
   type t = term
   let write = library_set
   let read  = library_get
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
