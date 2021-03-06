(*
* This file is part of MetaPRL, a modular, higher order
* logical framework that provides a logical programming
* environment for OCaml and other languages.
*
* See the file doc/htmlman/default.html or visit http://metaprl.org/
* for more information.
*
* Copyright (C) 1998 Lori Lorigo, Richard Eaton, Cornell University
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
* Authors: Lori Lorigo, Richard Eaton
*)

open Refiner.Refiner.TermType

type library
and transaction

(*
 *	Library
 *)

val connect	: string (*servername*) -> string (* remote hostname *)
			-> int (* remote socket *)
			-> Orb.connection

val disconnect	: Orb.connection -> unit


(*
 *	Library instances:
 *
 *	lib_new	: creates new instance of a library.
 *	join	: finds latest instance of library using mnemonic.
 *	restore	: restores named instance of library.
 *	save	: marks a restorable instance of a library.
 *	leave	: leaves library instance running but kills all local state
 *		  associated with it.
 *	lib_close : kill instance of library. join or restore can be used recover.
 *
 *	The mnemonic should be something distinquishing you from othe users.
 *
 *	save/restore allows one to synch local data with library state.
 *	At the moment, Import/Export require remote eval, thus
 *	slow. If this becomes an important feature then it can
 *	be improved.
 *
 *	oid_export only works in scope of callback from save.
 *	oid_import only works in scope of callback from restore.
 *
 *	It is expected that (function t -> ()) will be common callback.
 *
 *	lib_close or leave must still be called after save if session is complete.
 *	If not exports are required then lib_close can be called directly, ie the
 *	cookie returned by close is acceptable as input to restore.
 *	OTOH, save may be called any number of times while a lib is open or joined.
 *
 *)


val lib_new	: Orb.connection -> string (* mnemonic *) -> library
val lib_close	: library -> string (* cookie *)

val join	: Orb.connection -> string list -> library
val leave	: library -> unit

val join_eval	: Orb.connection -> string list -> (term -> term) -> library

val restore	: Orb.connection -> string (* cookie *) -> (transaction -> unit) -> library
val save	: library -> (transaction -> unit) -> string (* cookie *)

val oid_export	: transaction -> object_id -> string
val oid_import	: transaction -> string -> object_id


(*
 *	Transactions : all library functions must be called from within a
 *	  transaction.
 *
 *	- A transaction provides a consistent view of object data irrespective
 *	  of the acts of concurrent transactions.
 *	- Failing out of a transaction leaves the library state unchanged.
 *	- Modifying an object requires a write lock. Attempting to
 *	  lock an object locked by another transaction results in failure.
 *
 *	Note: It is possible to create a single transaction and use it for
 *	the duration of a library connection. This is a bad idea. If you should
 *	fail out of the transaction everything is lost and unrecoverable. Only
 *	committed data is persistent and data is committed only at transaction
 *	end.
 *
 *	The library supports distribution of object data.
 *	A local transaction supplies a consistent view of data distributed
 *	locally but fails if access of non local data is attempted.
 *
 *	The goal is that data required for refinement is distributed and thus no
 *	communication is required with the library during refinement.
 *
 *	BEWARE : failing out of a transaction undoes all modifications to library
 *	  by the transaction. If needed, I can supply a commit call to make changes
 *	  persist prior to transaction end.
 *)

val with_transaction		: library -> (transaction -> 'a) -> 'a
val with_local_transaction	: library -> (transaction -> 'a) -> 'a


(*
 *	Remote evaluation:
 *
 *	Eg : make_directory	: transaction -> object_id -> string -> object_id
 *	 == dag_make_directory	: object_id -> string -> object_id
 *
 *	make_directory t oid s =
 *	 eval_to_object_id t
 *	   (String_ap (Oid_ap (Null_ap (itext_term "dag_make_directory ")) oid s))
 *)


val null_ap			: term -> (term * term list)
val string_ap			: (term * term list) -> string -> (term * term list)
val token_ap			: (term * term list) -> string -> (term * term list)
val object_id_ap		: (term * term list) -> object_id -> (term * term list)

val make_ap	: string 		(* remote unmarshall function name *)
			-> ('a -> term)	(* local marshall function *)
			-> ((term * term list) -> 'a -> (term * term list))


val eval			: transaction -> (term * term list) -> unit

val eval_to_term		: transaction -> (term * term list) -> term
val eval_to_token		: transaction -> (term * term list) -> string
val eval_to_string		: transaction -> (term * term list) -> string
val eval_to_object_id		: transaction -> (term * term list) -> object_id

val make_eval	: string	 	(* remote marshall function name *)
			-> (term -> 'a)	(* local unmarshall function *)
			-> transaction -> (term * term list) -> 'a

val server_loop		: library -> unit


(*
 *	Generic object manipulation
 *)

val create		: transaction
				-> string		(* object type *)
				-> term option		(* initial term or none *)
				-> (string * term) list	(* initial property list *)
				-> object_id

(* delete'd objects do not go away until unreferenced and garbage collected.
 * delete_strong'd objects go away immediately even if referenced.
 *)
val delete		: transaction -> object_id -> unit
val delete_strong	: transaction -> object_id -> unit

val put_term		: transaction -> object_id -> term -> unit
val get_term		: transaction -> object_id -> term

val put_property 	: transaction -> object_id -> string -> term -> unit
val get_property 	: transaction -> object_id -> string -> term
val remove_property 	: transaction -> object_id -> string -> unit

val get_properties 	: transaction -> object_id -> (string * term) list
val put_properties 	: transaction -> object_id -> (string * term) list -> unit

(* activation allows data derived from objects to be distributed.
   The data distributed depends on object type.
 *)
val activate		: transaction -> object_id -> unit
val deactivate		: transaction -> object_id -> unit


(* Objects can be tagged as collectable. If a collectable object is not
 * referenced by a non-collectable object and is not active then it may be
 * removed from the library. By default objects are created as uncollectable.
 * Collection is equivalent to deletion, ie you can pre-empt collection by
 * deleting.
 * At the moment, GC is not being performed. GC implementation will
 * will be accompanied by a recovery method as well.
 *)
val allow_collection	 : transaction -> object_id -> unit
val disallow_collection : transaction -> object_id -> unit


(*
 *	The library supports organizing objects into trees.
 *
 *	The tree is a tree of object_ids. Each directory consists of an assoc
 *	list of names and object_ids. A child of a node may be another directory
 *	or some other object. Directories will be identified by a property.
 *	  - multiple roots with same name not allowed.
 *	  - multiple objects with same name not allowed in a directory.
 *
 *	It is possible to build a tree which does not conform to the tree
 *	structure outlined by using the more primitive functions. The results of
 *	using the tree functions on a non-conforming tree are unpredictable,
 *	however failures will be generated wherever feasible when a
 *	non-conforming tree is detected or when a call may corrupt a tree.
 *	  - Overwriting a directory contents with put_term will result in error.
 *	  - Removing the directory property (via remove_property or
 *	    put_properties) will result in error.
 *
 *	The native nuprl5 library functions will not detect tree corruptions.
 *
 * 	Removing a directory recursively removes the contents:
 *	Root directories are created as uncollectable. Subdirectories and leafs
 *	are created as collectable. Thus, if a directory is removed, the loss of
 *	pointers to its children may allow them to be collected.
 *
 *	To force deletion of contents delete_strong would need to be called
 *	on each member.
 *
 *)

val make_root		: transaction -> string -> object_id
val remove_root	: transaction -> object_id -> unit

val make_directory	: transaction -> object_id -> string -> object_id
val remove_directory	: transaction -> object_id -> string -> unit


(* We allow insertion of objects into the tree. The object inserted may be a dir.
 * If an insertion would cause a cycle, then an error is thrown.
 *)
(* NB: cycle prevention not yet implemented. *)

val make_leaf		: transaction -> object_id
				-> string (* name *) -> string (* type *)
				-> object_id
val remove_leaf	: transaction -> object_id -> string -> unit

val insert_leaf	: transaction
				-> object_id -> string (* name *)
				-> string (* type *) -> term
				-> object_id  			(*lal: perhaps should return unit*)

(* overwrites in dir if name exists. *)
val ninsert_leaf	: transaction
				-> object_id -> string (* name *)
				-> string (* type *) -> term
				-> object_id			(*lal: perhaps should return unit*)


(* NB: cycle prevention not yet implemented. *)
val insert		: transaction -> object_id -> string -> object_id -> unit

val roots		: transaction (* local *) -> (string * object_id) list
val root		: transaction (* local *) -> string -> object_id

val directory_p	: transaction (* local *) -> object_id -> bool

val children		: transaction (* local *) -> object_id -> (string * object_id) list
val child		: transaction (* local *) -> object_id -> string -> object_id
val descendent		: transaction (* local *) -> object_id -> string list -> object_id


(*
val find_root_path	: transaction (* local *) -> object_id -> string list
val find_root_paths	: transaction (* local *) -> object_id -> (string list) list

val parents		: transaction (* local *) -> object_id -> object_id list

val lookup		: transaction (* local *)
				-> object_id -> string list -> object_id

(* The list of paths from one object to another.
 * First object_id arg is start and second is end.
 *)

val find_path		: transaction (* local *)
				-> object_id -> object_id -> string list
val find_paths		: transaction (* local *)
				-> object_id -> object_id -> (string list) list
*)
