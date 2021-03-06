open Jlogic_sig
open Jtypes
open Jordering

(* Debugging *)

val print_equations :
   (position list * (position list * position list)) list -> unit

val print_tunify : int * (position * position list) list -> unit

val shorten : position list -> position list -> (position list * position list)

type equation = position list * (position list * position list)

module type JQuantifierSig =
sig

   val build_ordering :
      calculus ->
      position list ->
      position list ->
      Set.t PMap.t ->
      Set.t ->
      Set.t PMap.t

   val add_fo_eqlist : equation list -> equation list -> equation list

end

module JQuantifier (_ : JLogicSig) : JQuantifierSig

type trace_entry =
	 ((position list * position list * position list * position list) *
     (equation list * (int * (position * position list) list) * Set.t PMap.t) *
     equation list * int list
	 )

type tracelist = trace_entry list list

module JTUnifyQ (_ : JLogicSig) :
sig

	val do_stringunify :
      calculus ->
		position list ->
		position list ->
		position ->
		position ->
		equation list ->
		equation list ->
		Set.t PMap.t ->
		Set.t ->
		tracelist ->
		int ->
		(int * (position * position list) list) * (* unifier *)
		(equation list) *
      (* applied new eqlist *)
		Set.t PMap.t *
		tracelist

end

module JTUnifyProp (_ : JLogicSig) :
sig

   val do_stringunify :
      calculus ->
      position list ->
      position list ->
      position ->
      position ->
      equation list ->
      equation list ->
      Set.t PMap.t ->
      Set.t ->
		tracelist ->
		int ->
      (int * (position * position list) list) * (* unifier *)
      (equation list) *
      (* applied new eqlist *)
      Set.t PMap.t *
		tracelist

end
