(*
 * Implement a set of strings using a splay set.
 *)

module StringOrd =
struct
   type t = string
   let compare = compare
end

module StringSet = Fun_splay_set.Make (StringOrd)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
