(*
 * Type universes.
 *)

include Fol_type

open Mp_resource
open Tacticals
open Base_auto_tactic

declare univ
declare prop{'t}

dform univ_df : univ = `"Univ"
dform prop_df : prop{'t} = downarrow slot{'t}

prim univ_type 'H 'J : :
   sequent ['ext] { 'H; x: univ; 'J['x] >- "type"{prop{'x}} } =
   trivial

(*
 * Automation.  Add a search tactic to trivialT.
 *)
let nthUnivT i p =
   let j, k = Sequent.hyp_indices p i in
      univ_type j k p

let trivial_resource =
   Mp_resource.improve trivial_resource (**)
      { auto_name = "nthUnivT";
        auto_prec = trivial_prec;
        auto_tac = onSomeHypT nthUnivT
      }

(*
 * -*-
 * Local Variables:
 * Caml-master: "pousse"
 * End:
 * -*-
 *)
