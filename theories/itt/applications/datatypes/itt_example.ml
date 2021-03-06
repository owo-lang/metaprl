(*
 * Display all the elements in a particular theory.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)
open Top_conversionals

extends Itt_theory
extends Itt_nat

open Dtactic

open Itt_bool

interactive curry :
   sequent { <H> >- "type"{'A} } -->
   sequent { <H> >- "type"{'B} } -->
   sequent { <H> >- "type"{'C} } -->
   sequent { <H> >- ('A => 'B => 'C) => ('A & 'B => 'C) }

(*
 * Microwave oven example.
 *)
declare on
declare "open"
declare oven

declare button
declare door
declare action

define unfold_state : state <--> (oven -> bool)
define unfold_exec : exec <--> (nat -> state)

declare eq_action{'a1; 'a2}
declare eq_oven{'o1; 'o2}
declare state_val{'s; 'o}
declare exec_val{'s; 'i}

define unfold_next :
   next{'s; 'a} <-->
      lambda{o.
         ifthenelse{eq_action{'a; button};
            ifthenelse{state_val{'s; 'o};
               ifthenelse{state_val{'s; ."open"};
                  btrue;                        (* on and open *)
                  bfalse};                      (* on and not open *)
               ifthenelse{state_val{'s; ."open"};
                  bnot{eq_oven{'o; on}};       (* not on and open *)
                  eq_oven{'o; on}}};           (* not on and not open *)
            ifthenelse{state_val{'s; on};
               ifthenelse{state_val{'s; ."open"};
                  btrue;                        (* on and open *)
                  bfalse};                      (* on and not open *)
                       bnot{eq_oven{'o; on}}}}}

define unfold_eq_state :
   eq_state{'s1; 's2} <-->
      ((state_val{'s1; on} = state_val{'s2; on} in bool)
       and (state_val{'s1; ."open"} = state_val{'s2; ."open"} in bool))

define unfold_is_exec : is_exec{'e} <-->
   "assert"{bnot{band{state_val{exec_val{'e; 0}; ."open"}; state_val{exec_val{'e; 0}; on}}}} &
   (all i: nat. (eq_state{exec_val{'e; ('i +@ 1)}; next{exec_val{'e; 'i}; button}} or eq_state{exec_val{'e;('i +@ 1)}; next{exec_val{'e; 'i}; door}}))

(*
 * Display forms.
 *)
dform eq_oven_df : parens :: "prec"[prec_bor] :: except_mode[src] :: eq_oven{'o1; 'o2} =
   slot{'o1} `" =o" " " slot{'o2}

dform eq_action_df : parens :: "prec"[prec_bor] :: except_mode[src] :: eq_action{'a1; 'a2} =
   slot{'a1} `" =a " " " slot{'a2}

dform eq_state_df : parens :: "prec"[prec_bor] :: except_mode[src] :: eq_state{'s1; 's2} =
   slot{'s1} `" =s " " " slot{'s2}

dform next_df : next{'s; 'a} =
   `"Next(" slot{'s} `"," " " slot{'a} `")"

dform state_val_df : state_val{'s; 'o} =
   slot{'s} `"." slot{'o}

dform is_exec_df : is_exec{'e} =
   `"IsExec(" slot{'e} `")"

dform exec_val_df : exec_val{'e; 'i} =
   slot{'e} `"[" slot{'i} `"]"

(*
 * Definitions (so we can derive the rules of the system).
 *)
prim_rw unfold_on : on <--> bfalse
prim_rw unfold_open : "open" <--> btrue
prim_rw unfold_oven : oven <--> bool

prim_rw unfold_button : button <--> bfalse
prim_rw unfold_door : door <--> btrue
prim_rw unfold_action : action <--> bool

prim_rw unfold_eq_oven : eq_oven{'s1; 's2} <--> band{bimplies{'s1; 's2}; bimplies{'s2; 's1}}
prim_rw unfold_eq_action : eq_action{'s1; 's2} <--> band{bimplies{'s1; 's2}; bimplies{'s2; 's1}}
prim_rw unfold_state_val : state_val{'s; 'o} <--> ('s 'o)
prim_rw unfold_exec_val : exec_val{'e; 'i} <--> ('e 'i)

let fold_on     = makeFoldC << on     >> unfold_on
let fold_open   = makeFoldC << "open" >> unfold_open
let fold_oven   = makeFoldC << oven   >> unfold_oven
let fold_button = makeFoldC << button >> unfold_button
let fold_door   = makeFoldC << door   >> unfold_door
let fold_action = makeFoldC << action >> unfold_action

let fold_eq_oven = makeFoldC << eq_oven{'s1; 's2} >> unfold_eq_oven
let fold_eq_action = makeFoldC << eq_action{'s1; 's2} >> unfold_eq_action

interactive_rw reduce_eq_oven1 {| reduce |} : eq_oven{on; on} <--> btrue
interactive_rw reduce_eq_oven2 {| reduce |} : eq_oven{on; ."open"} <--> bfalse
interactive_rw reduce_eq_oven3 {| reduce |} : eq_oven{"open"; on} <--> bfalse
interactive_rw reduce_eq_oven4 {| reduce |} : eq_oven{"open"; "open"} <--> btrue

interactive_rw reduce_eq_action1 {| reduce |} : eq_action{button; button} <--> btrue
interactive_rw reduce_eq_action2 {| reduce |} : eq_action{button; door} <--> bfalse
interactive_rw reduce_eq_action3 {| reduce |} : eq_action{door; button} <--> bfalse
interactive_rw reduce_eq_action4 {| reduce |} : eq_action{door; door} <--> btrue

let reduce_next = (higherC unfold_next thenC higherC unfold_state_val thenC reduceC)

(*
 * Axiomatize the oven.
 *)
interactive oven_type {| intro [] |} :
   sequent { <H> >- "type"{oven} }

interactive on_intro {| intro [] |}:
   sequent { <H> >- on in oven }

interactive open_intro {| intro [] |}:
   sequent { <H> >- "open" in oven }

interactive oven_elim {| elim [] |} 'H :
   sequent { <H>; <J[on]> >- 'C[on] } -->
   sequent { <H>; <J["open"]> >- 'C["open"] } -->
   sequent { <H>; x: oven; <J['x]> >- 'C['x] }

(*
 * Axiomatize the action.
 *)
interactive action_type {| intro [] |}:
   sequent { <H> >- "type"{action} }

interactive button_intro {| intro [] |}:
   sequent { <H> >- button in action }

interactive door_intro {| intro [] |}:
   sequent { <H> >- door in action }

interactive action_elim {| elim [] |} 'H :
   sequent { <H>; <J[button]> >- 'C[button] } -->
   sequent { <H>; <J[door]> >- 'C[door] } -->
   sequent { <H>; x: action; <J['x]> >- 'C['x] }

(*
 * Boolean predicates.
 *)
interactive eq_oven_intro {| intro [] |} :
   sequent { <H> >- 'o1 in oven } -->
   sequent { <H> >- 'o2 in oven } -->
   sequent { <H> >- eq_oven{'o1; 'o2} in bool }

interactive eq_action_intro {| intro [] |} :
   sequent { <H> >- 'a1 in action } -->
   sequent { <H> >- 'a2 in action } -->
   sequent { <H> >- eq_action{'a1; 'a2} in bool }

interactive state_val_intro {| intro [] |} :
   sequent { <H> >- 's in state } -->
   sequent { <H> >- 'o in oven } -->
   sequent { <H> >- state_val{'s; 'o} in bool }

interactive exec_val_intro {| intro [] |} :
   sequent { <H> >- 'e in exec } -->
   sequent { <H> >- 'i in nat } -->
   sequent { <H> >- exec_val{'e; 'i} in state }

interactive eq_state_intro {| intro [] |} :
   sequent { <H> >- 's1 in state } -->
   sequent { <H> >- 's2 in state } -->
   sequent { <H> >- "type"{eq_state{'s1; 's2}} }

interactive eq_state_elim {| elim [] |} 'H :
   ["wf"] sequent { <H>; x: eq_state{'s1; 's2}; <J['x]> >- 's1 in state } -->
   ["wf"] sequent { <H>; x: eq_state{'s1; 's2}; <J['x]> >- 's2 in state } -->
   ["main"] sequent { <H>; x: eq_state{'s1; 's2}; <J['x]>;
      a: "assert"{state_val{'s1; on}}; b: "assert"{state_val{'s1; ."open"}};
      c: "assert"{state_val{'s2; on}}; d: "assert"{state_val{'s2; ."open"}} >- 'C['x] } -->
   ["main"] sequent { <H>; x: eq_state{'s1; 's2}; <J['x]>;
      a: "assert"{bnot{state_val{'s1; on}}}; b: "assert"{state_val{'s1; ."open"}};
      c: "assert"{bnot{state_val{'s2; on}}}; d: "assert"{state_val{'s2; ."open"}} >- 'C['x] } -->
   ["main"] sequent { <H>; x: eq_state{'s1; 's2}; <J['x]>;
      a: "assert"{state_val{'s1; on}}; b: "assert"{bnot{state_val{'s1; ."open"}}};
      c: "assert"{state_val{'s2; on}}; d: "assert"{bnot{state_val{'s2; ."open"}}} >- 'C['x] } -->
   ["main"] sequent { <H>; x: eq_state{'s1; 's2}; <J['x]>;
      a: "assert"{bnot{state_val{'s1; on}}}; b: "assert"{bnot{state_val{'s1; ."open"}}};
      c: "assert"{bnot{state_val{'s2; on}}}; d: "assert"{bnot{state_val{'s2; ."open"}}} >- 'C['x] } -->
   sequent { <H>; x: eq_state{'s1; 's2}; <J['x]> >- 'C['x] }

(*
 * Next wf.
 *)
interactive next_wf {| intro [] |} :
   sequent { <H> >- 's in state } -->
   sequent { <H> >- 'a in action } -->
   sequent { <H> >- next{'s; 'a} in state }

(*
 * Itt_nat is incomplete, so let's add some thms.
 *)
interactive nat_sum_wf {| intro [] |} :
   sequent { <H> >- 'i in nat } -->
   sequent { <H> >- 'j in nat } -->
   sequent { <H> >- ('i +@ 'j) in nat }

interactive one_ge_zero_wf {| intro [] |} :
   sequent { <H> >- 1 >= 0 }

(*
 * Is_exec wf.
 *)
interactive is_exec_wf {| intro [] |} :
   sequent { <H> >- 'e in exec } -->
   sequent { <H> >- "type"{is_exec{'e}} }

(*
 * Some nice simplifications for next.
 *)
interactive_rw next_on : state_val{next{'s; button}; on} <--> 's

(*
 * Main theorem.
 *)
interactive safety :
   sequent { <H> >- 'e in exec } -->
   sequent { <H> >- is_exec{'e} } -->
   sequent { <H> >- all i: nat. "assert"{bnot{band{state_val{exec_val{'e; 'i}; ."open"}; state_val{exec_val{'e; 'i}; on}}}} }

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
