extends Itt_equal
extends Itt_logic

open Tactic_type.Tacticals

define unfold_nequal: nequal{'T; 'a; 'b} <--> not{'a='b in 'T}

topval  triv_nequalT : tactic
