extends Itt_int_ext

open Basic_tactics

val ind_term : term
val is_ind_term : term -> bool
val dest_ind : term -> term * term * var * var * term
val mk_ind_term : term -> term -> var -> var -> term -> term

define unfold_nat :
   nat <--> ({x:int | 'x>=0})

define unfold_nat_plus :
   nat_plus <--> ({x:int | 'x>0})

define unfold_finite_nat : nat{'k} <--> int_seg{0; 'k}

define unfoldInd : ind{'n; 'base; k,l. 'up['k;'l]} <-->
                   ind{'n; i,j.it; 'base; k,l . 'up['k;'l]}

define iform unfoldInd1 : ind{'n; 'base; l. 'up['l]} <-->
                    ind{'n; i,j.it; 'base; k,l . 'up['l]}

topval foldInd : conv

(************************************************************************
 * TACTICS                                                              *
 ************************************************************************)

topval natBackInductionT : term -> tactic

topval positiveRule1T : tactic
topval positiveRule2T : tactic
