(*
 * Simple recursive type.
 *
 *)

open Refiner.Refiner.Term

include Itt_equal
include Itt_prec
include Itt_subtype

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

declare srec{T. 'B['T]}
declare srecind{'a; p, h. 'g['p; 'h]}

(************************************************************************
 * REWRITES                                                             *
 ************************************************************************)

rewrite reduceSrecind : srecind{'a; p, h. 'g['p; 'h]} <-->
   'g[lambda{a. srecind{'a; p, h. 'g['p; 'h]}}; 'a]

(************************************************************************
 * RULES                                                                *
 ************************************************************************)

(*
 * H >- Ui ext srec(T. B[T])
 * by srecFormation T
 *
 * H, T: Ui >- Ui ext B[T]
 *)
axiom srecFormation 'H 'T :
   sequent ['ext] { 'H; T: univ[@i:l] >- univ[@i:l] } -->
   sequent ['ext] { 'H >- univ[@i:l] }

(*
 * H >- srec(T1. B1[T1]) = srec(T2. B2[T2]) in Ui
 * by srecEquality T S1 S2 z
 *
 * H; T: Ui >- B1[T] = B2[T] in Ui
 * H; S1: Ui; S2: Ui; z: subtype(S1; S2) >- subtype(B1[S1]; B1[S2])
 *)
axiom srecEquality 'H 'T 'S1 'S2 'z :
   sequent [squash] { 'H; T: univ[@i:l] >- 'B1['T] = 'B2['T] in univ[@i:l] } -->
   sequent [squash] { 'H; S1: univ[@i:l]; S2: univ[@i:l]; z: subtype{'S1; 'S2} >- subtype{'B1['S1]; 'B1['S2]} } -->
   sequent ['ext] { 'H >- srec{T1. 'B1['T1]} = srec{T2. 'B2['T2]} in univ[@i:l] }

(*
 * H >- srec(T. B[T]) ext g
 * by srec_memberFormation
 *
 * H >- B[srec(T. B[T])] ext g
 * H >- srec(T. B[T]) = srec(T. B[T]) in Ui
 *)
axiom srec_memberFormation 'H :
   sequent ['ext] { 'H >- 'B[srec{T. 'B['T]}] } -->
   sequent [squash] { 'H >- "type"{srec{T. 'B['T]}} } -->
   sequent ['ext] { 'H >- srec{T. 'B['T]} }

(*
 * H >- x1 = x2 in srec(T. B[T])
 * by srec_memberEquality
 *
 * H >- x1 = x2 in B[srec(T. B[T])]
 * H >- srec(T. B[T]) = srec(T. B[T]) in Ui
 *)
axiom srec_memberEquality 'H :
   sequent [squash] { 'H >- 'x1 = 'x2 in 'B[srec{T. 'B['T]}] } -->
   sequent [squash] { 'H >- "type"{srec{T. 'B['T]}} } -->
   sequent ['ext] { 'H >- 'x1 = 'x2 in srec{T. 'B['T]} }

(*
 * H, x: srec(T. B[T]), J[x] >- C[x]
 * by srecElimination T1 u v w z
 *
 * H, x: srec(T. B[T]), J[x],
 *   T1: Ui,
 *   u: subtype(T1; srec(T. B[T])),
 *   w: v: T1 -> C[v],
 *   z: T[T1]
 * >- C[z]
 *)
axiom srecElimination 'H 'J 'x srec{T. 'B['T]} 'T1 'u 'v 'w 'z univ[@i:l] :
   sequent ['ext] { 'H; x: srec{T. 'B['T]}; 'J['x];
             T1: univ[@i:l];
             u: subtype{'T1; srec{T. 'B['T]}};
             w: v: 'T1 -> 'C['v];
             z: 'B['T1]
           >- 'C['z]
           } -->
   sequent ['ext] { 'H; x: srec{T. 'B['T]}; 'J['x] >- 'C['x] }

(*
 * H, x: srec(T. B[T]); J[x] >- C[x]
 * by srecUnrollElimination y u
 *
 * H, x: srec(T. B[T]); J[x]; y: B[srec(T. B[T])]; u: x = y in B[srec(T. B[T])] >- C[y]
 *)
axiom srecUnrollElimination 'H 'J 'x 'y 'u :
   sequent ['ext] { 'H; x: srec{T. 'B['T]}; 'J['x]; y: 'B[srec{T. 'B['T]}]; u: 'x = 'y in 'B[srec{T. 'B['T]}] >- 'C['y] } -->
   sequent ['ext] { 'H; x: srec{T. 'B['T]}; 'J['x] >- 'C['x] }

(*
 * H >- srecind(r1; h1, z1. t1) = srecind(r2; h2, z2. t2) in S[r1]
 * by srecindEquality lambda(x. S[x]) srec(T. B[T]) T1 u v w z
 *
 * H >- r1 = r2 in srec(T. B[T])
 * H, T1: Ui, z: subtype(T1; srec(T. B[T])),
 *    v: w: T1 -> S[w], w: T[T1]
 *    >- t1[v; w] = t2[v; w] in S[w]
 *)
axiom srecindEquality 'H lambda{x. 'S['x]} srec{T. 'B['T]} 'T1 'u 'v 'w 'z univ[@i:l] :
   sequent [squash] { 'H >- 'r1 = 'r2 in srec{T. 'B['T]} } -->
   sequent [squash] { 'H; T1: univ[@i:l]; z: subtype{'T1; srec{T. 'B['T]}};
               v: w: 'T1 -> 'S['w]; w: 'B['T1]
           >- 't1['v; 'w] = 't2['v; 'w] in 'S['w]
           } -->
   sequent ['ext] { 'H >- srecind{'r1; h1, z1. 't1['h1; 'z1]}
                   = srecind{'r2; h2, z2. 't2['h2; 'z2]}
                   in 'S['r1]
           }

(************************************************************************
 * TACTICS                                                              *
 ************************************************************************)

val is_srec_term : term -> bool
val dest_srec : term -> string * term
val mk_srec_term : string -> term -> term

val is_srecind_term : term -> bool
val dest_srecind : term -> string * string * term * term
val mk_srecind_term : string -> string -> term -> term -> term

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
