(*
 * Simple recursive type.
 *
 *)

open Debug

include Itt_equal
include Itt_prec
include Itt_subtype

(* debug_string DebugLoad "Loading itt_srec..." *)

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

declare srec{T. 'B['T]}
declare srecind{'a; p, h. 'g['p; 'h]}

(************************************************************************
 * REWRITES                                                             *
 ************************************************************************)

primrw srecind : srecind{'a; p, h. 'g['p; 'h]} <-->
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
prim srecFormation 'H 'T :
   ('B['T] : sequent ['ext] { 'H; T: univ[@i:l] >- univ[@i:l] }) -->
   sequent ['ext] { 'H >- univ[@i:l] } =
   srec{T. 'B['T]}

(*
 * H >- srec(T1. B1[T1]) = srec(T2. B2[T2]) in Ui
 * by srecEquality T S1 S2 z
 *
 * H; T: Ui >- B1[T] = B2[T] in Ui
 * H; S1: Ui; S2: Ui; z: subtype(S1; S2) >- subtype(B1[S1]; B1[S2])
 *)
prim srecEquality 'H 'T 'S1 'S2 'z :
   sequent [squash] { 'H; T: univ[@i:l] >- 'B1['T] = 'B2['T] in univ[@i:l] } -->
   sequent [squash] { 'H; S1: univ[@i:l]; S2: univ[@i:l]; z: subtype{'S1; 'S2} >- subtype{'B1['S1]; 'B1['S2]} } -->
   sequent ['ext] { 'H >- srec{T1. 'B1['T1]} = srec{T2. 'B2['T2]} in univ[@i:l] } =
   it

(*
 * H >- srec(T. B[T]) ext g
 * by srec_memberFormation
 *
 * H >- B[srec(T. B[T])] ext g
 * H >- srec(T. B[T]) = srec(T. B[T]) in Ui
 *)
prim srec_memberFormation 'H :
   ('g : sequent ['ext] { 'H >- 'B[srec{T. 'B['T]}] }) -->
   sequent [squash] { 'H >- "type"{(srec{T. 'B['T]})} } -->
   sequent ['ext] { 'H >- srec{T. 'B['T]} } =
   'g

(*
 * H >- x1 = x2 in srec(T. B[T])
 * by srec_memberEquality
 *
 * H >- x1 = x2 in B[srec(T. B[T])]
 * H >- srec(T. B[T]) = srec(T. B[T]) in Ui
 *)
prim srec_memberEquality 'H :
   sequent [squash] { 'H >- 'x1 = 'x2 in 'B[srec{T. 'B['T]}] } -->
   sequent [squash] { 'H >- "type"{(srec{T. 'B['T]})} } -->
   sequent ['ext] { 'H >- 'x1 = 'x2 in srec{T. 'B['T]} } =
   it

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
prim srecElimination 'H 'J 'x srec{T. 'B['T]} 'T1 'u 'v 'w 'z univ[@i:l] :
   ('g['x; 'T1; 'u; 'w; 'z] : sequent ['ext] { 'H; x: srec{T. 'B['T]}; 'J['x];
             T1: univ[@i:l];
             u: subtype{'T1; srec{T. 'B['T]}};
             w: v: 'T1 -> 'C['v];
             z: 'B['T1]
           >- 'C['z]
           }) -->
   sequent ['ext] { 'H; x: srec{T. 'B['T]}; 'J['x] >- 'C['x] } =
   'g['x; 'T1; 'u; 'w; 'z]

(*
 * H, x: srec(T. B[T]); J[x] >- C[x]
 * by srecUnrollElimination y u
 *
 * H, x: srec(T. B[T]); J[x]; y: B[srec(T. B[T])]; u: x = y in B[srec(T. B[T])] >- C[y]
 *)
prim srecUnrollElimination 'H 'J 'x 'y 'u :
   ('g['x; 'y; 'u] : sequent ['ext] { 'H; x: srec{T. 'B['T]}; 'J['x]; y: 'B[srec{T. 'B['T]}]; u: 'x = 'y in 'B[srec{T. 'B['T]}] >- 'C['y] }) -->
   sequent ['ext] { 'H; x: srec{T. 'B['T]}; 'J['x] >- 'C['x] } =
   'g['x; 'x; it]

(*
 * H >- srecind(r1; h1, z1. t1) = srecind(r2; h2, z2. t2) in S[r1]
 * by srecindEquality lambda(x. S[x]) srec(T. B[T]) T1 u v w z
 *
 * H >- r1 = r2 in srec(T. B[T])
 * H, T1: Ui, z: subtype(T1; srec(T. B[T])),
 *    v: w: T1 -> S[w], w: T[T1]
 *    >- t1[v; w] = t2[v; w] in S[w]
 *)
prim srecindEquality 'H lambda{x. 'S['x]} srec{T. 'B['T]} 'T1 'u 'v 'w 'z univ[@i:l] :
   sequent [squash] { 'H >- 'r1 = 'r2 in srec{T. 'B['T]} } -->
   sequent [squash] { 'H; T1: univ[@i:l]; z: subtype{'T1; srec{T. 'B['T]}};
               v: w: 'T1 -> 'S['w]; w: 'B['T1]
           >- 't1['v; 'w] = 't2['v; 'w] in 'S['w]
           } -->
   sequent ['ext] { 'H >- srecind{'r1; h1, z1. 't1['h1; 'z1]} 
                   = srecind{'r2; h2, z2. 't2['h2; 'z2]}
                   in 'S['r1]
           } =
   it

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:52:26  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.3  1996/05/21 02:17:11  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/04/11 13:34:16  jyh
 * This is the final version with the old syntax for terms.
 *
 * Revision 1.1  1996/03/30 01:37:19  jyh
 * Initial version of ITT.
 *
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
