doc <:doc<
   @spelling{reversibility}
   @module[Itt_functions]

      In this module we define the basic concepts of functions: image,
   surjection, reversible function and so on.


   @docoff
   ----------------------------------------------------------------

   @begin[license]

   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/htmlman/default.html or visit http://metaprl.org/
   for more information.

   Copyright (C) 1998 Jason Hickey, Cornell University

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Authors:
   Alexei Kopylov @email{kopylov@cs.caltech.edu}

   @end[license]
>>

doc <:doc<
   @parents
>>

extends Itt_tunion
extends Itt_logic
extends Itt_subset2
extends Itt_fun2
extends Itt_power
extends Itt_sqsimple

doc docoff

open Basic_tactics
open Itt_struct
open Itt_equal

doc <:doc<
   @modsection{Image of a function}
   @modsubsection{Definitions}
   If we have a function f: A -> B, then we can define an image of A in B.
>>

define unfold_Img: Img{'f;'A;'B} <--> { b:'B | exst a:'A. 'f('a) = 'b in 'B }

dform image_df :  Img{'f;'A;'B} = `"Img" sup{'B} slot{'f} `"(" slot{'A} `")"


doc <:doc<
   @modsubsection{Basic Rules}
   <<Img{'f;'A;'B}>> is a well-formed type
   whenever $A$ and $B$ are types and $f$ is a function from $A$ to $B$.
   Moreover image is a @i{subset} of $B$.
>>

interactive img_wf {| intro[] |} :
   sequent{ <H> >- "type"{'A} } -->
   sequent{ <H> >- "type"{'B} } -->
   sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent{ <H> >- "type"{Img{'f;'A;'B}} }

interactive img_wf2 {| intro[] |} :
   sequent{ <H> >- "type"{'A} } -->
   sequent{ <H> >- "type"{'B} } -->
   sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent{ <H> >- Img{'f;'A;'B} subset 'B }

interactive img_wf3 {| intro[] |} :
   sequent{ <H> >- 'A in univ[i:l] } -->
   sequent{ <H> >- 'B in univ[i:l] } -->
   sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent{ <H> >- Img{'f;'A;'B} in univ[i:l] }

interactive img_wf4 {| intro[] |} :
   sequent{ <H> >- 'A in univ[i:l] } -->
   sequent{ <H> >- 'B in univ[i:l] } -->
   sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent{ <H> >- Img{'f;'A;'B} in Power[i:l]{'B} }


doc <:doc<
   Introduction rule is simple: if <<'a>> is in <<'A>> then of course <<'f('a)>> is in <<Img{'f;'A;'B}>>.
>>

interactive img_intro {| intro[] |} :
   sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent{ <H> >- 'a in 'A} -->
   sequent{ <H> >- 'f('a) in Img{'f;'A;'B} }


doc <:doc<
   Basically, the elimination rule says that if we want to prove something for all elements of <<Img{'f;'A;'B}>>,
   then it is enough to prove it for all elements of the form <<'f('a)>> where <<'a in 'A>>.
   But the rule has extra conditions.
   First it holds only for squash-stable conclusions (see @hrefmodule[Itt_squash]).
   Second, it requires $B$ to be simple (see @hrefmodule[Itt_squiggle]).

   Later we will prove that these conditions could be abandoned if $f$ is @i{reversible}
   (rule @hrefrule[img_elim_reversible]).
>>

interactive img_elim {| elim[] |} 'H:
   sequent{ <H>; b:Img{'f;'A;'B}; <J['b]> >- sqsimple{'B} } -->
   sequent{ <H>; a:'A; <J['f('a)]> >-  'u['f('a)] = 'v['f('a)] in 'C['f('a)] } -->
   sequent{ <H>; b:Img{'f;'A;'B}; <J['b]> >- 'u['b] = 'v['b] in 'C['b] }


doc <:doc<
   @modsubsection{Subtyping}
   Image is monotone w.r.t. subtyping.
>>
interactive img_subtyping {| intro[] |} :
   sequent{ <H> >- 'A_1 subtype 'A_2 } -->
   sequent{ <H> >- 'B_1 subtype 'B_2 } -->
   [wf] sequent{ <H> >- 'f in 'A_2 -> 'B_1 } -->
   sequent{ <H> >- Img{'f;'A_1;'B_1} subtype Img{'f;'A_2;'B_2} }

(* TODO: Also monotone w.r.t subseting *)

doc <:doc<
   @modsubsection{Alternative definitions}
>>

define alternative_Img: "Img'"{'f;'A;'B} <--> Union a:'A.  singleton{'f('a); 'B}

dform image_df :  "Img'"{'f;'A;'B} = `"Img'" sup{'B} slot{'f} `"(" slot{'A} `")"

interactive equivalence_of_definions:
   [wf] sequent{ <H> >- "type"{'A} } -->
   [wf] sequent{ <H> >- "type"{'B} } -->
   [wf] sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent { <H> >- ext_equal{ Img{'f;'A;'B};  "Img'"{'f;'A;'B} }  }


doc <:doc<
   @modsection{Inverse functions}
   @modsubsection{Definitions}

>>

define unfold_inverse: inverse{'g;'f;'A} <--> compose{'g;'f} = id in 'A -> 'A

doc <:doc<
   If << compose{'g;'f} = id in 'A -> 'A>> then function $f$ is called @i{right inverse} of function $g$ and
   $g$ is a @i{left inverse} of $f$.
>>


define unfold_LInverse: LInverse{'f;'A;'B} <--> {g:'B -> 'A | inverse{'g;'f;'A} }


define unfold_RInverse: RInverse{'f;'A;'B} <--> {g:'B -> 'A | inverse{'f;'g;'B} }

doc <:doc<
   A function <<f:'A->'B>> is a surjection if it has a right inverse.
   That is, for any element in <<'B>> we can find its prototype in <<'A>>.
   A function is an injection if it has a left inverse,
   Note that in constructive theory is a stronger statement than just saying
   that $f$ does not map two elements of $A$ to one element of $B$.
>>

(* This should be IO-abstraction ??? *)
define unfold_is_injection : is_injection{'f;'A;'B} <--> LInverse{'f;'A;'B}
define unfold_is_surjection : is_surjection{'f;'A;'B} <--> RInverse{'f;'A;'B}

dform  is_injection_df: is_injection{'f;'A;'B} = slot{'f} `" is injection(" ('A -> 'B) `")"
dform  is_surjection_df: is_surjection{'f;'A;'B} = slot{'f} `" is surjection(" ('A -> 'B) `")"


(*TODO: Surjection{'A;'B}={f:... }.
  Note however that <<'f in Surjection('A;'B)>> is much weaker than <<is_surjection{'f;'A;'B}>> since
  the former hides the inverse function.
*)

doc <:doc<
   A function <<f:'A->'B>> is @i{reversible} if it has an inverse function $g$ from its image
   to $A$. In this case $g$ is a @i{reverse} function of $f$.
   In other words, a function is reversible if it is a surjection onto its own image.
   In classical mathematics
   all functions are reversible. However, in constructive theory some functions are
   computable only one way. Such functions are not reversible.
>>

define unfold_RReverse: RReverse{'f;'A;'B} <--> RInverse{'f;'A;Img{'f;'A;'B}}
define unfold_LReverse: LReverse{'f;'A;'B} <--> LInverse{'f;'A;Img{'f;'A;'B}}

define unfold_is_reversible: is_reversible{'f;'A;'B} <--> RReverse{'f;'A;'B}

dform  is_reversible_df: is_reversible{'f;'A;'B} = slot{'f} `" is reversible(" ('A -> 'B) `")"


doc <:doc<
   @modsubsection{Basic Rules}
   @paragraph{Well-formedness}
>>


interactive inverse_wf {| intro[] |} 'B:
   [wf] sequent{ <H> >- "type"{'A} } -->
   [wf] sequent{ <H> >- 'f in 'A -> 'B } -->
   [wf] sequent{ <H> >- 'g in 'B -> 'A } -->
   sequent { <H> >-  "type"{inverse{'g;'f;'A}}  }

doc <:doc<
   Note that <<inverse{'g;'f;'A}>> could be true and well-formed even
   if there is no such $B$ that <<'f in 'A->'B>> and <<'g in 'B->'A>>.
   In this case well-formedness could be proved by @hrefrule[inverse_intro].
>>

interactive linverse_wf {| intro[] |} :
   [wf] sequent{ <H> >- "type"{'A} } -->
   [wf] sequent{ <H> >- "type"{'B} } -->
   [wf] sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent { <H> >-  "type"{LInverse{'f;'A;'B}}  }

interactive rinverse_wf {| intro[] |} :
   [wf] sequent{ <H> >- "type"{'A} } -->
   [wf] sequent{ <H> >- "type"{'B} } -->
   [wf] sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent { <H> >-  "type"{RInverse{'f;'A;'B}}  }

interactive rreverse_wf {| intro[] |} :
   [wf] sequent{ <H> >- "type"{'A} } -->
   [wf] sequent{ <H> >- "type"{'B} } -->
   [wf] sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent { <H> >-  "type"{RReverse{'f;'A;'B}}  }

interactive lreverse_wf {| intro[] |} :
   [wf] sequent{ <H> >- "type"{'A} } -->
   [wf] sequent{ <H> >- "type"{'B} } -->
   [wf] sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent { <H> >-  "type"{LReverse{'f;'A;'B}}  }

doc <:doc<
   @paragraph{Elimination}
>>


interactive inverse_elim {| elim[] |} 'H 'x:
   sequent{ <H>; inverse{'g;'f;'A}; <J> >- 'x in 'A } -->
   sequent{ <H>; inverse{'g;'f;'A}; 'g('f('x)) = 'x in 'A; <J> >- 'C  } -->
   sequent{ <H>; inverse{'g;'f;'A}; <J> >- 'C  }

interactive linverse_elim {| elim[] |} 'H:
   sequent{ <H>; g:'B -> 'A; inverse{'g;'f;'A}; <J['g]> >- 'C['g]  } -->
   sequent{ <H>; g:LInverse{'f;'A;'B}; <J['g]> >- 'C['g]  }

interactive rinverse_elim {| elim[] |} 'H 'y:
   sequent{ <H>; g:RInverse{'f;'A;'B}; <J['g]> >- 'y in 'B } -->
   sequent{ <H>; g:RInverse{'f;'A;'B}; <J['g]>; 'g 'y in 'A; 'f('g('y))='y in 'B >- 'C['g]  } -->
   sequent{ <H>; g:RInverse{'f;'A;'B}; <J['g]> >- 'C['g]  }

interactive rreverse_elim {| elim[] |} 'H 'y:
   sequent{ <H>; g:RReverse{'f;'A;'B}; <J['g]> >- 'y in Img{'f;'A;'B} } -->
   sequent{ <H>; g:RReverse{'f;'A;'B}; <J['g]>;  'g 'y in 'A; 'f('g('y))='y in 'B >- 'C['g]  } -->
   sequent{ <H>; g:RReverse{'f;'A;'B}; <J['g]> >- 'C['g]  }

interactive lreverse_elim {| elim[] |} 'H:
   sequent{ <H>; g:Img{'f;'A;'B} -> 'A; inverse{'g;'f;'A}; <J['g]> >- 'C['g]  } -->
   sequent{ <H>; g:LReverse{'f;'A;'B}; <J['g]> >- 'C['g]  }




doc <:doc<
   @paragraph{Introduction}
>>

interactive inverse_intro {| intro[AutoMustComplete] |}:
   [wf] sequent{ <H> >- "type"{'A} } -->
   sequent{ <H>; x:'A >- 'g('f('x)) = 'x in 'A } -->
   sequent{ <H> >-  inverse{'g;'f;'A}  }

interactive linverse_mem_intro {| intro[AutoMustComplete] |}:
   sequent{ <H> >- 'g in 'B -> 'A } -->
   sequent{ <H> >-  inverse{'g;'f;'A} } -->
   sequent{ <H> >-  'g in LInverse{'f;'A;'B}  }

interactive rinverse_mem_intro {| intro[AutoMustComplete] |}:
   sequent{ <H> >- 'g in 'B -> 'A } -->
   sequent{ <H> >-  inverse{'f;'g;'B} } -->
   sequent{ <H> >-  'g in RInverse{'f;'A;'B}  }

interactive rinverse_intro {| intro[AutoMustComplete] |}:
   [wf] sequent{ <H> >- "type"{'B} } -->
   sequent{ <H>; y:'B >- exst x:'A. 'f 'x ='y in 'B } -->
   sequent{ <H> >-  RInverse{'f;'A;'B}  }

interactive rreverse_mem_intro {| intro[AutoMustComplete] |}:
   sequent{ <H> >- 'g in Img{'f;'A;'B} -> 'A } -->
   sequent{ <H> >-  inverse{'f;'g;Img{'f;'A;'B} } } -->
   sequent{ <H> >-  'g in RReverse{'f;'A;'B}  }

interactive rreverse_intro {| intro[AutoMustComplete] |}:
   [wf] sequent{ <H> >- "type"{Img{'f;'A;'B}} } -->
   sequent{ <H>; y:Img{'f;'A;'B} >- exst x:'A. 'f 'x ='y in 'B } -->
   sequent{ <H> >-  RReverse{'f;'A;'B}  }

doc <:doc<
   @modsubsection{Properties}
    Unfortunately the above introduction rules for reverse type are not very helpful,
    since after applying those rule we need to eliminate <<Img{'f;'A;'B}>> in hypothesis.
   But @hrefrule[img_elim] is too weak,
   and @hrefrule[img_elim_reversible] needs the assumption that <<RReverse{'f;'A;'B}>>,
   which is exactly what we are trying to prove!
   But if $B$ is simple then we can prove that if $g$ is a left inverse of $f$ then $g$ is also
   a right inverse of $f$ as a function from the image of $f$:
>>

interactive linverse_is_rinverse {| intro[SelectOption 1] |}:
   [wf] sequent{ <H> >- "type"{'A} } -->
   sequent{ <H> >- sqsimple{'B} } -->
   [wf] sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent{ <H> >-  LReverse{'f;'A;'B} subtype RReverse{'f;'A;'B} }

doc <:doc<
    Thus, the following rules hold:
>>

interactive rreverse_mem_intro_simple {| intro[SelectOption 1] |}:
   [wf] sequent{ <H> >- "type"{'A} } -->
   sequent{ <H> >- sqsimple{'B} } -->
   [wf] sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent{ <H> >- 'g in LReverse{'f;'A;'B}  } -->
   sequent{ <H> >- 'g in RReverse{'f;'A;'B}  }

interactive rreverse_intro_simple {| intro[SelectOption 1] |} :
   [wf] sequent{ <H> >- "type"{'A} } -->
   sequent{ <H> >- sqsimple{'B} } -->
   [wf] sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent{ <H> >- LReverse{'f;'A;'B}  } -->
   sequent{ <H> >- RReverse{'f;'A;'B}  }

interactive lreverse_mem_intro {| intro[AutoMustComplete] |}:
   [wf] sequent{ <H> >- "type"{'A} } -->
   sequent{ <H> >- sqsimple{'B} } -->
   [wf] sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent{ <H> >- 'g in void -> void } -->
   sequent{ <H> >-  inverse{'g;'f;'A } } -->
   sequent{ <H> >-  'g in LReverse{'f;'A;'B}  }

interactive lreverse_intro {| intro[AutoMustComplete] |} 'g:
   [wf] sequent{ <H> >- "type"{'A} } -->
   sequent{ <H> >- sqsimple{'B} } -->
   [wf] sequent{ <H> >- 'f in 'A -> 'B } -->
   sequent{ <H> >- 'g in void -> void } -->
   sequent{ <H> >-  inverse{'g;'f;'A } } -->
   sequent{ <H> >-  LReverse{'f;'A;'B}  }

interactive rreverse_subtype 'A:
   sequent{ <H> >- '"A'" subtype 'A  } -->
   sequent{ <H> >- RReverse{'f;'A;'B}  } -->
   sequent{ <H> >- RReverse{'f;'"A'";'B}  }

(* TODO:
   Inverse = LInverse isect RInverse
   g_1 in LInverse, g_2 in RInvers => g_1=g_2 in Inverse
*)

doc <:doc<
   @modsubsection{Squiggle reversible functions }
   @paragraph{Definition}
>>

define sq_reverse: sq_reverse{'f;'g;'A;'B} <-->  all y:Img{'f;'A;'B}. ('g('y) in 'A & 'f('g('y))~'y)


doc <:doc<
   @paragraph{Basic Rules}
   There is no special well-formedness rule. This proposition is well-formed only if it is true.
>>

let resource intro +=
   [<<"type"{ sq_reverse{'f;'g;'A;'B}}>>, wrap_intro typeEquality]

interactive sqreverse_intro {| intro[AutoMustComplete] |}:
   [wf] sequent{ <H> >- "type"{Img{'f;'A;'B}} } -->
   sequent{ <H>; y:Img{'f;'A;'B} >- 'g('y) in 'A} -->
   sequent{ <H>; y:Img{'f;'A;'B} >- 'f('g('y)) ~ 'y  } -->
   sequent{ <H> >-  sq_reverse{'f;'g;'A;'B}  }

interactive sqreverse_elim {| elim[] |} 'H 'y:
   sequent{ <H>; g:RReverse{'f;'A;'B}; <J> >- 'y in Img{'f;'A;'B} } -->
   sequent{ <H>; g:RReverse{'f;'A;'B}; 'g 'y in 'A; 'f('g('y))~'y; <J> >- 'C  } -->
   sequent{ <H>; sq_reverse{'f;'g;'A;'B}; <J> >- 'C  }

doc <:doc<
   If $B$ is simple then usual reversibility implies squiggle reversibility.
>>

interactive sqreverse_intro_simple {| intro[SelectOption 1] |}:
   sequent{ <H> >- "type"{Img{'f;'A;'B}}  } -->
   sequent{ <H> >- sqsimple{'B} } -->
   sequent{ <H> >- 'g in RReverse{'f;'A;'B}  } -->
   sequent{ <H> >- sq_reverse{'f;'g;'A;'B}  }

doc <:doc<
   @paragraph{Image Elimination Rule for Reversible Functions}
   For squiggle reversible functions we can formulate a stronger image elimination rule:
>>

interactive img_elim_reversible {| elim[] |} 'H 'g:
   sequent{ <H>; b:Img{'f;'A;'B}; <J['b]> >- sq_reverse{'f;'g;'A;'B} } -->
   sequent{ <H>; a:'A; <J['f('a)]> >-  'C['f('a)] } -->
   sequent{ <H>; b:Img{'f;'A;'B}; <J['b]> >- 'C['b] }

(*
   Note, that the above proof uses the pairwise functionality, although I belive this rule is provable
   is pointwise functionality as well.
   QUestion: Is it actually true?  -AK
*)

doc <:doc<
    Rules @hrefrule[img_elim_reversible] and @hrefrule[sqreverse_intro_simple] implies that
>>

interactive img_elim_reversible_simple {| elim[] |} 'H:
   sequent{ <H>; b:Img{'f;'A;'B}; <J['b]> >- sqsimple{'B} } -->
   sequent{ <H>; b:Img{'f;'A;'B}; <J['b]> >- RReverse{'f;'A;'B} } -->
   sequent{ <H>; a:'A; <J['f('a)]> >-  'C['f('a)] } -->
   sequent{ <H>; b:Img{'f;'A;'B}; <J['b]> >- 'C['b] }

