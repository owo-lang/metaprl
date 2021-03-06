extends Kat_ax

open Top_conversionals
open Base_select
open Dtactic

interactive denest {| intro[] |}:
     [wf] sequent{ <H> >- 'y in kleene} -->
     [wf] sequent{ <H> >- 'x in kleene} -->
     sequent{ <H> >- ((star{'x}) * (star{('y * (star{'x}))})) ~ (star{('x + 'y)}) }

interactive_rw denestl_rw :
     ('y in kleene) -->
     ('x in kleene) -->
     ((star{'x}) * (star{('y * (star{'x}))})) <--> (star{('x + 'y)})

interactive_rw denestr_rw :
     ('y in kleene) -->
     ('x in kleene) -->
     (star{('x + 'y)}) <--> ((star{'x}) * (star{('y * (star{'x}))}))

interactive slide {| intro[] |}:
     [wf] sequent{ <H> >- 'y in kleene} -->
     [wf] sequent{ <H> >- 'x in kleene} -->
     sequent{ <H> >- ('x * (star{('y * 'x)})) ~ ((star{('x * 'y)}) * 'x) }

interactive_rw slidel_rw :
     ('y in kleene) -->
     ('x in kleene) -->
     ('x * (star{('y * 'x)})) <--> ((star{('x * 'y)}) * 'x)

interactive_rw slider_rw :
     ('y in kleene) -->
     ('x in kleene) -->
     ((star{('x * 'y)}) * 'x) <--> ('x * (star{('y * 'x)}))

interactive mono_star :
     [wf] sequent{ <H> >- 'y in kleene} -->
     [wf] sequent{ <H> >- 'x in kleene} -->
     sequent{ <H> >- 'x <= 'y } -->
     sequent{ <H> >- (star{'x}) <= (star{'y}) }

interactive mono_timesr :
     [wf] sequent{ <H> >- 'z in kleene} -->
     [wf] sequent{ <H> >- 'y in kleene} -->
     [wf] sequent{ <H> >- 'x in kleene} -->
     sequent{ <H> >- 'x <= 'y } -->
     sequent{ <H> >- ('x * 'z) <= ('y * 'z) }

interactive mono_timesl :
     [wf] sequent{ <H> >- 'x in kleene} -->
     [wf] sequent{ <H> >- 'z in kleene} -->
     [wf] sequent{ <H> >- 'y in kleene} -->
     sequent{ <H> >- 'y <= 'z } -->
     sequent{ <H> >- ('x * 'y) <= ('x * 'z) }

interactive mono_plusr :
     [wf] sequent{ <H> >- 'z in kleene} -->
     [wf] sequent{ <H> >- 'y in kleene} -->
     [wf] sequent{ <H> >- 'x in kleene} -->
     sequent{ <H> >- 'x <= 'y } -->
     sequent{ <H> >- ('x + 'z) <= ('y + 'z) }

interactive mono_plusl :
     [wf] sequent{ <H> >- 'x in kleene} -->
     [wf] sequent{ <H> >- 'z in kleene} -->
     [wf] sequent{ <H> >- 'y in kleene} -->
     sequent{ <H> >- 'y <= 'z } -->
     sequent{ <H> >- ('x + 'y) <= ('x + 'z) }

interactive _eq_leq :
     [wf] sequent{ <H> >- 'y in kleene} -->
     [wf] sequent{ <H> >- 'x in kleene} -->
     sequent{ <H> >- 'x ~ 'y } -->
     sequent{ <H> >- 'x <= 'y }

interactive sup :
     [wf] sequent{ <H> >- 'y in kleene} -->
     [wf] sequent{ <H> >- 'z in kleene} -->
     [wf] sequent{ <H> >- 'x in kleene} -->
     sequent{ <H> >- 'y <= 'z } -->
     sequent{ <H> >- 'x <= 'z } -->
     sequent{ <H> >- ('x + 'y) <= 'z }

interactive supr {| intro[] |}:
     [wf] sequent{ <H> >- 'x in kleene} -->
     [wf] sequent{ <H> >- 'y in kleene} -->
     sequent{ <H> >- 'y <= ('x + 'y) }

interactive supl {| intro[] |}:
     [wf] sequent{ <H> >- 'y in kleene} -->
     [wf] sequent{ <H> >- 'x in kleene} -->
     sequent{ <H> >- 'x <= ('x + 'y) }

interactive trans_leq 'y :
     [wf] sequent{ <H> >- 'z in kleene} -->
     [wf] sequent{ <H> >- 'y in kleene} -->
     [wf] sequent{ <H> >- 'x in kleene} -->
     sequent{ <H> >- 'y <= 'z } -->
     sequent{ <H> >- 'x <= 'y } -->
     sequent{ <H> >- 'x <= 'z }

interactive antisym :
     [wf] sequent{ <H> >- 'y in kleene} -->
     [wf] sequent{ <H> >- 'x in kleene} -->
     sequent{ <H> >- 'y <= 'x } -->
     sequent{ <H> >- 'x <= 'y } -->
     sequent{ <H> >- 'x ~ 'y }

interactive_rw antisyml_rw 'y :
     ('y in kleene) -->
     ('x in kleene) -->
     ('y <= 'x) -->
     ('x <= 'y) -->
     'x <--> 'y

interactive_rw antisymr_rw 'x :
     ('y in kleene) -->
     ('x in kleene) -->
     ('y <= 'x) -->
     ('x <= 'y) -->
     'y <--> 'x

interactive ref_leq {| intro[] |}:
     [wf] sequent{ <H> >- 'x in kleene} -->
     sequent{ <H> >- 'x <= 'x }

interactive cong_plusl :
     [wf] sequent{ <H> >- 'x in kleene} -->
     [wf] sequent{ <H> >- 'z in kleene} -->
     [wf] sequent{ <H> >- 'y in kleene} -->
     sequent{ <H> >- 'y ~ 'z } -->
     sequent{ <H> >- ('x + 'y) ~ ('x + 'z) }

interactive_rw cong_plusll_rw 'z :
     ('x in kleene) -->
     ('z in kleene) -->
     ('y in kleene) -->
     ('y ~ 'z) -->
     ('x + 'y) <--> ('x + 'z)

interactive_rw cong_pluslr_rw 'y :
     ('x in kleene) -->
     ('z in kleene) -->
     ('y in kleene) -->
     ('y ~ 'z) -->
     ('x + 'z) <--> ('x + 'y)

interactive id_plusl {| intro[] |}:
     [wf] sequent{ <H> >- 'x in kleene} -->
     sequent{ <H> >- (0 + 'x) ~ 'x }

interactive_rw id_plusll_rw :
     ('x in kleene) -->
     (0 + 'x) <--> 'x

interactive_rw id_pluslr_rw :
     ('x in kleene) -->
     'x <--> (0 + 'x)

