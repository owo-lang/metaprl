(*
 * This is the null thread implementation.
 *)

open Refiner.Refiner.RefineError

open Thread_refiner_sig

module MakeThreadRefiner (Arg : ThreadRefinerArgSig) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type extract = Arg.extract

   type 'term t = 'term list * extract
   type 'term tactic = 'term -> 'term t

   type 'term server = unit

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Server is empty.
    *)
   let create _ =
      ()

   (*
    * Constructor.
    *)
   let create_value args ext =
      (args, ext)

   (*
    * Evaluation is trivial.
    *)
   let eval () x = x

   (*
    * Choice.
    *)
   let rec first tacs arg =
      match tacs with
         [tac] ->
            tac arg
       | tac :: tacs ->
            begin
               try tac arg with
                  RefineError _ ->
                     first tacs arg
            end
       | [] ->
            raise (RefineError ("first", StringError "no tactics"))

   (*
    * Composition forms.
    *)
   let rec apply1 tac = function
      arg :: args ->
         let args', ext = tac arg in
         let argsl, extl = apply1 tac args in
            args' @ argsl, ext :: extl
    | [] ->
         [], []

   let rec apply2 args1 args2 =
      match args1, args2 with
         arg1 :: args1, arg2 :: args2 ->
            let args, ext = arg1 arg2 in
            let argsl, extl = apply2 args1 args2 in
               args @ argsl, ext :: extl
       | [], [] ->
            [], []
       | _ ->
            raise (RefineError ("compose2", StringError "argument lists do not match"))

   let rec flatten = function
      (args, ext) :: tl ->
         let argsl, extl = flatten tl in
            args @ argsl, ext :: extl
    | [] ->
         [], []

   let compose1 tac1 tac2 arg =
      let args, ext = tac1 arg in
      let argsl, extl = apply1 tac2 args in
         argsl, Arg.compose ext extl

   let compose2 tac1 tacs2 arg =
      let args, ext = tac1 arg in
      let argsl, extl = apply2 tacs2 args in
         argsl, Arg.compose ext extl

   let composef tac1 tacf arg =
      let args, ext = tac1 arg in
      let argsl, extl = flatten (tacf args) in
         argsl, Arg.compose ext extl
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
