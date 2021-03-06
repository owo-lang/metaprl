include Simple_print
include Lm_symbol
include Term_sig
include Term_addr_sig
include Rewrite_sig
include Opname
include Term_match_table
(* TODO[jyh]: apparently there is a bug here, where RefineError
 * get a NULL tag if Refiner.Refiner is included.  This looks
 * like a link error to me.
include Refiner.Refiner
 *)
include Refiner.Refiner.TermType
include Refiner.Refiner.Term
include Refiner.Refiner.TermAddr
include Refiner.Refiner.TermMan
include Refiner.Refiner.TermMeta
include Refiner.Refiner.TermOp
include Refiner.Refiner.TermSubst
include Refiner.Refiner.Rewrite
include Refiner.Refiner.RefineError
include Options_boot
include Mp_resource
include Top_resource
include Top_tacticals
include Top_conversionals
include Tactic_type.Tactic
include Tactic_type.Conversionals
module Sequent = Tactic_type.Sequent
include Sequent
include Tactic_type.Tacticals
include Perv
include Var
include Typeinf
include Auto_tactic
include Dtactic
include Forward
include Top_options
