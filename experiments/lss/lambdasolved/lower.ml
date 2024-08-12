open Ast
module M = Monotype_lifted.Ast

let lower : Ctx.t -> M.program -> program =
 fun ctx program ->
  let program = Inst.inst ~fresh_tvar:ctx.fresh_tvar program in
  Solve.infer ctx program;
  program
