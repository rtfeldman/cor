open Language
open Syntax

let string_of_position ({ pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

type parsed_program = {
  symbols : Symbol.t;
  fresh_tvar : Syntax.Type.fresh_tvar;
  syntax : Syntax.Ast.program;
}

let parse s =
  let lexbuf = Lexer.from_string s in
  let lex = Lexer.provider lexbuf in
  let parse =
    MenhirLib.Convert.Simplified.traditional2revised Parser.toplevel
  in
  try
    let parse_ctx = Ast.fresh_parse_ctx () in
    let syntax = parse lex parse_ctx in
    Ok
      { symbols = parse_ctx.symbols; syntax; fresh_tvar = parse_ctx.fresh_tvar }
  with
  | Lexer.SyntaxError what ->
      Error
        (Printf.sprintf "Syntax error: %s at %s" what
           (string_of_position (Lexer.position lexbuf)))
  | Parser.Error ->
      Error
        (Printf.sprintf "Parse error at %s"
           (string_of_position (Lexer.position lexbuf)))

type canonicalized_program = {
  symbols : Symbol.t;
  syntax : Syntax.Ast.program;
  fresh_tvar : Canonical.Type.fresh_tvar;
  canonical : Canonical.Ast.program;
}

let canonicalize ({ symbols; syntax; fresh_tvar } : parsed_program) =
  try
    let canonical = Canonical.Lower.lower { symbols; fresh_tvar } syntax in
    Ok { symbols; fresh_tvar; syntax; canonical }
  with Canonical.Lower.Can_error e -> Error e

type solved_program = {
  symbols : Symbol.t;
  syntax : Syntax.Ast.program;
  fresh_tvar : Canonical.Type.fresh_tvar;
  canonical_solved : Canonical_solved.Ast.program;
}

let solve ({ symbols; syntax; fresh_tvar; canonical } : canonicalized_program) =
  try
    let canonical_solved =
      Canonical_solved.Lower.lower { symbols; fresh_tvar } canonical
    in
    Ok { symbols; fresh_tvar; syntax; canonical_solved }
  with Canonical_solved.Lower.Solve_err e -> Error e

type monotype_program = {
  symbols : Symbol.t;
  syntax : Syntax.Ast.program;
  monotype : Monotype.Ast.program;
}

let monotype
    ({ symbols; syntax; fresh_tvar; canonical_solved } : solved_program) =
  let ctx = Monotype.Lower.make_context ~symbols ~fresh_tvar canonical_solved in
  let monotype = Monotype.Lower.lower ctx canonical_solved in
  Ok { symbols; syntax; monotype }

type monotype_lifted_program = {
  symbols : Symbol.t;
  syntax : Syntax.Ast.program;
  monotype_lifted : Monotype_lifted.Ast.program;
}

let monotype_lifted ({ symbols; syntax; monotype } : monotype_program) =
  let ctx = Monotype_lifted.Ctx.make ~symbols monotype in
  let monotype_lifted = Monotype_lifted.Lower.lower ctx monotype in
  Ok { symbols; syntax; monotype_lifted }

let ( let* ) = Result.bind

module Lss : LANGUAGE = struct
  let name = "lss"

  let run ~stage source =
    match stage with
    | "parse" ->
        let* { syntax; symbols; _ } = parse source in
        Ok (Syntax.Ast_print.string_of_program symbols syntax)
    | "canonicalize" ->
        let* p = parse source in
        let* { canonical; _ } = canonicalize p in
        Ok (Canonical.Print.string_of_program canonical)
    | "solve" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* { canonical_solved; _ } = solve p in
        Ok (Canonical_solved.Print.string_of_program canonical_solved)
    | "monotype" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* p = solve p in
        let* { monotype; _ } = monotype p in
        Ok (Monotype.Print.string_of_program monotype)
    | "monotype_lifted" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* p = solve p in
        let* p = monotype p in
        let* { monotype_lifted; _ } = monotype_lifted p in
        Ok (Monotype_lifted.Print.string_of_program monotype_lifted)
    | _ -> Error (Format.sprintf "Invalid stage: %s" stage)

  let type_at loc s =
    let* p = parse s in
    let* p = canonicalize p in
    let* { symbols; syntax; _ } = solve p in
    let ty =
      Syntax.Query.type_at loc syntax
      |> Option.map (fun ty ->
             let names = Syntax.Type_print.name_vars [ ty ] in
             Syntax.Type_print.string_of_tvar default_width symbols names ty)
    in
    Ok ty

  let hover_info loc s =
    let* p = parse s in
    let* p = canonicalize p in
    let* { symbols; syntax; _ } = solve p in
    Ok (Syntax.Query.hover_info loc syntax symbols)
end
