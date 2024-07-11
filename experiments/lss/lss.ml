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
    | _ -> Error (Format.sprintf "Invalid stage: %s" stage)

  let type_at _ _ = failwith "Not implemented"
  let hover_info _ _ = failwith "Not implemented"
end
