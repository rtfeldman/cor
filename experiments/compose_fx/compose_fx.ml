open Language
open Syntax

let string_of_position ({ pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

let fresh_parse_ctx () : parse_ctx =
  let n = ref Type.min_var in
  let fresh_int () =
    incr n;
    !n
  in
  let fresh_tvar : Type.fresh_tvar =
   fun ty -> { ty = ref ty; var = `Var (fresh_int ()); recur = ref false }
  in
  let symbols = Symbol.make () in
  { fresh_tvar; symbols }

type parsed_program = {
  symbols : Symbol.t;
  fresh_tvar : Type.fresh_tvar;
  syn : Syntax.program;
}

type canonicalized_program = {
  symbols : Symbol.t;
  fresh_tvar : Type.fresh_tvar;
  syn : Syntax.program;
  can : Can.program;
}

type solved_program = {
  symbols : Symbol.t;
  fresh_tvar : Type.fresh_tvar;
  syn : Syntax.program;
  can : Can.program;
}

type mono_program = { ctx : Ir_ctx.ctx; mono : Mono.specialized }
type ir_program = { symbols : Symbol.t; program : Ir.program }
type evaled_program = { symbols : Symbol.t; evaled : Eval.evaled list }

let parse s =
  let lexbuf = Lexer.from_string s in
  let lex = Lexer.provider lexbuf in
  let parse =
    MenhirLib.Convert.Simplified.traditional2revised Parser.toplevel
  in
  let parse_ctx = fresh_parse_ctx () in
  try
    let syn = parse lex parse_ctx in
    let program : parsed_program =
      { symbols = parse_ctx.symbols; syn; fresh_tvar = parse_ctx.fresh_tvar }
    in
    Ok program
  with
  | Lexer.SyntaxError what ->
      Error
        (Printf.sprintf "Syntax error: %s at %s" what
           (string_of_position (Lexer.position lexbuf)))
  | Parser.Error ->
      Error
        (Printf.sprintf "Parse error at %s"
           (string_of_position (Lexer.position lexbuf)))

let canonicalize ({ symbols; syn; fresh_tvar } : parsed_program) =
  try
    let can = Can_lower.canonicalize { symbols; fresh_tvar } syn in
    let program : canonicalized_program = { symbols; fresh_tvar; syn; can } in
    Ok program
  with Can_lower.Can_error e -> Error e

let solve ({ symbols; syn; can; fresh_tvar } : canonicalized_program) =
  try
    Solve.infer_program { symbols; fresh_tvar } can;
    Ok { symbols; fresh_tvar; syn; can }
  with Solve.Solve_err e -> Error e

let mono ({ symbols; fresh_tvar; can; _ } : solved_program) =
  let ctx = Ir_ctx.new_ctx symbols fresh_tvar in
  let specialized = Mono_lower.specialize ctx can in
  Ok { ctx; mono = specialized }

let ir ({ ctx; mono } : mono_program) =
  let compiled = Ir_lower.compile ~ctx mono in
  let compiled = Ir_sort.sort_program compiled in
  Ir_check.check compiled;
  Ok { symbols = ctx.symbols; program = compiled }

let eval ({ program; symbols } : ir_program) =
  let evaled = Eval.eval_program program in
  Ok { symbols; evaled }

let print_parsed ?(width = default_width) ({ symbols; syn; _ } : parsed_program)
    =
  string_of_program ~width symbols syn

let print_canonicalized ?(width = default_width)
    ({ symbols; syn; _ } : canonicalized_program) =
  string_of_program ~width symbols syn

let print_solved ?(width = default_width) ({ can; _ } : solved_program) =
  Can.string_of_program ~width can

let print_mono ?(width = default_width) ({ mono; _ } : mono_program) =
  Mono.string_of_specialized ~width mono

let print_ir ?(width = 80) ({ program; _ } : ir_program) =
  Ir.string_of_program ~width program

let print_evaled ?(width = default_width) ({ evaled; symbols } : evaled_program)
    =
  Eval.string_of_evaled ~width symbols evaled

let ( let* ) = Result.bind

module Compose_fx : LANGUAGE = struct
  let name = "compose_fx"

  let run ~stage source =
    match stage with
    | "parse" ->
        let* p = parse source in
        Ok (print_parsed p)
    | "canonicalize" ->
        let* p = parse source in
        let* p = canonicalize p in
        Ok (print_canonicalized p)
    | "solve" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* p = solve p in
        Ok (print_solved p)
    | "mono" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* p = solve p in
        let* p = mono p in
        Ok (print_mono p)
    | "ir" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* p = solve p in
        let* p = mono p in
        let* p = ir p in
        Ok (print_ir p)
    | "eval" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* p = solve p in
        let* p = mono p in
        let* p = ir p in
        let* p = eval p in
        Ok (print_evaled p)
    | _ -> Error (Format.sprintf "Invalid stage: %s" stage)

  let type_at loc s =
    let* p = parse s in
    let* p = canonicalize p in
    let* { symbols; syn; _ } = solve p in
    let ty =
      type_at loc syn
      |> Option.map (fun ty ->
             let names = Type.name_vars [ ty ] in
             Type.string_of_tvar default_width symbols names ty)
    in
    Ok ty

  let hover_info loc s =
    let* p = parse s in
    let* p = canonicalize p in
    let* { symbols; syn; _ } = solve p in
    Ok (hover_info loc syn symbols)
end
