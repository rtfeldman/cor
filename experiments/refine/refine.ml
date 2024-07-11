open Language
open Syntax
open Solve

let string_of_position ({ pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

let fresh_int_generator () =
  let n = ref 0 in
  fun () ->
    incr n;
    !n

type fresh_var = unit -> int

let fresh_parse_ctx () : parse_ctx = { fresh_var = fresh_int_generator () }

let parse s =
  let lexbuf = Lexer.from_string s in
  let lex = Lexer.provider lexbuf in
  let parse =
    MenhirLib.Convert.Simplified.traditional2revised Parser.toplevel
  in
  let parse_ctx = fresh_parse_ctx () in
  try
    let parsed = parse lex parse_ctx in
    Ok parsed
  with
  | Lexer.SyntaxError what ->
      Error
        (Printf.sprintf "Syntax error: %s at %s" what
           (string_of_position (Lexer.position lexbuf)))
  | Parser.Error ->
      Error
        (Printf.sprintf "Parse error at %s"
           (string_of_position (Lexer.position lexbuf)))

let solve p =
  try
    (* let _ty = infer_program p in *)
    let _ty = infer_program p in
    Ok p
  with Solve_err e -> Error e

let lower ctx p =
  let ty = xty p in
  let ir = Lower.ir_of_expr ctx p in
  Ok (ty, ir)

let eval (ty, program) =
  let var, memory = Eval.eval program in
  Ok (ty, var, memory)

let ( let* ) = Result.bind

module Refine : LANGUAGE = struct
  let name = "refine"

  let run ~stage source =
    match stage with
    | "parse" ->
        let* p = parse source in
        Ok (string_of_program p)
    | "solve" ->
        let* p = parse source in
        let* p = solve p in
        Ok (string_of_program p)
    | "ir" ->
        let* p = parse source in
        let* p = solve p in
        let ctx = Ir.new_ctx () in
        let* _, ir = lower ctx p in
        Ok (Ir.string_of_program ir)
    | "eval" ->
        let* p = parse source in
        let* p = solve p in
        let* p = lower (Ir.new_ctx ()) p in
        let* ty, var, memory = eval p in
        Ok (Eval.print_back default_width ty var memory)
    | _ -> Error (Format.sprintf "Invalid stage: %s" stage)

  let type_at loc source =
    let* p = parse source in
    let* p = solve p in
    let ty = type_at loc p in
    let res =
      ty |> Option.map (fun ty -> Syntax.string_of_ty default_width ty)
    in
    Ok res

  let hover_info loc source =
    let* p = parse source in
    let* p = solve p in
    Ok (hover_info loc p)
end
