open Language
open Syntax
open Solve

let string_of_position ({ pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

let fresh_var_generator () =
  let n = ref 0 in
  fun () ->
    incr n;
    ref (Unbd !n)

let fresh_parse_ctx () : parse_ctx = { fresh_var = fresh_var_generator () }

let parse s =
  let lexbuf = Lexer.from_string s in
  let lex = Lexer.provider lexbuf in
  let parse =
    MenhirLib.Convert.Simplified.traditional2revised Parser.toplevel
  in
  let parse_ctx = fresh_parse_ctx () in
  try
    let parsed = parse lex parse_ctx in
    Ok (parsed, parse_ctx.fresh_var)
  with
  | Lexer.SyntaxError what ->
      Error
        (Printf.sprintf "Syntax error: %s at %s" what
           (string_of_position (Lexer.position lexbuf)))
  | Parser.Error ->
      Error
        (Printf.sprintf "Parse error at %s"
           (string_of_position (Lexer.position lexbuf)))

let solve (p, fresh_var) =
  try
    (* let _ty = infer_program p in *)
    let _ty = infer_program fresh_var p in
    Ok p
  with Solve_err e -> Error e

let ( let* ) = Result.bind

module Easy_tags : LANGUAGE = struct
  let name = "easy_tags"

  let run ~stage source =
    match stage with
    | "parse" ->
        let* p, _ = parse source in
        Ok (string_of_program p)
    | "solve" ->
        let* p = parse source in
        let* p = solve p in
        Ok (string_of_program p)
    | _ -> Error (Format.sprintf "Invalid stage: %s" stage)

  let type_at loc source =
    let* p = parse source in
    let* p = solve p in
    let ty = type_at loc p in
    let res =
      ty
      |> Option.map (fun ty ->
             let names = name_vars [ ty ] in
             string_of_ty default_width names ty)
    in
    Ok res

  let hover_info loc source =
    let* p = parse source in
    let* p = solve p in
    Ok (hover_info loc p)
end
