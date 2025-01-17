open Language
open Syntax

let string_of_position ({ pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

let parse s =
  let lexbuf = Lexer.from_string s in
  let lex = Lexer.provider lexbuf in
  let parse =
    MenhirLib.Convert.Simplified.traditional2revised Parser.toplevel
  in
  try
    let parsed = parse lex in
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

module Roc : LANGUAGE = struct
  let name = "roc"

  let run ~stage source =
    match stage with
    | "parse" -> source |> parse |> Result.map string_of_expr
    | _ -> Error (Format.sprintf "Invalid stage: %s" stage)

  let type_at _ _ = failwith "unimplemented"
  let hover_info _ = failwith "unimplemented"
end
