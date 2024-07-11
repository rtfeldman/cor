open Language
open Syntax
open Solve
open Ir
open Eval
open Util

let string_of_position ({ pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

let fresh_int_generator () =
  let n = ref 0 in
  fun () ->
    incr n;
    !n

type fresh_var = unit -> int

let fresh_parse_ctx () : parse_ctx =
  {
    fresh_var = fresh_int_generator ();
    fresh_clos_name = fresh_int_generator ();
  }

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

let solve p fresh_var =
  let fresh_ty () = TVar (ref (Unbd (fresh_var ()))) in
  try
    let spec_table = infer_program fresh_ty p in
    Ok (p, spec_table)
  with Solve_err e -> Error e

type tctx = (int * string) list

let parse s = parse s
let solve (p, fresh_var) = solve p fresh_var
let mono (p, spec_table) = Ok (p, spec_table)

type ir_program = { defs : (string * e_expr) list; entry_points : string list }

let ir (p, spec_table) =
  try
    let defs, entry_points = ir p spec_table in
    Ok { defs; entry_points }
  with Ir_error e -> Error e

type evaled_program = (string * expr list) list

let eval { defs; entry_points } =
  try Ok (eval defs entry_points) with Eval_error e -> Error e

let print_parsed ?(width = default_width) (p, _) = string_of_program ~width p
let print_solved ?(width = default_width) (p, _) = string_of_program ~width p
let print_mono = print_solved

let print_ir ?(width = default_width) { defs; entry_points } =
  string_of_program ~width
    (List.map (fun (x, e) -> Def ((noloc, x), e, List.mem x entry_points)) defs)

let print_evaled ?(width = default_width) evaled =
  let open Format in
  with_buffer
    (fun f ->
      fprintf f "@[<v 0>";
      List.iteri
        (fun i (s, es) ->
          if i > 0 then fprintf f "@,@,";
          fprintf f "@[<hov 2>%s =" s;
          List.iteri
            (fun i e ->
              fprintf f "@ ";
              if i > 0 then fprintf f "| ";
              fprintf f "@[";
              pp_expr f (noloc, TVal "?", e);
              fprintf f "@]")
            es;
          fprintf f "@]")
        evaled;
      fprintf f "@]")
    width

let ( let* ) = Result.bind

module Uls : LANGUAGE = struct
  let name = "uls"

  let run ~stage source =
    match stage with
    | "parse" ->
        let* p = parse source in
        Ok (print_parsed p)
    | "solve" ->
        let* p = parse source in
        let* p = solve p in
        Ok (print_solved p)
    | "mono" ->
        let* p = parse source in
        let* p = solve p in
        let* p = mono p in
        Ok (print_mono p)
    | "ir" ->
        let* p = parse source in
        let* p = solve p in
        let* p = mono p in
        let* p = ir p in
        Ok (print_ir p)
    | "eval" ->
        let* p = parse source in
        let* p = solve p in
        let* p = mono p in
        let* p = ir p in
        let* p = eval p in
        Ok (print_evaled p)
    | _ -> Error (Format.sprintf "Invalid stage: %s" stage)

  let type_at loc s =
    let* p = parse s in
    let* p, _ = solve p in
    type_at loc p
    |> Option.map (fun (tctx, t) -> string_of_ty default_width tctx t)
    |> Result.ok

  let hover_info lineco s =
    let* p = parse s in
    let* p, _ = solve p in
    Ok (hover_info lineco p)
end
