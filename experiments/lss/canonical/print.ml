open Ast
open Util

let pp_symbol f symbol =
  Format.pp_print_string f (Symbol.show_symbol_raw symbol)

let pp_typed_symbol f (_, symbol) = Format.fprintf f "%a" pp_symbol symbol

let pp_pat f (p : e_pat) =
  let open Format in
  let int_of_parens_ctx = function `Free -> 1 | `Apply -> 2 in
  let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2 in

  let rec go parens (_, atom) =
    match atom with
    | PTag (t, atoms) ->
        fprintf f "@[<hov 2>";
        let printer () =
          fprintf f "%s" t;
          List.iteri
            (fun i p ->
              if i > 0 then fprintf f "@ ";
              go `Apply p)
            atoms
        in
        with_parens f (parens >> `Free) printer;
        fprintf f "@]"
    | PVar x -> pp_symbol f x
  in
  go `Free p

let pp_arrow f (lam, captures) =
  let open Format in
  match captures with
  | [] -> fprintf f "@[-[%a]->@]" pp_symbol lam
  | _ ->
      fprintf f "@[-[%a %a]->@]" pp_symbol lam
        (Format.pp_print_list ~pp_sep:pp_print_space pp_typed_symbol)
        captures

let pp_expr f =
  let open Format in
  let int_of_parens_ctx = function `Free -> 1 | `Apply -> 2 in
  let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2 in

  let rec go parens (_, e) =
    match e with
    | Var x -> pp_symbol f x
    | Int i -> pp_print_int f i
    | Str s -> fprintf f "\"%s\"" (String.escaped s)
    | Unit -> pp_print_string f "{}"
    | Tag (tag, payloads) ->
        fprintf f "@[<v 0>";
        let expr () =
          fprintf f "@[<hov 2>%s@ " tag;
          List.iteri
            (fun i p ->
              if i > 0 then fprintf f "@ ";
              go `Apply p)
            payloads;
          fprintf f "@]"
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | LetFn
        (Letfn { bind; arg; body; lam_sym; captures; sig_; recursive = _ }, rest)
      ->
        let ty = fst bind in
        let clos = Clos { arg; body; captures; lam_sym } in
        go `Free (ty, Let (Letval { bind; body = (ty, clos); sig_ }, rest))
    | Let (Letval { bind = _, x; body = rhs; _ }, rest) ->
        fprintf f "@[<v 0>@[<hv 0>";
        let expr () =
          fprintf f "@[<hv 2>let %a =@ " pp_symbol x;
          go `Free rhs;
          fprintf f "@]@ in@]@,";
          go `Free rest
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | Clos { arg = _, x; body = e; captures; lam_sym; _ } ->
        fprintf f "@[<hov 2>\\%a %a@ " pp_symbol x pp_arrow (lam_sym, captures);
        go `Apply e;
        fprintf f "@]"
    | Call (head, arg) ->
        fprintf f "@[";
        let expr () =
          fprintf f "@[<hov 2>";
          go `Apply head;
          fprintf f "@ ";
          go `Apply arg;
          fprintf f "@]"
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | KCall (head, args) ->
        fprintf f "@[<hov 2>%s@ " (List.assoc head S.string_of_kernelfn);
        List.iteri
          (fun i arg ->
            if i > 0 then fprintf f "@ ";
            go `Apply arg)
          args;
        fprintf f "@]"
    | When (e, branches) ->
        fprintf f "@[<v 0>@[<hv 2>when ";
        go `Free e;
        fprintf f " is";
        List.iteri
          (fun _i (pat, body) ->
            fprintf f "@ @[<hov 2>| %a ->@ " pp_pat pat;
            go `Free body;
            fprintf f "@]")
          branches;
        fprintf f "@]@,end@]"
  in
  go `Free

let pp_letfn f
    (Letfn
      { bind = _, x; arg; body; lam_sym; captures; recursive = _; sig_ = _ }) =
  let open Format in
  fprintf f "@[<v 0>@[<hv 2>let %a = \\%a %a@ %a@]@]" pp_symbol x pp_symbol
    (snd arg) pp_arrow (lam_sym, captures) pp_expr body

let pp_letval f (Letval { bind; body; _ }) =
  let open Format in
  fprintf f "@[<v 0>@[<hv 2>let %a =@ %a@]@]" pp_symbol (snd bind) pp_expr body

let pp_def : Format.formatter -> def -> unit =
 fun f def ->
  let open Format in
  match def with
  | Def { kind } -> (
      match kind with
      | `Letfn letfn -> fprintf f "@[%a@]" pp_letfn letfn
      | `Letval letval -> fprintf f "@[%a@]" pp_letval letval)
  | Run { bind; body; sig_ } ->
      fprintf f "@[%a@]" pp_letval (Letval { bind; body; sig_ })

let pp_defs : Format.formatter -> def list -> unit =
 fun f defs ->
  let open Format in
  fprintf f "@[<v 0>";
  List.iteri
    (fun i def ->
      if i > 0 then fprintf f "@,";
      pp_def f def)
    defs;
  fprintf f "@]"

let string_of_program ?(width = default_width) (program : program) =
  with_buffer (fun f -> pp_defs f program) width
