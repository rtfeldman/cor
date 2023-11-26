open Symbol
open Util
module S = Syntax

type typed_symbol = S.tvar * symbol

type e_pat = S.tvar * pat
and pat = PTag of string * e_pat list | PVar of symbol

type e_expr = S.tvar * expr

and letfn =
  | Letfn of {
      recursive : bool;
      bind : typed_symbol;
      arg : typed_symbol;
      body : e_expr;
      rest : e_expr;
      sig_ : S.tvar option;
      captures : typed_symbol list;
    }

and letval =
  | Letval of {
      bind : typed_symbol;
      body : e_expr;
      rest : e_expr;
      sig_ : S.tvar option;
    }

and expr =
  | Var of symbol
  | Int of int
  | Str of string
  | Unit
  | Tag of string * e_expr list
  | LetFn of letfn
  | Let of letval
  | Clos of {
      arg : S.tvar * symbol;
      body : e_expr;
      captures : typed_symbol list;
    }
  | Call of e_expr * e_expr
  | KCall of S.kernelfn * e_expr list
  | When of e_expr * branch list

and branch = e_pat * e_expr

type def =
  | Def of { kind : [ `Letfn of letfn | `Letval of letval ] }
  | Run of e_expr

type program = def list

let pp_symbol = Syntax.pp_symbol
let with_parens = Syntax.with_parens

let pp_pat symbols f (p : e_pat) =
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
    | PVar x -> pp_symbol symbols f x
  in
  go `Free p

let pp_expr symbols f =
  let open Format in
  let int_of_parens_ctx = function `Free -> 1 | `Apply -> 2 in
  let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2 in

  let rec go parens (_, e) =
    match e with
    | Var x -> pp_symbol symbols f x
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
    | LetFn (Letfn { bind; arg; body; rest; captures; sig_; _ }) ->
        let ty = fst bind in
        let clos = Clos { arg; body; captures } in
        go `Free (ty, Let (Letval { bind; body = (ty, clos); rest; sig_ }))
    | Let (Letval { bind = _, x; body = rhs; rest; _ }) ->
        fprintf f "@[<v 0>@[<hv 0>";
        let expr () =
          fprintf f "@[<hv 2>let %a =@ " (pp_symbol symbols) x;
          go `Free rhs;
          fprintf f "@]@ in@]@,";
          go `Free rest
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | Clos { arg = _, x; body = e; _ } ->
        fprintf f "@[<hov 2>\\%a ->@ " (pp_symbol symbols) x;
        go `Apply e;
        fprintf f "@]"
    | Call (head, arg) ->
        fprintf f "@[<hov 2>";
        go `Apply head;
        fprintf f "@ ";
        go `Apply arg;
        fprintf f "@]"
    | KCall (head, args) ->
        fprintf f "@[<hov 2>%s@ " (List.assoc head Syntax.string_of_kernelfn);
        List.iteri
          (fun i arg ->
            if i > 0 then fprintf f "@ ";
            go `Apply arg)
          args;
        fprintf f "@]"
    | When (e, branches) ->
        fprintf f "@[<v 0>@[<hv 2>when@ ";
        go `Free e;
        fprintf f " is@]@ @[<hv 2>";
        List.iteri
          (fun i (pat, body) ->
            fprintf f "|@ %a ->@ " (pp_pat symbols) pat;
            go `Free body;
            if i < List.length branches - 1 then fprintf f "@ ")
          branches;
        fprintf f "@]@,@]"
  in
  go `Free

let pp_def : Symbol.t -> Format.formatter -> def -> unit =
 fun symbols f def ->
  let open Format in
  match def with
  | Def { kind } -> (
      match kind with
      | `Letfn (Letfn { bind; _ } as letfn) ->
          fprintf f "@[%a@]" (pp_expr symbols) (fst bind, LetFn letfn)
      | `Letval (Letval { bind; _ } as letval) ->
          fprintf f "@[%a@]" (pp_expr symbols) (fst bind, Let letval))
  | Run e -> fprintf f "@[%a@]" (pp_expr symbols) e

let pp_defs : Symbol.t -> Format.formatter -> def list -> unit =
 fun symbols f defs ->
  let open Format in
  fprintf f "@[<v 0>";
  List.iteri
    (fun i def ->
      if i > 0 then fprintf f "@,";
      pp_def symbols f def)
    defs;
  fprintf f "@]"

let string_of_program ?(width = Syntax.default_width) (symbols : Symbol.t)
    (program : program) =
  with_buffer (fun f -> pp_defs symbols f program) width
