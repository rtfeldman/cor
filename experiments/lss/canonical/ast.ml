open Symbol
open Type
module S = Syntax.Ast

type typed_symbol = tvar * symbol

type e_pat = tvar * pat
and pat = PTag of string * e_pat list | PVar of symbol

type e_expr = tvar * expr

and letfn =
  | Letfn of {
      recursive : symbol Option.t;
      bind : typed_symbol;
      arg : typed_symbol;
      body : e_expr;
      sig_ : tvar option;
      lam_sym : symbol;
      captures : typed_symbol list;
    }

and letval =
  | Letval of { bind : typed_symbol; body : e_expr; sig_ : tvar option }

and expr =
  | Var of symbol
  | Int of int
  | Str of string
  | Unit
  | Tag of string * e_expr list
  | LetFn of letfn * e_expr
  | Let of letval * e_expr
  | Clos of {
      arg : tvar * symbol;
      body : e_expr;
      lam_sym : symbol;
      captures : typed_symbol list;
    }
  | Call of e_expr * e_expr
  | KCall of S.kernelfn * e_expr list
  | When of e_expr * branch list

and branch = e_pat * e_expr

let type_of_letfn = function Letfn { bind = ty, _; _ } -> ty
let type_of_letval = function Letval { bind = ty, _; _ } -> ty

type let_def = { kind : [ `Letfn of letfn | `Letval of letval ] }

type def =
  | Def of let_def
  | Run of { bind : typed_symbol; body : e_expr; sig_ : tvar option }

type program = def list

let name_of_def = function
  | Def { kind = `Letfn (Letfn { bind = _, x; _ }) }
  | Def { kind = `Letval (Letval { bind = _, x; _ }) } ->
      x
  | Run { bind = _, x; _ } -> x
