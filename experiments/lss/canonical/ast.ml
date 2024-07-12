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
    }

and letval =
  | Letval of { bind : typed_symbol; body : e_expr; sig_ : tvar option }

and let_def = [ `Letfn of letfn | `Letval of letval ]

and expr =
  | Var of symbol
  | Int of int
  | Str of string
  | Unit
  | Tag of string * e_expr list
  | Let of let_def * e_expr
  | Clos of { arg : tvar * symbol; body : e_expr }
  | Call of e_expr * e_expr
  | KCall of S.kernelfn * e_expr list
  | When of e_expr * branch list

and branch = e_pat * e_expr

let type_of_letfn = function Letfn { bind = ty, _; _ } -> ty
let type_of_letval = function Letval { bind = ty, _; _ } -> ty

type def =
  | Def of let_def
  | Run of { bind : typed_symbol; body : e_expr; sig_ : tvar option }

type program = def list

let name_of_def = function
  | Def (`Letfn (Letfn { bind = _, x; _ }))
  | Def (`Letval (Letval { bind = _, x; _ })) ->
      x
  | Run { bind = _, x; _ } -> x
