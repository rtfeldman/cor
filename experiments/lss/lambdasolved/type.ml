open Symbol

type variable = [ `Var of int ] [@@deriving show]

type tvar = { tref : tref ref; var : variable }
and tref = Link of tvar | Unbd | ForA | Content of ty_content
and ty_content = LSet of lambda_set
and captures = ty SymbolMap.t
and lambda_set = captures SymbolMap.t

and ty =
  | TFn of ty * tvar * ty
  | TTag of ty_tag list
  | TPrim of [ `Str | `Int | `Unit ]

and ty_tag = string * ty list

type fresh_tvar = tref -> tvar

let rec unlink : tvar -> tvar =
 fun ({ tref; _ } as t) -> match !tref with Link t -> unlink t | _ -> t

let tvar_v : tvar -> variable = fun { var; _ } -> var
let tvar_deref : tvar -> tref = fun { tref; _ } -> !tref
let tvar_set tvar tref = tvar.tref := tref
