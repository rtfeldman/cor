open Symbol

type variable = [ `Var of int ] [@@deriving show]

type tvar = { ty : ty ref; var : variable }
and ty = Link of tvar | Unbd | ForA | Content of ty_content
and captures = tvar SymbolMap.t
and lambda_set = captures SymbolMap.t

and ty_content =
  | TFn of tvar * tvar * tvar
  | TTag of ty_tag list
  | TPrim of [ `Str | `Int | `Unit ]
  | LSet of lambda_set

and ty_tag = string * tvar list

let tvar_int () = { ty = ref (Content (TPrim `Int)); var = `Var 0 }
let tvar_str () = { ty = ref (Content (TPrim `Str)); var = `Var 1 }
let min_var = 1000

type fresh_tvar = ty -> tvar

let fresh_tvar_generator () =
  let next_tvar = ref min_var in
  fun ty ->
    let tvar = { ty = ref ty; var = `Var !next_tvar } in
    incr next_tvar;
    tvar

let rec unlink : tvar -> tvar =
 fun ({ ty; _ } as t) -> match !ty with Link t -> unlink t | _ -> t

let tvar_v : tvar -> variable = fun { var; _ } -> var
let tvar_deref : tvar -> ty = fun { ty; _ } -> !ty
let tvar_set tvar ty = tvar.ty := ty
