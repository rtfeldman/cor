type ty =
  | TFn of ty * ty
  | TTag of ty_tag list
  | TPrim of [ `Str | `Int | `Unit ]

and ty_tag = string * ty list
