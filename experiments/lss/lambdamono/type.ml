type ty =
  | TTag of (string * ty list) list
  | TRecord of (string * ty) list
  | TPrim of [ `Str | `Int | `Unit ]
