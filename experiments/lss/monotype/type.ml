type ty_content =
  | TFn of ty * ty
  | TTag of ty_tag list
  | TPrim of [ `Str | `Int | `Unit ]

and ty_tag = string * ty list
and ty = ty_content ref

let equal_ty : ty -> ty -> bool =
 fun ty1 ty2 ->
  let rec go visited ty1 ty2 =
    if List.exists (fun (u1, u2) -> ty1 == u1 && ty2 == u2) visited then true
    else if ty1 == ty2 then true
    else
      let visited = (ty1, ty2) :: visited in
      match (!ty1, !ty2) with
      | TFn (ty1, ty2), TFn (ty1', ty2') ->
          go visited ty1 ty1' && go visited ty2 ty2'
      | TTag ty_tags, TTag ty_tags' ->
          List.for_all2
            (fun (tag1, tys1) (tag2, tys2) ->
              tag1 = tag2
              && List.length tys1 = List.length tys2
              && List.for_all2 (go visited) tys1 tys2)
            ty_tags ty_tags'
      | TPrim `Str, TPrim `Str -> true
      | TPrim `Int, TPrim `Int -> true
      | TPrim `Unit, TPrim `Unit -> true
      | _ -> false
  in
  go [] ty1 ty2
