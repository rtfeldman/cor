open Type
module T = Canonical_solved.Type

let lower_type : T.tvar -> ty =
  let rec go_content : T.ty_content -> ty = function
    | T.TFn ((_, in'), (_, out)) ->
        let in' = go in' in
        let out = go out in
        TFn (in', out)
    | T.TTag { tags; ext = _, ext } ->
        let tags, ext = T.chase_tags tags ext in
        let tags = List.map go_tag tags in
        let tags =
          List.sort (fun (tag1, _) (tag2, _) -> compare tag1 tag2) tags
        in
        assert (go ext = TTag []);
        TTag tags
    | T.TTagEmpty -> TTag []
    | T.TPrim `Str -> TPrim `Str
    | T.TPrim `Int -> TPrim `Int
    | T.TPrim `Unit -> TPrim `Unit
  and go_tag : T.ty_tag -> ty_tag =
   fun (tag, args) ->
    let args = List.map (fun (_, t) -> go t) args in
    (tag, args)
  and go tvar =
    let ty = T.tvar_deref @@ T.unlink tvar in
    match ty with
    | T.Link _ -> failwith "unexpected link"
    | T.Unbd _ -> TTag []
    | T.ForA _ -> failwith "unexpected generalized type"
    | T.Content c -> go_content c
    | T.Alias { real; _ } -> go real
  in
  go
