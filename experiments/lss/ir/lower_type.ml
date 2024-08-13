open Layout
module T = Lambdamono.Type

let rec lower_type : T.ty -> layout = function
  | T.TTag tags ->
      let lower_tag (_, args) =
        let struct' = List.map lower_type args in
        Struct struct'
      in
      Union (List.map lower_tag tags)
  | T.TRecord fields ->
      let struct' = List.map snd fields |> List.map lower_type in
      Struct struct'
  | T.TPrim `Unit -> Struct []
  | T.TPrim `Str -> Str
  | T.TPrim `Int -> Int
