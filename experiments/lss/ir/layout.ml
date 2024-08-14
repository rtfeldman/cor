type layout_content =
  | Str
  | Int
  | Struct of layout list
  | Union of layout list
  | Box of layout
  | Unfilled

and layout = layout_content ref
