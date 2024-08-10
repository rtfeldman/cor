type t = { symbols : Symbol.t; toplevels : Symbol.symbol list }

let extract_toplevels (program : Monotype.Ast.program) =
  let open Monotype.Ast in
  let rec go acc = function
    | [] -> acc
    | `Run (Run { bind = _, x; _ }) :: rest
    | `Def (`Letfn (Letfn { bind = _, x; _ })) :: rest
    | `Def (`Letval (Letval { bind = _, x; _ })) :: rest ->
        go (x :: acc) rest
  in
  go [] program

let make ~symbols (program : Monotype.Ast.program) =
  { symbols; toplevels = extract_toplevels program }
