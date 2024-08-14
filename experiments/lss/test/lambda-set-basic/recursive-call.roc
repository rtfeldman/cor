# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

sig after : ({} -> ([A,B] -> [A,B])) -> ([A,B] -> [A,B])
let after = \cont ->
  let f = \a ->
    let inner = cont {}
    in inner a
  in f
;;

let nestForever =
  let nester = \x ->
    let f = \r -> \t -> when t is
    | A -> B
    | B -> nester r A
    end
    in
    after f
  in
  nester
;;

run main = nestForever {} B
;;
