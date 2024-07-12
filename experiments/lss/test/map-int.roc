# cor +canonicalize -print

let map = \x ->
  let f = \y -> ~add y 1 in
  f x
;;

run main = map 1;;

> cor-out +canonicalize -print
> let map = \x ->
>   let f = \y ->
>     ~add y 1 in
>   f x
> let main =
>   map 1