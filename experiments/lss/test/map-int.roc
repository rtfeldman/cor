# cor +canonicalize -print
# cor +monotype -print

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

> cor-out +monotype -print
> let map1: Int -> Int = \x ->
>   let f: Int -> Int = \y ->
>     ~add y 1 in
>   f x
> run main: Int =
>   map1 1