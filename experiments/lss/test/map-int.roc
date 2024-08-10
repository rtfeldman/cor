# cor +canonicalize -print
# cor +monotype -print
# cor +monotype_lifted -print

let map = \x ->
  let f = \y -> ~add y x in
  f 1
;;

run main = map 1;;

> cor-out +canonicalize -print
> let map = \x ->
>   let f = \y ->
>     ~add y x in
>   f 1
> let main =
>   map 1

> cor-out +monotype -print
> let map1: Int -> Int = \x ->
>   let f: Int -> Int = \y ->
>     ~add y x in
>   f 1
> run main: Int =
>   map1 1

> cor-out +monotype_lifted -print
> let f(x: Int): Int -> Int = \y ->
>   ~add y x
> let map1: Int -> Int = \x ->
>   f 1
> run main: Int =
>   map1 1