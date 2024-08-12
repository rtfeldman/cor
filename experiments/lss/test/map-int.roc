# cor +canonicalize -print
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print

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

> cor-out +lambdasolved -print
> let f(x: Int): Int -[f (x: Int)]-> Int = \y ->
>   ~add y x
> let map1: Int -[map1]-> Int = \x ->
>   f 1
> run main: Int =
>   map1 1

> cor-out +lambdamono -print
> fn f1(y: Int, captures2: {x: Int}): Int =
>   let x: Int = captures2.x in
>   ~add y x
> fn map2(x: Int): Int =
>   when F {x: x} is
>     | F captures1 -> f1(1, captures1)
>   end
> run main: Int =
>   when Map1 is
>     | Map1 -> map2(1)
>   end