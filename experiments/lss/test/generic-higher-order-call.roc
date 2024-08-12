# cor +canonicalize -print
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print

let id = \x -> x;;
run run1 =
  let n = 1 in
  id (\x -> ~add x n) 1;;
run run2 = id (\x -> x) 2;;

> cor-out +canonicalize -print
> let id = \x ->
>   x
> let run1 =
>   let n =
>     1 in
>   (id \x1 -> ~add x1 n) 1
> let run2 =
>   (id \x2 -> x2) 2

> cor-out +monotype -print
> let id1: (Int -> Int) -> Int -> Int = \x ->
>   x
> run run1: Int =
>   let n: Int =
>     1 in
>   (id1 \x1 -> ~add x1 n) 1
> run run2: Int =
>   (id1 \x2 -> x2) 2

> cor-out +monotype_lifted -print
> let id1: (Int -> Int) -> Int -> Int = \x ->
>   x
> let clos(n: Int): Int -> Int = \x1 ->
>   ~add x1 n
> run run1: Int =
>   let n: Int =
>     1 in
>   (id1 clos) 1
> let clos1: Int -> Int = \x2 ->
>   x2
> run run2: Int =
>   (id1 clos1) 2

> cor-out +lambdasolved -print
> let id1: (Int -<'21>-> Int) -[id1]-> Int -<'21>-> Int = \x ->
>   x
> let clos(n: Int): Int -[clos (n: Int)]-> Int = \x1 ->
>   ~add x1 n
> run run1: Int =
>   let n: Int =
>     1 in
>   (id1 clos) 1
> let clos1: Int -[clos1]-> Int = \x2 ->
>   x2
> run run2: Int =
>   (id1 clos1) 2
