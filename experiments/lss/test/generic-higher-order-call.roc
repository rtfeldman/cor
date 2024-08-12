# cor +canonicalize -print
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print

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

> cor-out +lambdamono -print
> fn clos3(x2: Int): Int =
>   x2
> fn id3(x: [Clos1]): [Clos1] =
>   x
> fn clos2(x1: Int, captures4: {n: Int}): Int =
>   let n: Int = captures4.n in
>   ~add x1 n
> fn id2(x: [Clos {n: Int}]): [Clos {n: Int}] =
>   x
> run run1: Int =
>   let n: Int = 1 in
>   when when Id1 is
>          | Id1 -> id2(Clos {n: n})
>        end is
>     | Clos captures1 -> clos2(1, captures1)
>   end
> run run2: Int =
>   when when Id1 is
>          | Id1 -> id3(Clos1)
>        end is
>     | Clos1 -> clos3(2)
>   end