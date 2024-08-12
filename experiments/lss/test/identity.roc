# cor +solve -elab
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print

let id = \x -> x;;
#   ^^

run idint = id 1;;
#   ^^^^^
run idstr = id "hello";;
#   ^^^^^

> cor-out +solve -elab
> 
> let id = \x -> x;;
> #   ^^ 'a -> 'a
> 
> run idint = id 1;;
> #   ^^^^^ Int
> run idstr = id "hello";;
> #   ^^^^^ Str
> 

> cor-out +monotype -print
> let id2: Str -> Str = \x ->
>   x
> let id1: Int -> Int = \x ->
>   x
> run idint: Int =
>   id1 1
> run idstr: Str =
>   id2 "hello"

> cor-out +monotype_lifted -print
> let id2: Str -> Str = \x ->
>   x
> let id1: Int -> Int = \x ->
>   x
> run idint: Int =
>   id1 1
> run idstr: Str =
>   id2 "hello"

> cor-out +lambdasolved -print
> let id2: Str -[id2]-> Str = \x ->
>   x
> let id1: Int -[id1]-> Int = \x ->
>   x
> run idint: Int =
>   id1 1
> run idstr: Str =
>   id2 "hello"

> cor-out +lambdamono -print
> fn id4(x: Str): Str =
>   x
> fn id3(x: Int): Int =
>   x
> run idint: Int =
>   when Id1 is
>     | Id1 -> id3(1)
>   end
> run idstr: Str =
>   when Id2 is
>     | Id2 -> id4("hello")
>   end