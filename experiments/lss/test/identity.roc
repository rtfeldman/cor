# cor +solve -elab
# cor +monotype -print

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

> cor-out +monotype -print
> let id2: Str -> Str = \x ->
>   x
> let id1: Int -> Int = \x ->
>   x
> run idint: Int =
>   id1 1
> run idstr: Str =
>   id2 "hello"