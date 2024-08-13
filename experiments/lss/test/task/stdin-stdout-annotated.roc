# cor +canonicalize -print
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print

Task v op : (v -> op) -> op

sig await : Task a op -> (a -> Task b op) -> Task b op
let await = \fromResult -> \next ->
    \continue ->
        fromResult (\result ->
            let inner = next result in
            inner continue)
;;

OpIn a b : [
    StdinLine (Str -> OpIn a b),
    Done a,
]b

sig lineIn : Task Str (OpIn * *)
let lineIn = \toNext -> StdinLine (\s -> toNext s)
;;

OpOut a b : [
    StdoutLine Str (Str -> OpOut a b),
    Done a,
]b

sig lineOut : Str -> Task Str (OpOut * *)
let lineOut = \s -> (\toNext -> StdoutLine s (\x -> toNext x))
;;

Op a : [
    StdinLine (Str -> Op a),
    StdoutLine Str (Str -> Op a),
    Done a,
]

sig main : Task Str (Op *)
let main = await lineIn (\s -> lineOut s)
;;

run main_handler =
    let op = main (\x -> Done x) in
    let handle = \op -> when op is
        | StdinLine f -> handle (f "hello")
        | StdoutLine s f -> handle (f s)
        | Done x -> x
    end
    in
    handle op
;;

> cor-out +canonicalize -print
> let await = \fromResult ->
>   \next ->
>     \continue ->
>       (fromResult \result -> (let inner =
>                                 next result in
>                              inner continue))
> let lineIn = \toNext ->
>   StdinLine \s -> (toNext s)
> let lineOut = \s1 ->
>   \toNext1 -> (StdoutLine s1 \x -> (toNext1 x))
> let main =
>   (await lineIn) \s2 -> (lineOut s2)
> let main_handler =
>   let op =
>     main \x1 -> (Done x1) in
>   let handle = \op1 ->
>     when op1 is
>       | StdinLine f -> handle (f "hello")
>       | StdoutLine s3 f1 -> handle (f1 s3)
>       | Done x2 -> x2
>     end
>   in
>   handle op

> cor-out +monotype -print
> run main_handler: Str =
>   let op: [Done Str, StdinLine (Str -> <rec>), StdoutLine Str (Str -> <rec>)] =
>     main \x1 -> (Done x1)
>   in
>   let rec handle: [
>                     Done Str,
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str (Str -> <rec>)
>                     ] -> Str = \op1 ->
>     when op1 is
>       | StdinLine f -> handle (f "hello")
>       | StdoutLine s3 f1 -> handle (f1 s3)
>       | Done x2 -> x2
>     end
>   in
>   handle op
