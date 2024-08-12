# cor +canonicalize -print
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print

let f = \t ->
  when t is
    | A -> \w -> w
    | B y -> \w -> ~add w y
    | C y z -> \w -> ~add w (~add y z)
  end
;;

run main =
  let adder = f (B 10) in
  adder 12
;;

> cor-out +canonicalize -print
> let f = \t ->
>   when t is
>     | A -> \w -> w
>     | B y -> \w1 -> ~add w1 y
>     | C y1 z -> \w2 -> ~add w2 ~add y1 z
>   end
> let main =
>   let adder =
>     f (B 10) in
>   adder 12

> cor-out +monotype -print
> let f1: [A, B Int, C Int Int] -> Int -> Int = \t ->
>   when t is
>     | A -> \w -> w
>     | B y -> \w1 -> ~add w1 y
>     | C y1 z -> \w2 -> ~add w2 ~add y1 z
>   end
> run main: Int =
>   let adder: Int -> Int =
>     f1 (B 10) in
>   adder 12

> cor-out +monotype_lifted -print
> let clos: Int -> Int = \w ->
>   w
> let clos1(y: Int): Int -> Int = \w1 ->
>   ~add w1 y
> let clos2(y1: Int z: Int): Int -> Int = \w2 ->
>   ~add w2 ~add y1 z
> let f1: [A, B Int, C Int Int] -> Int -> Int = \t ->
>   when t is
>     | A -> clos
>     | B y -> clos1
>     | C y1 z -> clos2
>   end
> run main: Int =
>   let adder: Int -> Int =
>     f1 (B 10) in
>   adder 12

> cor-out +lambdasolved -print
> let clos: Int -[clos, clos1 (y: Int), clos2 (y1: Int) (z: Int)]-> Int = \w ->
>   w
> let clos1(y: Int): Int -[clos, clos1 (y: Int), clos2 (y1: Int) (z: Int)]-> Int = \w1 ->
>   ~add w1 y
> let clos2(y1: Int z: Int): Int
>                              -[clos, clos1 (y: Int), clos2 (y1: Int) (z: Int)]-> Int = \w2 ->
>   ~add w2 ~add y1 z
> let f1: [A, B Int, C Int Int]
>           -[f1]-> Int -[clos, clos1 (y: Int), clos2 (y1: Int) (z: Int)]-> Int = \t ->
>   when t is
>     | A -> clos
>     | B y -> clos1
>     | C y1 z -> clos2
>   end
> run main: Int =
>   let adder: Int -[clos, clos1 (y: Int), clos2 (y1: Int) (z: Int)]-> Int =
>     f1 (B 10)
>   in
>   adder 12

> cor-out +lambdamono -print
> fn clos5(w2: Int, captures4: {y1: Int, z: Int}): Int =
>   let z: Int = captures4.z in
>   let y1: Int = captures4.y1 in
>   ~add w2 ~add y1 z
> fn clos4(w1: Int, captures5: {y: Int}): Int =
>   let y: Int = captures5.y in
>   ~add w1 y
> fn clos3(w: Int): Int =
>   w
> fn f2(t: [A, B Int, C Int Int]): [
>                                    Clos,
>                                    Clos1 {y: Int},
>                                    Clos2 {y1: Int, z: Int}
>                                    ] =
>   when t is
>     | A -> Clos
>     | B y -> Clos1 {y: y}
>     | C y1 z -> Clos2 {y1: y1, z: z}
>   end
> run main: Int =
>   let adder: [Clos, Clos1 {y: Int}, Clos2 {y1: Int, z: Int}] =
>     when F1 is
>       | F1 -> f2(B 10)
>     end
>   in
>   when adder is
>     | Clos -> clos3(12)
>     | Clos1 captures2 -> clos4(12, captures2)
>     | Clos2 captures3 -> clos5(12, captures3)
>   end