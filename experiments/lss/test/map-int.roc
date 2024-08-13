# cor +canonicalize -print
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

let map = \x ->
  let f = \y -> ~add y x in
  f 2
;;

run main = map 1;;

> cor-out +canonicalize -print
> let map = \x ->
>   let f = \y ->
>     ~add y x in
>   f 2
> let main =
>   map 1

> cor-out +monotype -print
> let map1: Int -> Int = \x ->
>   let f: Int -> Int = \y ->
>     ~add y x in
>   f 2
> run main: Int =
>   map1 1

> cor-out +monotype_lifted -print
> let f(x: Int): Int -> Int = \y ->
>   ~add y x
> let map1: Int -> Int = \x ->
>   f 2
> run main: Int =
>   map1 1

> cor-out +lambdasolved -print
> let f(x: Int): Int -[f (x: Int)]-> Int = \y ->
>   ~add y x
> let map1: Int -[map1]-> Int = \x ->
>   f 2
> run main: Int =
>   map1 1

> cor-out +lambdamono -print
> fn f1(y: Int, captures2: {x: Int}): Int =
>   let x: Int = captures2.x in
>   ~add y x
> fn map2(x: Int): Int =
>   when F {x: x} is
>     | F captures1 -> f1(2, captures1)
>   end
> run main: Int =
>   when Map1 is
>     | Map1 -> map2(1)
>   end

> cor-out +ir -print
> fn f1(y: int, captures2: { int }): int
> {
>   let x: int = @get_struct_field<captures2, 0>;
>   let var: int = @call_kfn(add, y, x);
>   return var;
> }
> 
> fn map2(x: int): int
> {
>   let var1: { int } = @make_struct{ x };
>   let struct: { { int } } = @make_struct{ var1 };
>   let var2: [ `0 { { int } } ] = @make_union<0, struct>;
>   let discr: int = @get_union_id<var2>;
>   switch discr {
>   0 -> {
>     let payload: { { int } } = @get_union_struct<var2>;
>     let captures1: { int } = @get_struct_field<payload, 0>;
>     let var3: int = 2;
>     @call_direct(f1, var3, captures1)
>   }
>   } in join join;
>   return join;
> }
> 
> fn main_thunk(): int
> {
>   let struct1: {} = @make_struct{};
>   let var4: [ `0 {} ] = @make_union<0, struct1>;
>   let discr1: int = @get_union_id<var4>;
>   switch discr1 {
>   0 -> {
>     let var5: int = 1;
>     @call_direct(map2, var5)
>   }
>   } in join join1;
>   return join1;
> }
> 
> entry main: int = @call_direct(main_thunk);

> cor-out +eval -print
> main = 3
>      >