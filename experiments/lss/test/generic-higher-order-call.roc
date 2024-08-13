# cor +canonicalize -print
# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print

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

> cor-out +ir -print
> fn clos3(x2: int): int
> {
>   return x2;
> }
> 
> fn id3(x: [ `0 {} ]): [ `0 {} ]
> {
>   return x;
> }
> 
> fn clos2(x1: int, captures4: { int }): int
> {
>   let n: int = @get_struct_field<captures4, 0>;
>   let var: int = @call_kfn(add, x1, n);
>   return var;
> }
> 
> fn id2(x: [ `0 { { int } } ]): [ `0 { { int } } ]
> {
>   return x;
> }
> 
> fn run1_thunk(): int
> {
>   let n: int = 1;
>   let struct: {} = @make_struct{};
>   let var1: [ `0 {} ] = @make_union<0, struct>;
>   let discr: int = @get_union_id<var1>;
>   switch discr {
>   0 -> {
>     let var2: { int } = @make_struct{ n };
>     let struct1: { { int } } = @make_struct{ var2 };
>     let var3: [ `0 { { int } } ] = @make_union<0, struct1>;
>     @call_direct(id2, var3)
>   }
>   } in join join;
>   let discr1: int = @get_union_id<join>;
>   switch discr1 {
>   0 -> {
>     let payload: { { int } } = @get_union_struct<join>;
>     let captures1: { int } = @get_struct_field<payload, 0>;
>     let var4: int = 1;
>     @call_direct(clos2, var4, captures1)
>   }
>   } in join join1;
>   return join1;
> }
> 
> entry run1: int = @call_direct(run1_thunk);
> 
> fn run2_thunk(): int
> {
>   let struct2: {} = @make_struct{};
>   let var5: [ `0 {} ] = @make_union<0, struct2>;
>   let discr2: int = @get_union_id<var5>;
>   switch discr2 {
>   0 -> {
>     let struct3: {} = @make_struct{};
>     let var6: [ `0 {} ] = @make_union<0, struct3>;
>     @call_direct(id3, var6)
>   }
>   } in join join2;
>   let discr3: int = @get_union_id<join2>;
>   switch discr3 {
>   0 -> {
>     let var7: int = 2;
>     @call_direct(clos3, var7)
>   }
>   } in join join3;
>   return join3;
> }
> 
> entry run2: int = @call_direct(run2_thunk);
