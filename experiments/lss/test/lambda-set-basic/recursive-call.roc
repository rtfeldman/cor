# cor +monotype -print
# cor +monotype_lifted -print
# cor +lambdasolved -print
# cor +lambdamono -print
# cor +ir -print
# cor +eval -print

sig after : ({} -> ([A,B] -> [A,B])) -> ([A,B] -> [A,B])
let after = \cont ->
  let f = \a ->
    let inner = cont {}
    in inner a
  in f
;;

let nestForever =
  let nester = \x ->
    let f = \r ->
      let g = \t -> when t is
        | A -> B
        | B -> nester r A
        end
      in g
    in
    after f
  in
  nester
;;

run main = nestForever {} B
;;

> cor-out +monotype -print
> let after1: ({} -> [A, B] -> [A, B]) -> [A, B] -> [A, B] = \cont ->
>   let f: [A, B] -> [A, B] = \a ->
>     let inner: [A, B] -> [A, B] =
>       cont {} in
>     inner a
>   in
>   f
> let nestForever: {} -> [A, B] -> [A, B] =
>   let rec nester: {} -> [A, B] -> [A, B] = \x ->
>     let f1: {} -> [A, B] -> [A, B] = \r ->
>       let g: [A, B] -> [A, B] = \t ->
>         when t is
>           | A -> B 
>           | B -> (nester r) (A )
>         end
>       in
>       g
>     in
>     after1 f1
>   in
>   nester
> run main: [A, B] =
>   (nestForever {}) (B )

> cor-out +monotype_lifted -print
> let f2(cont: {} -> [A, B] -> [A, B]): [A, B] -> [A, B] = \a ->
>   let inner: [A, B] -> [A, B] =
>     cont {} in
>   inner a
> let after1: ({} -> [A, B] -> [A, B]) -> [A, B] -> [A, B] = \cont ->
>   f2
> let g1(r: {}): [A, B] -> [A, B] = \t ->
>   when t is
>     | A -> B 
>     | B -> (nester1 r) (A )
>   end
> let f3: {} -> [A, B] -> [A, B] = \r ->
>   g1
> let nester1: {} -> [A, B] -> [A, B] = \x ->
>   after1 f3
> let nestForever: {} -> [A, B] -> [A, B] =
>   nester1
> run main: [A, B] =
>   (nestForever {}) (B )

> cor-out +lambdasolved -print
> let f2(cont: {} -<'1166>-> [A, B] -<'1184>-> [A, B]): [A, B]
>                                                         -[
>                                                            f2
>                                                              (cont: ({}
>                                                                     -<'1166>-> 
>                                                                     [A, B]
>                                                                     -<'1184>-> 
>                                                                     [
>                                                                     A,
>                                                                     B
>                                                                     ]))
>                                                            ]-> [A, B] = \a ->
>   let inner: [A, B] -<'1184>-> [A, B] =
>     cont {} in
>   inner a
> let after1: {} -<'1254>-> [A, B] -<'1256>-> [A, B]
>               -[after1]-> [A, B]
>                             -[
>                                f2
>                                  (cont: ({}
>                                           -<'1254>-> [A, B] -<'1256>-> [A, B]))
>                                ]-> [A, B] = \cont ->
>   f2
> let f3: {} -[f3]-> [A, B] -[g1 (r: {})]-> [A, B] = \r ->
>   g1
> let g1(r: {}): [A, B] -[g1 (r: {})]-> [A, B] = \t ->
>   when t is
>     | A -> B 
>     | B -> (nester1 r) (A )
>   end
> let nester1: {}
>                -[nester1]-> [A, B]
>                               -[
>                                  f2
>                                    (cont: ({}
>                                             -[f3]-> [A, B]
>                                                       -[g1 (r: {})]-> 
>                                                       [A, B]))
>                                  ]-> [A, B] = \x ->
>   after1 f3
> let nestForever: {}
>                    -[nester1]-> [A, B]
>                                   -[
>                                      f2
>                                        (cont: ({}
>                                                 -[f3]-> [A, B]
>                                                           -[g1 (r: {})]-> 
>                                                           [
>                                                             A,
>                                                             B
>                                                             ]))
>                                      ]-> [A, B] =
>   nester1
> run main: [A, B] =
>   (nestForever {}) (B )

> cor-out +lambdamono -print
> fn after2(cont: [F3]): [F2 {cont: [F3]}] =
>   F2 {cont: cont}
> fn g2(t: [A, B], captures7: {r: {}}): [A, B] =
>   let r: {} = captures7.r in
>   when t is
>     | A -> B
>     | B ->
>       when when Nester1 is
>              | Nester1 -> nester2(r)
>            end is
>         | F2 captures6 -> f4(A, captures6)
>       end
>   end
> fn f5(r: {}): [G1 {r: {}}] =
>   G1 {r: r}
> fn f4(a: [A, B], captures4: {cont: [F3]}): [A, B] =
>   let cont: [F3] = captures4.cont in
>   let inner: [G1 {r: {}}] = when cont is
>                               | F3 -> f5({})
>                             end in
>   when inner is
>     | G1 captures3 -> g2(a, captures3)
>   end
> fn nester2(x: {}): [F2 {cont: [F3]}] =
>   when After1 is
>     | After1 -> after2(F3)
>   end
> let nestForever: [Nester1] =
>   Nester1
> run main: [A, B] =
>   when when nestForever is
>          | Nester1 -> nester2({})
>        end is
>     | F2 captures1 -> f4(B, captures1)
>   end

> cor-out +ir -print
> fn after2(cont: [ `0 {} ]): [ `0 { { [ `0 {} ] } } ]
> {
>   let var: { [ `0 {} ] } = @make_struct{ cont };
>   let struct: { { [ `0 {} ] } } = @make_struct{ var };
>   let var1: [ `0 { { [ `0 {} ] } } ] = @make_union<0, struct>;
>   return var1;
> }
> 
> fn g2(t: [ `0 {}, `1 {} ], captures7: { {} }): [ `0 {}, `1 {} ]
> {
>   let r: {} = @get_struct_field<captures7, 0>;
>   let discr: int = @get_union_id<t>;
>   switch discr {
>   0 -> {
>     let struct1: {} = @make_struct{};
>     @make_union<1, struct1>
>   }
>   1 -> {
>     let struct2: {} = @make_struct{};
>     let var2: [ `0 {} ] = @make_union<0, struct2>;
>     let discr1: int = @get_union_id<var2>;
>     switch discr1 {
>     0 -> {
>       @call_direct(nester2, r)
>     }
>     } in join join;
>     let discr2: int = @get_union_id<join>;
>     switch discr2 {
>     0 -> {
>       let payload: { { [ `0 {} ] } } = @get_union_struct<join>;
>       let captures6: { [ `0 {} ] } = @get_struct_field<payload, 0>;
>       let struct3: {} = @make_struct{};
>       let var3: [ `0 {}, `1 {} ] = @make_union<0, struct3>;
>       @call_direct(f4, var3, captures6)
>     }
>     } in join join1;
>     join1
>   }
>   } in join join2;
>   return join2;
> }
> 
> fn f5(r: {}): [ `0 { { {} } } ]
> {
>   let var4: { {} } = @make_struct{ r };
>   let struct4: { { {} } } = @make_struct{ var4 };
>   let var5: [ `0 { { {} } } ] = @make_union<0, struct4>;
>   return var5;
> }
> 
> fn f4(a: [ `0 {}, `1 {} ], captures4: { [ `0 {} ] }): [ `0 {}, `1 {} ]
> {
>   let cont: [ `0 {} ] = @get_struct_field<captures4, 0>;
>   let discr3: int = @get_union_id<cont>;
>   switch discr3 {
>   0 -> {
>     let var6: {} = @make_struct{};
>     @call_direct(f5, var6)
>   }
>   } in join join3;
>   let inner: [ `0 { { {} } } ] = join3;
>   let discr4: int = @get_union_id<inner>;
>   switch discr4 {
>   0 -> {
>     let payload1: { { {} } } = @get_union_struct<inner>;
>     let captures3: { {} } = @get_struct_field<payload1, 0>;
>     @call_direct(g2, a, captures3)
>   }
>   } in join join4;
>   return join4;
> }
> 
> fn nester2(x: {}): [ `0 { { [ `0 {} ] } } ]
> {
>   let struct5: {} = @make_struct{};
>   let var7: [ `0 {} ] = @make_union<0, struct5>;
>   let discr5: int = @get_union_id<var7>;
>   switch discr5 {
>   0 -> {
>     let struct6: {} = @make_struct{};
>     let var8: [ `0 {} ] = @make_union<0, struct6>;
>     @call_direct(after2, var8)
>   }
>   } in join join5;
>   return join5;
> }
> 
> fn nestForever_thunk(): [ `0 {} ]
> {
>   let struct7: {} = @make_struct{};
>   let var9: [ `0 {} ] = @make_union<0, struct7>;
>   return var9;
> }
> 
> global nestForever: [ `0 {} ] = @call_direct(nestForever_thunk);
> 
> fn main_thunk(): [ `0 {}, `1 {} ]
> {
>   let discr6: int = @get_union_id<nestForever>;
>   switch discr6 {
>   0 -> {
>     let var10: {} = @make_struct{};
>     @call_direct(nester2, var10)
>   }
>   } in join join6;
>   let discr7: int = @get_union_id<join6>;
>   switch discr7 {
>   0 -> {
>     let payload2: { { [ `0 {} ] } } = @get_union_struct<join6>;
>     let captures1: { [ `0 {} ] } = @get_struct_field<payload2, 0>;
>     let struct8: {} = @make_struct{};
>     let var11: [ `0 {}, `1 {} ] = @make_union<1, struct8>;
>     @call_direct(f4, var11, captures1)
>   }
>   } in join join7;
>   return join7;
> }
> 
> entry main: [ `0 {}, `1 {} ] = @call_direct(main_thunk);

> cor-out +eval -print
> main = [1]
>      > B 