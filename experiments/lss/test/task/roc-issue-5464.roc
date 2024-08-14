# cor +monotype -print
# cor +monotype_lifted -print

# https://github.com/roc-lang/roc/issues/5464
Op a : [
    StdoutLine Str ({} -> Op a),
    StdinLine (Str -> Op a),
    Done a,
]

Task ok err op : ([ Ok ok, Err err ] -> op) -> op

sig succeed : ok -> Task ok * *
let succeed = \ok -> \toNext -> toNext (Ok ok);;

sig fail : err -> Task * err *
let fail = \err-> \toNext -> toNext (Err err);;

sig await : Task ok1 err op -> (ok1 -> Task ok2 err op) -> Task ok2 err op
let await = \fromResult -> \next ->
    \continue -> fromResult (\result ->
        let inner = when result is
            | Ok v -> next v
            | Err e -> fail e
        end
        in
        inner continue)
;;


sig outLine : Str -> Task {} * (Op *)
let outLine = \s -> (\toNext -> StdoutLine s (\x -> toNext (Ok x)));;

sig inLine : Task Str * (Op *)
let inLine = \toNext -> StdinLine (\s -> toNext (Ok s));;

sig main : Task {} * (Op *)
let main =
    await (outLine "What's your first name?")
        (\x -> await (inLine)
            (\firstName -> await (outLine "What's your last name?")
                (\y -> await (inLine)
                    (\lastName -> outLine (~str_concat "Hello " firstName " " lastName "!")))))
;;

run main_handler =
#   ^^^^^^^^^^^^
    let op = main (\x -> Done x) in
#       ^^
    let handle = \op -> \i -> \t -> when op is
#       ^^^^^^
        | StdinLine f -> handle (f (~str_concat "stdin" (~itos i))) (~add i 1) (Stdin t)
        | StdoutLine s f -> handle (f {}) (~add i 1) (Stdout s t)
        | Done x -> Done x t
    end
    in
    handle op 0 EntryPoint
;;

> cor-out +monotype -print
> let fail1: []
>              -> ([Err [], Ok {}]
>                   -> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -> <rec>),
>                        StdoutLine Str ({} -> <rec>)
>                        ])
>                   -> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -> <rec>),
>                        StdoutLine Str ({} -> <rec>)
>                        ] = \err ->
>   \toNext1 -> (toNext1 (Err err))
> let inLine1: ([Err [], Ok Str]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ] = \toNext3 ->
>   StdinLine \s1 -> (toNext3 (Ok s1))
> let await2: (([Err [], Ok Str]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> (Str
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ] = \fromResult ->
>   \next ->
>     \continue ->
>       (fromResult
>          \result ->
>            (let inner: ([Err [], Ok {}]
>                          -> [
>                               Done [Err [], Ok {}],
>                               StdinLine (Str -> <rec>),
>                               StdoutLine Str ({} -> <rec>)
>                               ])
>                          -> [
>                               Done [Err [], Ok {}],
>                               StdinLine (Str -> <rec>),
>                               StdoutLine Str ({} -> <rec>)
>                               ] =
>               when result is
>                 | Ok v -> next v
>                 | Err e -> fail1 e
>               end
>            in
>            inner continue))
> let outLine1: Str
>                 -> ([Err [], Ok {}]
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ])
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ] = \s ->
>   \toNext2 -> (StdoutLine s \x -> (toNext2 (Ok x)))
> let await1: (([Err [], Ok {}]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> ({}
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ] = \fromResult ->
>   \next ->
>     \continue ->
>       (fromResult
>          \result ->
>            (let inner: ([Err [], Ok {}]
>                          -> [
>                               Done [Err [], Ok {}],
>                               StdinLine (Str -> <rec>),
>                               StdoutLine Str ({} -> <rec>)
>                               ])
>                          -> [
>                               Done [Err [], Ok {}],
>                               StdinLine (Str -> <rec>),
>                               StdoutLine Str ({} -> <rec>)
>                               ] =
>               when result is
>                 | Ok v -> next v
>                 | Err e -> fail1 e
>               end
>            in
>            inner continue))
> let main: ([Err [], Ok {}]
>             -> [
>                  Done [Err [], Ok {}],
>                  StdinLine (Str -> <rec>),
>                  StdoutLine Str ({} -> <rec>)
>                  ])
>             -> [
>                  Done [Err [], Ok {}],
>                  StdinLine (Str -> <rec>),
>                  StdoutLine Str ({} -> <rec>)
>                  ] =
>   (await1 (outLine1 "What's your first name?"))
>     \x1 ->
>       ((await2 inLine1)
>          \firstName ->
>            ((await1 (outLine1 "What's your last name?"))
>               \y ->
>                 ((await2 inLine1)
>                    \lastName ->
>                      (outLine1 ~str_concat "Hello " firstName " " lastName "!"))))
> run main_handler: [
>                     Done [Err [], Ok {}]
>                       [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                     ] =
>   let op: [
>             Done [Err [], Ok {}],
>             StdinLine (Str -> <rec>),
>             StdoutLine Str ({} -> <rec>)
>             ] =
>     main \x2 -> (Done x2)
>   in
>   let rec handle: [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ]
>                     -> Int
>                          -> [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                               -> [
>                                    Done [Err [], Ok {}]
>                                      [
>                                        EntryPoint,
>                                        Stdin <rec>,
>                                        Stdout Str <rec>
>                                        ]
>                                    ] = \op1 ->
>     \i ->
>       \t ->
>         when op1 is
>           | StdinLine f ->
>             ((handle (f ~str_concat "stdin" ~itos i)) ~add i 1) (Stdin t)
>           | StdoutLine s2 f1 -> ((handle (f1 {})) ~add i 1) (Stdout s2 t)
>           | Done x3 -> Done x3 t
>         end
>   in
>   ((handle op) 0) (EntryPoint )

> cor-out +monotype_lifted -print
> let clos(err: []): ([Err [], Ok {}]
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ])
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ] = \toNext1 ->
>   toNext1 (Err err)
> let fail1: []
>              -> ([Err [], Ok {}]
>                   -> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -> <rec>),
>                        StdoutLine Str ({} -> <rec>)
>                        ])
>                   -> [
>                        Done [Err [], Ok {}],
>                        StdinLine (Str -> <rec>),
>                        StdoutLine Str ({} -> <rec>)
>                        ] = \err ->
>   clos
> let clos1(toNext3:
>             [Err [], Ok Str]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]): Str
>                          -> [
>                               Done [Err [], Ok {}],
>                               StdinLine <rec>,
>                               StdoutLine Str ({} -> <rec>)
>                               ] = \s1 ->
>   toNext3 (Ok s1)
> let inLine1: ([Err [], Ok Str]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ] = \toNext3 ->
>   StdinLine clos1
> let clos2(continue:
>             [Err [], Ok {}]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]
>            next:
>              Str
>                -> ([Err [], Ok {}]
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ])
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ]): [Err [], Ok Str]
>                                -> [
>                                     Done [Err [], Ok {}],
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str ({} -> <rec>)
>                                     ] = \result ->
>   let inner: ([Err [], Ok {}]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ] =
>     when result is
>       | Ok v -> next v
>       | Err e -> fail1 e
>     end
>   in
>   inner continue
> let clos3(fromResult:
>             ([Err [], Ok Str]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]
>            next:
>              Str
>                -> ([Err [], Ok {}]
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ])
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ]): ([Err [], Ok {}]
>                                -> [
>                                     Done [Err [], Ok {}],
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str ({} -> <rec>)
>                                     ])
>                                -> [
>                                     Done [Err [], Ok {}],
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str ({} -> <rec>)
>                                     ] = \continue ->
>   fromResult clos2
> let clos4(fromResult:
>             ([Err [], Ok Str]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]): (Str
>                          -> ([Err [], Ok {}]
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ])
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ])
>                          -> ([Err [], Ok {}]
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ])
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ] = \next ->
>   clos3
> let await2: (([Err [], Ok Str]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> (Str
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ] = \fromResult ->
>   clos4
> let clos5(toNext2:
>             [Err [], Ok {}]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]): {}
>                          -> [
>                               Done [Err [], Ok {}],
>                               StdinLine (Str -> <rec>),
>                               StdoutLine Str <rec>
>                               ] = \x ->
>   toNext2 (Ok x)
> let clos6(s: Str): ([Err [], Ok {}]
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ])
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ] = \toNext2 ->
>   StdoutLine s clos5
> let outLine1: Str
>                 -> ([Err [], Ok {}]
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ])
>                      -> [
>                           Done [Err [], Ok {}],
>                           StdinLine (Str -> <rec>),
>                           StdoutLine Str ({} -> <rec>)
>                           ] = \s ->
>   clos6
> let clos7(continue:
>             [Err [], Ok {}]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]
>            next:
>              {}
>                -> ([Err [], Ok {}]
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ])
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ]): [Err [], Ok {}]
>                                -> [
>                                     Done [Err [], Ok {}],
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str ({} -> <rec>)
>                                     ] = \result ->
>   let inner: ([Err [], Ok {}]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ] =
>     when result is
>       | Ok v -> next v
>       | Err e -> fail1 e
>     end
>   in
>   inner continue
> let clos8(fromResult:
>             ([Err [], Ok {}]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]
>            next:
>              {}
>                -> ([Err [], Ok {}]
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ])
>                     -> [
>                          Done [Err [], Ok {}],
>                          StdinLine (Str -> <rec>),
>                          StdoutLine Str ({} -> <rec>)
>                          ]): ([Err [], Ok {}]
>                                -> [
>                                     Done [Err [], Ok {}],
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str ({} -> <rec>)
>                                     ])
>                                -> [
>                                     Done [Err [], Ok {}],
>                                     StdinLine (Str -> <rec>),
>                                     StdoutLine Str ({} -> <rec>)
>                                     ] = \continue ->
>   fromResult clos7
> let clos9(fromResult:
>             ([Err [], Ok {}]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ]): ({}
>                          -> ([Err [], Ok {}]
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ])
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ])
>                          -> ([Err [], Ok {}]
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ])
>                               -> [
>                                    Done [Err [], Ok {}],
>                                    StdinLine (Str -> <rec>),
>                                    StdoutLine Str ({} -> <rec>)
>                                    ] = \next ->
>   clos8
> let await1: (([Err [], Ok {}]
>                -> [
>                     Done [Err [], Ok {}],
>                     StdinLine (Str -> <rec>),
>                     StdoutLine Str ({} -> <rec>)
>                     ])
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ])
>               -> ({}
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                    -> ([Err [], Ok {}]
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ])
>                         -> [
>                              Done [Err [], Ok {}],
>                              StdinLine (Str -> <rec>),
>                              StdoutLine Str ({} -> <rec>)
>                              ] = \fromResult ->
>   clos9
> let clos10(firstName: Str): Str
>                               -> ([Err [], Ok {}]
>                                    -> [
>                                         Done [Err [], Ok {}],
>                                         StdinLine (Str -> <rec>),
>                                         StdoutLine Str ({} -> <rec>)
>                                         ])
>                                    -> [
>                                         Done [Err [], Ok {}],
>                                         StdinLine (Str -> <rec>),
>                                         StdoutLine Str ({} -> <rec>)
>                                         ] = \lastName ->
>   outLine1 ~str_concat "Hello " firstName " " lastName "!"
> let clos11(firstName: Str): {}
>                               -> ([Err [], Ok {}]
>                                    -> [
>                                         Done [Err [], Ok {}],
>                                         StdinLine (Str -> <rec>),
>                                         StdoutLine Str ({} -> <rec>)
>                                         ])
>                                    -> [
>                                         Done [Err [], Ok {}],
>                                         StdinLine (Str -> <rec>),
>                                         StdoutLine Str ({} -> <rec>)
>                                         ] = \y ->
>   (await2 inLine1) clos10
> let clos12: Str
>               -> ([Err [], Ok {}]
>                    -> [
>                         Done [Err [], Ok {}],
>                         StdinLine (Str -> <rec>),
>                         StdoutLine Str ({} -> <rec>)
>                         ])
>                    -> [
>                         Done [Err [], Ok {}],
>                         StdinLine (Str -> <rec>),
>                         StdoutLine Str ({} -> <rec>)
>                         ] = \firstName ->
>   (await1 (outLine1 "What's your last name?")) clos11
> let clos13: {}
>               -> ([Err [], Ok {}]
>                    -> [
>                         Done [Err [], Ok {}],
>                         StdinLine (Str -> <rec>),
>                         StdoutLine Str ({} -> <rec>)
>                         ])
>                    -> [
>                         Done [Err [], Ok {}],
>                         StdinLine (Str -> <rec>),
>                         StdoutLine Str ({} -> <rec>)
>                         ] = \x1 ->
>   (await2 inLine1) clos12
> let main: ([Err [], Ok {}]
>             -> [
>                  Done [Err [], Ok {}],
>                  StdinLine (Str -> <rec>),
>                  StdoutLine Str ({} -> <rec>)
>                  ])
>             -> [
>                  Done [Err [], Ok {}],
>                  StdinLine (Str -> <rec>),
>                  StdoutLine Str ({} -> <rec>)
>                  ] =
>   (await1 (outLine1 "What's your first name?")) clos13
> let clos14: [Err [], Ok {}]
>               -> [
>                    Done [Err [], Ok {}],
>                    StdinLine (Str -> <rec>),
>                    StdoutLine Str ({} -> <rec>)
>                    ] = \x2 ->
>   Done x2
> let clos15(handle:
>              [
>                Done [Err [], Ok {}],
>                StdinLine (Str -> <rec>),
>                StdoutLine Str ({} -> <rec>)
>                ]
>                -> Int
>                     -> [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                          -> [
>                               Done [Err [], Ok {}]
>                                 [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                               ]
>             i: Int
>             op1:
>               [
>                 Done [Err [], Ok {}],
>                 StdinLine (Str -> <rec>),
>                 StdoutLine Str ({} -> <rec>)
>                 ]): [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                       -> [
>                            Done [Err [], Ok {}]
>                              [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                            ] = \t ->
>   when op1 is
>     | StdinLine f ->
>       ((handle1 (f ~str_concat "stdin" ~itos i)) ~add i 1) (Stdin t)
>     | StdoutLine s2 f1 -> ((handle1 (f1 {})) ~add i 1) (Stdout s2 t)
>     | Done x3 -> Done x3 t
>   end
> let clos16(handle:
>              [
>                Done [Err [], Ok {}],
>                StdinLine (Str -> <rec>),
>                StdoutLine Str ({} -> <rec>)
>                ]
>                -> Int
>                     -> [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                          -> [
>                               Done [Err [], Ok {}]
>                                 [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                               ]
>             op1:
>               [
>                 Done [Err [], Ok {}],
>                 StdinLine (Str -> <rec>),
>                 StdoutLine Str ({} -> <rec>)
>                 ]): Int
>                       -> [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                            -> [
>                                 Done [Err [], Ok {}]
>                                   [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                                 ] = \i ->
>   clos15
> let handle1: [
>                Done [Err [], Ok {}],
>                StdinLine (Str -> <rec>),
>                StdoutLine Str ({} -> <rec>)
>                ]
>                -> Int
>                     -> [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                          -> [
>                               Done [Err [], Ok {}]
>                                 [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                               ] = \op1 ->
>   clos16
> run main_handler: [
>                     Done [Err [], Ok {}]
>                       [EntryPoint, Stdin <rec>, Stdout Str <rec>]
>                     ] =
>   let op: [
>             Done [Err [], Ok {}],
>             StdinLine (Str -> <rec>),
>             StdoutLine Str ({} -> <rec>)
>             ] =
>     main clos14
>   in
>   ((handle1 op) 0) (EntryPoint )