open Ir

type fenv = (Symbol.symbol * proc) list
type venv = (Symbol.symbol * layout) list

let globals_names : definition list -> Symbol.symbol list =
 fun defs ->
  List.fold_left
    (fun acc def ->
      match def with Global { name; _ } -> name :: acc | _ -> acc)
    [] defs

let make_fenv : definition list -> fenv =
 fun defs ->
  List.fold_left
    (fun acc def ->
      match def with Proc proc -> (proc.name, proc) :: acc | _ -> acc)
    [] defs

let failctx : string -> string -> 'a =
 fun ctx msg -> failwith @@ "[" ^ ctx ^ "]: " ^ msg

let ctx_join : string -> string -> string = fun ctx1 ctx2 -> ctx1 ^ ":" ^ ctx2

let check_lay_equiv : string -> layout -> layout -> unit =
 fun ctx ol1 ol2 ->
  if not @@ Ir_layout.is_lay_equiv ol1 ol2 then
    failctx ctx @@ "Layouts are not equivalent: " ^ show_layout ol1 ^ " and "
    ^ show_layout ol2

let get_union : layout -> layout list =
 fun lay ->
  match !lay with
  | Union ls -> ls
  | _ -> failwith @@ "Not a union: " ^ show_layout lay

let get_union_variant : layout -> int -> layout =
 fun lay i -> List.nth (get_union lay) i

let check_is_union : layout -> unit = fun lay -> ignore @@ get_union lay

let get_struct ctx lay =
  match !lay with
  | Struct ls -> ls
  | _ -> failctx ctx @@ "Not a struct: " ^ show_layout lay

let get_struct_field ctx lay i = List.nth (get_struct ctx lay) i

let get_boxed : layout -> layout =
 fun lay ->
  match !lay with
  | Box (l, _) -> l
  | _ -> failwith @@ "Not a box: " ^ show_layout lay

let check_is_ptr_type : layout -> unit =
 fun lay ->
  match !lay with
  | Box _ -> ()
  | _ -> failwith @@ "Not a pointer: " ^ show_layout lay

let lookup_var : string -> venv -> var -> layout =
 fun ctx venv (l_x, x) ->
  let l_x' =
    match List.assoc_opt x venv with
    | Some l_x' -> l_x'
    | None -> failctx ctx @@ "Variable not found: " ^ Symbol.norm_of x
  in
  check_lay_equiv (ctx_join ctx "venv vs local") l_x' l_x;
  l_x

let lookup_proc : string -> fenv -> Symbol.symbol -> proc =
 fun ctx venv x ->
  match List.assoc_opt x venv with
  | Some proc -> proc
  | None -> failctx ctx @@ "Proc not found: " ^ Symbol.norm_of x

type kernel_sig = {
  args : [ `Variadic of layout | `List of layout list ];
  ret : layout;
}

let kernel_sig : Syntax.kernelfn -> kernel_sig = function
  | `StrConcat -> { args = `Variadic (layout_str ()); ret = layout_str () }
  | `Itos -> { args = `List [ layout_int () ]; ret = layout_str () }
  | `Add -> { args = `Variadic (layout_int ()); ret = layout_int () }

let check_expr : string -> fenv -> venv -> layout -> expr -> unit =
 fun ctx fenv venv lay e ->
  match e with
  | Lit (`String _) -> check_lay_equiv ctx (ref Str) lay
  | Lit (`Int _) -> check_lay_equiv ctx (ref Int) lay
  | Var x ->
      let l_x = lookup_var ctx venv x in
      check_lay_equiv (ctx_join ctx "l_x vs lay") l_x lay
  | MakeUnion (i, x) ->
      let l_variant = get_union_variant lay i in
      let l_x = lookup_var ctx venv x in
      check_lay_equiv ctx l_variant l_x
  | GetUnionId x ->
      let l_x = lookup_var ctx venv x in
      check_is_union l_x
  | GetUnionStruct x ->
      let l_x = lookup_var ctx venv x in
      check_is_union l_x
  | MakeStruct xs ->
      let ls = List.map (lookup_var ctx venv) xs in
      check_lay_equiv
        (ctx_join ctx "expected struct vs outer")
        (ref @@ Struct ls) lay
  | GetStructField (x, i) ->
      let l_x = lookup_var ctx venv x in
      let l_field = get_struct_field ctx l_x i in
      check_lay_equiv ctx l_field lay
  | CallDirect (f, args) ->
      let proc = lookup_proc ctx fenv f in
      let l_args = List.map (lookup_var ctx venv) args in
      let proc_args = List.map (fun (l, _) -> l) proc.args in
      List.iter2 (check_lay_equiv ctx) proc_args l_args;
      let proc_l_ret = fst proc.ret in
      check_lay_equiv ctx proc_l_ret lay
  | CallKFn (kfn, args) ->
      let { args = kargs; ret = kret } = kernel_sig kfn in
      let l_args = List.map (lookup_var ctx venv) args in
      (match kargs with
      | `Variadic l -> List.iter (check_lay_equiv ctx l) l_args
      | `List kargs -> List.iter2 (check_lay_equiv ctx) kargs l_args);
      check_lay_equiv ctx kret lay
  | MakeBox x ->
      let l_x = lookup_var ctx venv x in
      let lay_inner = get_boxed lay in
      check_lay_equiv ctx l_x lay_inner
  | GetBoxed x ->
      let l_x = lookup_var ctx venv x in
      let l_x_inner = get_boxed l_x in
      check_lay_equiv ctx l_x_inner lay

let check_body : string -> fenv -> venv -> var -> stmt list -> unit =
 fun ctx fenv venv (l_ret, ret) stmts ->
  let rec go i venv ?(check_ret = false) = function
    | [] ->
        (if check_ret then
         match List.assoc_opt ret venv with
         | None ->
             failctx ctx @@ "Return variable not in scope: "
             ^ Symbol.norm_of ret
         | Some ret_layout ->
             check_lay_equiv (ctx_join ctx "ret") ret_layout l_ret);
        venv
    | Let ((l_x, x), e) :: rest ->
        check_expr (ctx_join ctx (Symbol.show_symbol_raw x)) fenv venv l_x e;
        go (i + 1) ((x, l_x) :: venv) rest
    | Switch { cond; branches; join = l_j, j } :: rest ->
        let cond_layout = lookup_var ctx venv cond in
        check_lay_equiv (ctx_join ctx "switch") cond_layout (ref Int);
        List.iteri
          (fun i (j, (stmts, expr)) ->
            if i <> j then
              failctx ctx @@ "Switch branch indices are not contiguous";
            let venv = go (i + 1) venv stmts ~check_ret:false in
            check_expr
              (ctx_join ctx ("switch " ^ string_of_int i ^ " expr"))
              fenv venv l_j expr)
          branches;
        go (i + 1) ((j, l_j) :: venv) rest
  in
  ignore @@ go 0 venv stmts

let check_definitions : fenv -> definition list -> unit =
 fun fenv definitions ->
  let rec go venv = function
    | [] -> ()
    | Global { name; layout; init } :: rest ->
        check_expr
          ("global " ^ Symbol.show_symbol_raw name)
          fenv venv layout init;
        go ((name, layout) :: venv) rest
    | Proc { name; args; body; ret } :: rest ->
        let body_venv' =
          List.fold_left
            (fun acc (layout, name) -> (name, layout) :: acc)
            venv args
        in
        check_body
          ("proc " ^ Symbol.show_symbol_raw name)
          fenv body_venv' ret body;
        go venv rest
  in

  go [] definitions

let check : program -> unit =
 fun { definitions; entry_points } ->
  let globals = globals_names definitions in
  let fenv = make_fenv definitions in
  List.iter
    (fun (ep, _) ->
      if not (List.mem ep globals) then
        failwith ("Entry point " ^ Symbol.show_symbol_raw ep ^ " not found"))
    entry_points;
  check_definitions fenv definitions
