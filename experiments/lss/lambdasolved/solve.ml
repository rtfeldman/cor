open Ast
open Type
open Symbol

type venv = (Symbol.symbol * ty) list

let show_venv venv =
  let show (x, t) = Symbol.show_symbol_raw x ^ ": " ^ Type_print.show_ty t in
  List.map show venv |> String.concat ", "

let get_keys lset = SymbolMap.bindings lset |> List.map fst |> SymbolSet.of_list

let is_generalized : ty -> bool =
 fun t ->
  let visited = ref [] in
  let rec is_generalized_tvar t =
    let var = tvar_v t in
    if List.mem var !visited then false
    else (
      visited := var :: !visited;
      match tvar_deref t with
      | Unbd -> false
      | Link t -> is_generalized_tvar t
      | ForA -> true
      | Content (LSet lset) ->
          SymbolMap.exists
            (fun _ captures ->
              SymbolMap.exists (fun _ t -> is_generalized_ty t) captures)
            lset)
  and is_generalized_ty : ty -> bool = function
    | TFn (tin, tlset, tout) ->
        is_generalized_ty tin || is_generalized_tvar tlset
        || is_generalized_ty tout
    | TTag tags ->
        List.exists
          (fun (_, captures) -> List.exists is_generalized_ty captures)
          tags
    | TPrim _ -> false
  in

  is_generalized_ty t

let inst : fresh_tvar -> ty -> ty =
 fun fresh_tvar gt ->
  if not (is_generalized gt) then gt
  else
    let tenv : (variable * tvar) list ref = ref [] in
    let rec inst_tvar gt =
      let var = tvar_v gt in
      match List.assoc_opt var !tenv with
      | Some t -> t
      | None ->
          let t = fresh_tvar Unbd in
          tenv := (var, t) :: !tenv;
          let t' =
            match tvar_deref gt with
            | Unbd -> gt
            | Link t -> inst_tvar t
            | ForA -> fresh_tvar Unbd
            | Content (LSet lset) ->
                let lset =
                  SymbolMap.map
                    (fun captures -> SymbolMap.map inst_ty captures)
                    lset
                in
                fresh_tvar @@ Content (LSet lset)
          in
          tvar_set t (Link t');
          t
    and inst_ty = function
      | TFn (tin, tlset, tout) ->
          TFn (inst_ty tin, inst_tvar tlset, inst_ty tout)
      | TTag tags ->
          TTag
            (List.map
               (fun (tag, captures) -> (tag, List.map inst_ty captures))
               tags)
      | TPrim prim -> TPrim prim
    in
    inst_ty gt

let occurs : variable -> ty -> bool =
 fun v t ->
  let visited = ref [] in
  let rec occurs_tvar t =
    let var = tvar_v t in
    if List.mem var !visited then false
    else (
      visited := var :: !visited;
      match tvar_deref t with
      | Unbd -> var = v
      | ForA ->
          (* generalized variables should never occur in the same scope in another variable *)
          assert (var <> v);
          false
      | Link t -> occurs_tvar t
      | Content (LSet lset) ->
          SymbolMap.exists
            (fun _ captures ->
              SymbolMap.exists (fun _ t -> occurs_ty t) captures)
            lset)
  and occurs_ty = function
    | TFn (tin, tlset, tout) ->
        occurs_ty tin || occurs_tvar tlset || occurs_ty tout
    | TTag tags ->
        List.exists (fun (_, captures) -> List.exists occurs_ty captures) tags
    | TPrim _ -> false
  in
  occurs_ty t

let gen : venv -> ty -> unit =
 fun venv t ->
  let visited = ref [] in
  let rec gen_tvar t =
    let var = tvar_v t in
    if List.mem var !visited then ()
    else (
      visited := var :: !visited;
      match tvar_deref t with
      | Unbd ->
          if List.exists (fun (_, t) -> occurs var t) venv then
            (* variable occurs in the current env, don't generalize *)
            ()
          else tvar_set t ForA
      | Link t -> gen_tvar t
      | ForA -> ()
      | Content (LSet lset) ->
          SymbolMap.iter
            (fun _ captures -> SymbolMap.iter (fun _ t -> gen_ty t) captures)
            lset)
  and gen_ty = function
    | TFn (tin, tlset, tout) ->
        gen_ty tin;
        gen_tvar tlset;
        gen_ty tout
    | TTag tags ->
        List.iter (fun (_, captures) -> List.iter gen_ty captures) tags
    | TPrim _ -> ()
  in
  gen_ty t

type separated_tags = {
  shared : (ty_tag * ty_tag) list;
  only1 : ty_tag list;
  only2 : ty_tag list;
}

let sort_tags : ty_tag list -> ty_tag list =
 fun tags -> List.sort (fun (tag1, _) (tag2, _) -> compare tag1 tag2) tags

let separate_tags tags1 tags2 =
  let tags1, tags2 = (sort_tags tags1, sort_tags tags2) in
  let rec walk shared only1 only2 = function
    | [], [] -> { shared; only1 = List.rev only1; only2 = List.rev only2 }
    | o :: rest, [] -> walk shared (o :: only1) only2 (rest, [])
    | [], o :: rest -> walk shared only1 (o :: only2) ([], rest)
    | t1 :: rest1, t2 :: rest2 when fst t1 < fst t2 ->
        walk shared (t1 :: only1) only2 (rest1, t2 :: rest2)
    | t1 :: rest1, t2 :: rest2 when fst t1 > fst t2 ->
        walk shared only1 (t2 :: only2) (t1 :: rest1, rest2)
    | t1 :: rest1, t2 :: rest2 ->
        walk ((t1, t2) :: shared) only1 only2 (rest1, rest2)
  in
  let result = walk [] [] [] (tags1, tags2) in
  result

let unify : fresh_tvar -> ty -> ty -> unit =
 fun fresh_tvar t u ->
  let rec unify_tvar visited t u =
    let t, u = (unlink t, unlink u) in
    let vart, varu = (tvar_v t, tvar_v u) in
    let fail s =
      failwith
        (s ^ ": " ^ Type_print.show_tvar t ^ " ~ " ^ Type_print.show_tvar u)
    in
    if vart = varu then ()
    else if List.mem (vart, varu) visited then fail "cyclic type"
    else
      let visited = (vart, varu) :: visited in
      let t' =
        match (tvar_deref t, tvar_deref u) with
        | Unbd, other | other, Unbd -> other
        | Link _, _ | _, Link _ -> fail "found a link after unlinking"
        | ForA, _ | _, ForA ->
            fail "cannot unify a generalized type; forgot to instantiate it?"
        | Content (LSet lset1), Content (LSet lset2) ->
            let union_captures (caps1 : captures) (caps2 : captures) =
              let diff_caps = SymbolMap.symmetric_diff caps1 caps2 in
              if not (SymbolMap.is_empty diff_caps) then
                fail "incompatible captures";
              let cap_symbols = get_keys caps1 in
              let new_caps =
                SymbolSet.fold
                  (fun cap new_caps ->
                    let t1 = SymbolMap.find cap caps1 in
                    let t2 = SymbolMap.find cap caps2 in
                    unify_ty visited t1 t2;
                    SymbolMap.add cap t1 new_caps)
                  cap_symbols SymbolMap.empty
              in
              new_caps
            in
            let shared_lambdas =
              SymbolSet.inter (get_keys lset1) (get_keys lset2)
            in
            let diff_lsets = SymbolMap.symmetric_diff lset1 lset2 in
            let new_lset =
              SymbolSet.fold
                (fun lam new_lset ->
                  let caps1 = SymbolMap.find lam lset1 in
                  let caps2 = SymbolMap.find lam lset2 in
                  let new_caps = union_captures caps1 caps2 in
                  SymbolMap.add lam new_caps new_lset)
                shared_lambdas diff_lsets
            in
            Content (LSet new_lset)
      in
      let v = fresh_tvar @@ Unbd in
      tvar_set t (Link v);
      tvar_set u (Link v);
      tvar_set v t'
  and unify_ty visited t u =
    let fail s =
      failwith (s ^ ": " ^ Type_print.show_ty t ^ " ~ " ^ Type_print.show_ty u)
    in
    match (t, u) with
    | TFn (tin, tlset, tout), TFn (uin, ulset, uout) ->
        unify_ty visited tin uin;
        unify_tvar visited tlset ulset;
        unify_ty visited tout uout
    | TTag tags1, TTag tags2 ->
        let ({ shared; only1; only2 } : separated_tags) =
          separate_tags tags1 tags2
        in
        let shared : ty_tag list =
          List.map
            (fun ((t1, args1), (t2, args2)) ->
              assert (t1 = t2);
              if List.length args1 <> List.length args2 then
                fail ("arity mismatch for tag " ^ t1);
              List.iter2 (unify_ty visited) args1 args2;
              (t1, args1))
            shared
        in
        let _all_tags = sort_tags @@ shared @ only1 @ only2 in
        ()
    | TPrim prim1, TPrim prim2 ->
        if prim1 <> prim2 then fail "incompatible primitives"
    | _ -> fail "incompatible types"
  in
  unify_ty [] t u

type kernel_sig = { args : [ `Variadic of ty | `List of ty list ]; ret : ty }

let kernel_sig : kernelfn -> kernel_sig = function
  | `StrConcat -> { args = `Variadic (TPrim `Str); ret = TPrim `Str }
  | `Add -> { args = `Variadic (TPrim `Int); ret = TPrim `Int }
  | `Itos -> { args = `List [ TPrim `Int ]; ret = TPrim `Str }

let infer_pat : Ctx.t -> venv -> e_pat -> venv * ty =
 fun ctx venv p ->
  let rec go venv (t, p) =
    let venv, t' =
      match p with
      | PTag (tag, args) ->
          let arg_venvs, arg_tys = List.split @@ List.map (go venv) args in
          let args_venv = List.concat arg_venvs in
          let tag = (tag, arg_tys) in
          let tag_ty = TTag [ tag ] in
          (args_venv, tag_ty)
      | PVar x -> ([ (x, t) ], t)
    in
    unify ctx.fresh_tvar t t';
    (venv, t)
  in
  go venv p

let infer_expr : Ctx.t -> venv -> e_expr -> ty =
 fun ctx venv e ->
  let rec go venv (t, e) =
    let t' =
      match e with
      | Var x -> (
          match List.assoc_opt x venv with
          | Some t -> inst ctx.fresh_tvar t
          | None ->
              failwith
                ("unbound variable " ^ Symbol.show_symbol_raw x ^ " in env "
               ^ show_venv venv))
      | Int _ -> TPrim `Int
      | Str _ -> TPrim `Str
      | Unit -> TPrim `Unit
      | Tag (tag, args) ->
          let arg_tys = List.map (go venv) args in
          TTag [ (tag, arg_tys) ]
      | Let ((t_x, x), e, rest) ->
          let t_e = go venv e in
          unify ctx.fresh_tvar t_e t_x;
          go ((x, t_x) :: venv) rest
      | Call (f, a) ->
          let t_f = go venv f in
          let t_a = go venv a in
          let t_f_lset = ctx.fresh_tvar @@ Unbd in
          let t_f_wanted = TFn (t_a, t_f_lset, t) in
          unify ctx.fresh_tvar t_f t_f_wanted;
          t
      | KCall (kernelfn, args) ->
          let ({ args = kargs; ret = kret } : kernel_sig) =
            kernel_sig kernelfn
          in
          let arg_tys = List.map (go venv) @@ args in
          (match kargs with
          | `Variadic t -> List.iter (unify ctx.fresh_tvar t) arg_tys
          | `List kargs -> List.iter2 (unify ctx.fresh_tvar) kargs arg_tys);
          kret
      | When (e, bs) ->
          let t_e = go venv e in
          let t_result = t in
          let go_branch (p, body) =
            let venv', t_p = infer_pat ctx venv p in
            unify ctx.fresh_tvar t_e t_p;
            let t_body = go (venv' @ venv) body in
            unify ctx.fresh_tvar t_result t_body
          in
          List.iter go_branch bs;
          t_result
    in
    unify ctx.fresh_tvar t t';
    t
  in
  go venv e

let infer_fn : Ctx.t -> venv -> symbol -> fn -> ty =
 fun ctx venv lambda { arg = t_a, a; captures; body } ->
  let captures_list = SymbolMap.bindings captures in
  let venv' = ((a, t_a) :: captures_list) @ venv in
  let t_ret = infer_expr ctx venv' body in
  let t_lset =
    ctx.fresh_tvar @@ Content (LSet (SymbolMap.singleton lambda captures))
  in
  let t_fn = TFn (t_a, t_lset, t_ret) in
  gen venv t_fn;
  t_fn

let infer_def_val : Ctx.t -> venv -> def -> ty =
 fun ctx venv ((_, x), def) ->
  match def with
  | `Fn fn -> infer_fn ctx venv x fn
  | `Val e -> infer_expr ctx venv e
  | `Run e -> infer_expr ctx venv e

let infer : Ctx.t -> program -> unit =
 fun ctx program ->
  let rec walk venv = function
    | [] -> ()
    | (((t, x), _) as def) :: defs ->
        let t' = infer_def_val ctx venv def in
        unify ctx.fresh_tvar t t';
        walk ((x, t') :: venv) defs
  in
  walk [] program
