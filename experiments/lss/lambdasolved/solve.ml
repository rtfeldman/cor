open Ast
open Type
open Symbol

type venv = (Symbol.symbol * tvar) list

let show_venv venv =
  let show (x, t) = Symbol.show_symbol_raw x ^ ": " ^ Type_print.show_ty t in
  List.map show venv |> String.concat ", "

let get_keys lset = SymbolMap.bindings lset |> List.map fst |> SymbolSet.of_list

let is_generalized : tvar -> bool =
 fun t ->
  let visited = ref [] in
  let rec is_generalized t =
    let t = unlink t in
    let var = tvar_v t in
    if List.mem var !visited then false
    else (
      visited := var :: !visited;
      match tvar_deref t with
      | Unbd -> false
      | Link _ -> failwith "found a link after unlinking"
      | ForA -> true
      | Content (LSet lset) ->
          SymbolMap.exists
            (fun _ captures ->
              SymbolMap.exists (fun _ t -> is_generalized t) captures)
            lset
      | Content (TFn (tin, tlset, tout)) ->
          is_generalized tin || is_generalized tlset || is_generalized tout
      | Content (TTag tags) ->
          List.exists
            (fun (_, captures) -> List.exists is_generalized captures)
            tags
      | Content (TPrim _) -> false)
  in
  is_generalized t

let inst : fresh_tvar -> tvar -> tvar =
 fun fresh_tvar gt ->
  if not (is_generalized gt) then gt
  else
    let tenv : (variable * tvar) list ref = ref [] in
    let rec inst gt =
      let var = tvar_v gt in
      match List.assoc_opt var !tenv with
      | Some t -> t
      | None ->
          let t = fresh_tvar Unbd in
          tenv := (var, t) :: !tenv;
          let t' =
            match tvar_deref gt with
            | Unbd -> gt
            | Link t -> inst t
            | ForA -> fresh_tvar Unbd
            | Content (LSet lset) ->
                let lset =
                  SymbolMap.map
                    (fun captures -> SymbolMap.map inst captures)
                    lset
                in
                fresh_tvar @@ Content (LSet lset)
            | Content (TFn (tin, tlset, tout)) ->
                fresh_tvar @@ Content (TFn (inst tin, inst tlset, inst tout))
            | Content (TTag tags) ->
                let tags =
                  List.map
                    (fun (tag, captures) -> (tag, List.map inst captures))
                    tags
                in
                fresh_tvar @@ Content (TTag tags)
            | Content (TPrim prim) -> fresh_tvar @@ Content (TPrim prim)
          in
          tvar_set t (Link t');
          t
    in
    inst gt

let occurs : variable -> tvar -> bool =
 fun v t ->
  let visited = ref [] in
  let rec occurs t =
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
      | Link t -> occurs t
      | Content (LSet lset) ->
          SymbolMap.exists
            (fun _ captures -> SymbolMap.exists (fun _ t -> occurs t) captures)
            lset
      | Content (TFn (tin, tlset, tout)) ->
          occurs tin || occurs tlset || occurs tout
      | Content (TTag tags) ->
          List.exists (fun (_, captures) -> List.exists occurs captures) tags
      | Content (TPrim _) -> false)
  in
  occurs t

let gen : venv -> tvar -> unit =
 fun venv t ->
  let visited = ref [] in
  let rec gen t =
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
      | Link t -> gen t
      | ForA -> ()
      | Content (LSet lset) ->
          SymbolMap.iter
            (fun _ captures -> SymbolMap.iter (fun _ t -> gen t) captures)
            lset
      | Content (TFn (tin, tlset, tout)) ->
          gen tin;
          gen tlset;
          gen tout
      | Content (TTag tags) ->
          List.iter (fun (_, captures) -> List.iter gen captures) tags
      | Content (TPrim _) -> ())
  in
  gen t

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

let unify : fresh_tvar -> tvar -> tvar -> unit =
 fun fresh_tvar t u ->
  let rec unify visited t u =
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
                    unify visited t1 t2;
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
        | Content (TFn (tin, tlset, tout)), Content (TFn (uin, ulset, uout)) ->
            unify visited tin uin;
            unify visited tlset ulset;
            unify visited tout uout;
            Content (TFn (tin, tlset, tout))
        | Content (TTag tags1), Content (TTag tags2) ->
            let ({ shared; only1; only2 } : separated_tags) =
              separate_tags tags1 tags2
            in
            let shared : ty_tag list =
              List.map
                (fun ((t1, args1), (t2, args2)) ->
                  assert (t1 = t2);
                  if List.length args1 <> List.length args2 then
                    fail ("arity mismatch for tag " ^ t1);
                  List.iter2 (unify visited) args1 args2;
                  (t1, args1))
                shared
            in
            let all_tags = sort_tags @@ shared @ only1 @ only2 in
            Content (TTag all_tags)
        | Content (TPrim prim1), Content (TPrim prim2) ->
            if prim1 <> prim2 then fail "incompatible primitives";
            Content (TPrim prim1)
        | _ -> fail "incompatible types"
      in
      let v = fresh_tvar @@ Unbd in
      tvar_set t (Link v);
      tvar_set u (Link v);
      tvar_set v t'
  in
  unify [] t u

type kernel_sig = {
  args : [ `Variadic of tvar | `List of tvar list ];
  ret : tvar;
}

let kernel_sig : kernelfn -> kernel_sig = function
  | `StrConcat -> { args = `Variadic (tvar_str ()); ret = tvar_str () }
  | `Add -> { args = `Variadic (tvar_int ()); ret = tvar_int () }
  | `Itos -> { args = `List [ tvar_int () ]; ret = tvar_str () }

let infer_pat : Ctx.t -> venv -> e_pat -> venv * tvar =
 fun ctx venv p ->
  let rec go venv (t, p) =
    let venv, t' =
      match p with
      | PTag (tag, args) ->
          let arg_venvs, arg_tys = List.split @@ List.map (go venv) args in
          let args_venv = List.concat arg_venvs in
          let tag = (tag, arg_tys) in
          let tag_ty = TTag [ tag ] in
          let t = ctx.fresh_tvar @@ Content tag_ty in
          (args_venv, t)
      | PVar x -> ([ (x, t) ], t)
    in
    unify ctx.fresh_tvar t t';
    (venv, t)
  in
  go venv p

let infer_expr : Ctx.t -> venv -> e_expr -> tvar =
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
      | Int _ -> ctx.fresh_tvar @@ Content (TPrim `Int)
      | Str _ -> ctx.fresh_tvar @@ Content (TPrim `Str)
      | Unit -> ctx.fresh_tvar @@ Content (TPrim `Unit)
      | Tag (tag, args) ->
          let arg_tys = List.map (go venv) args in
          let tag = TTag [ (tag, arg_tys) ] in
          ctx.fresh_tvar @@ Content tag
      | Let ((t_x, x), e, rest) ->
          let t_e = go venv e in
          unify ctx.fresh_tvar t_e t_x;
          go ((x, t_x) :: venv) rest
      | Call (f, a) ->
          let t_f = go venv f in
          let t_a = go venv a in
          let t_f_lset = ctx.fresh_tvar @@ Unbd in
          let t_f_wanted = ctx.fresh_tvar @@ Content (TFn (t_a, t_f_lset, t)) in
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

let infer_fn : Ctx.t -> venv -> symbol -> fn -> tvar =
 fun ctx venv lambda { arg = t_a, a; captures; body } ->
  let captures_list = SymbolMap.bindings captures in
  let venv' = ((a, t_a) :: captures_list) @ venv in
  let t_ret = infer_expr ctx venv' body in
  let t_lset =
    ctx.fresh_tvar @@ Content (LSet (SymbolMap.singleton lambda captures))
  in
  let t_fn = ctx.fresh_tvar @@ Content (TFn (t_a, t_lset, t_ret)) in
  gen venv t_fn;
  t_fn

let infer_def_val : Ctx.t -> venv -> def -> tvar =
 fun ctx venv ((_, x), def) ->
  match def with
  | `Fn fn -> infer_fn ctx venv x fn
  | `Val e -> infer_expr ctx venv e
  | `Run (e, _) -> infer_expr ctx venv e

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
