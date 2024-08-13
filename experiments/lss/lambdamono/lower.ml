open Ast
open Lower_type
open Type_clone_inst
open Ctx
open Symbol
module M = Lambdasolved.Ast
module T = Lambdasolved.Type

let specialize_expr ~ctx ~ty_cache expr =
  let rec go (t, e) =
    let t = clone_inst ctx.fresh_tvar ty_cache t in
    let e =
      match e with
      | M.Var x -> (
          match Specializations.specialize_fn ctx.specializations x t with
          | None -> Var x (* No specialization needed *)
          | Some _fn_sym ->
              (* construct the lambda set tag *)
              let { captures; ty = t_captures } =
                extract_closure_captures t x
              in
              let tag_name = lambda_tag_name x in
              let captures_expr =
                if List.length captures = 0 then None
                else
                  let build_field (x, t) =
                    (Symbol.show_symbol_raw x, (t, Var x))
                  in
                  let captures_rcd = Record (List.map build_field captures) in
                  Some (t_captures, captures_rcd)
              in
              let tag = Tag (tag_name, Option.to_list captures_expr) in
              tag)
      | M.Int i -> Int i
      | M.Str s -> Str s
      | M.Unit -> Unit
      | M.Tag (t, args) ->
          let args = List.map go args in
          Tag (t, args)
      | M.Let ((t_x, x), body, rest) ->
          let bind = (lower_type t_x, x) in
          let body = go body in
          let rest = go rest in
          Let (bind, body, rest)
      | M.Call (((t_f, _) as f), a) ->
          let f = go f in
          let a = go a in
          let compile_branch (lambda, captures) : branch =
            let captures_sym = ctx.symbols.fresh_symbol_named "captures" in
            let t_captures = lower_captures captures in
            let lambda_real =
              Specializations.specialize_fn ctx.specializations lambda t_f
              |> Option.get
            in
            if SymbolMap.cardinal captures = 0 then
              let pat = (fst f, PTag (lambda_tag_name lambda, [])) in
              let body = (fst a, Call (lambda_real, [ a ])) in
              (pat, body)
            else
              let pat =
                ( fst f,
                  PTag
                    (lambda_tag_name lambda, [ (t_captures, PVar captures_sym) ])
                )
              in
              let body =
                ( fst a,
                  Call (lambda_real, [ a; (t_captures, Var captures_sym) ]) )
              in
              (pat, body)
          in
          let lambda_set = extract_lambda_set t_f in
          let branches =
            SymbolMap.bindings lambda_set |> List.map compile_branch
          in
          When (f, branches)
      | M.KCall (kfn, args) ->
          let args = List.map go args in
          KCall (kfn, args)
      | M.When (e, branches) ->
          let e = go e in
          let branches = List.map go_branch branches in
          When (e, branches)
    in
    (lower_type t, e)
  and go_branch (p, e) =
    let p = go_pat p in
    let e = go e in
    (p, e)
  and go_pat (t, p) =
    let t = lower_type @@ clone_inst ctx.fresh_tvar ty_cache t in
    let p =
      match p with
      | M.PVar x -> PVar x
      | M.PTag (tag, args) ->
          let args = List.map go_pat args in
          PTag (tag, args)
    in
    (t, p)
  in
  go expr

let fresh_ty_cache () = ref []

let specialize_fn ~ctx ~ty_cache ~t_new ~t
    ({ arg = t_arg, arg; captures; body } : M.fn) =
  let t = clone_inst ctx.fresh_tvar ty_cache t in
  Lambdasolved.Solve.unify ctx.fresh_tvar t t_new;

  let t_arg = lower_type @@ clone_inst ctx.fresh_tvar ty_cache t_arg in
  let body = specialize_expr ~ctx ~ty_cache body in

  if SymbolMap.is_empty captures then { args = [ (t_arg, arg) ]; body }
  else
    let captures_sym = ctx.symbols.fresh_symbol_named "captures" in
    let t_captures = lower_captures captures in
    let args = [ (t_arg, arg); (t_captures, captures_sym) ] in
    let body =
      SymbolMap.fold
        (fun x t body ->
          let t = lower_type t in
          let captures_arg = (t_captures, Var captures_sym) in
          let access = (t, Access (captures_arg, Symbol.show_symbol_raw x)) in
          let bind = (t, x) in
          (fst body, Let (bind, access, body)))
        captures body
    in
    { args; body }

let specialize_val ~ctx ~ty_cache body =
  let body = specialize_expr ~ctx ~ty_cache body in
  body

let specialize_run ~ctx ~ty_cache body =
  let body = specialize_val ~ctx ~ty_cache body in
  body

let loop_specializations : Ctx.t -> unit =
 fun ctx ->
  let rec go () =
    match Specializations.next_specialization ctx.specializations with
    | None -> ()
    | Some { t_fn; fn; t_new; specialized; name_new = _ } ->
        let fn =
          specialize_fn ~ctx ~ty_cache:(fresh_ty_cache ()) ~t_new ~t:t_fn fn
        in
        specialized := Some fn;
        go ()
  in
  go ()

let init_specializations : Ctx.t -> M.program -> def list =
 fun ctx program ->
  let rec go (acc : def list) = function
    | [] -> List.rev acc
    | ((_, x), def) :: defs ->
        let acc =
          match def with
          | `Run (run, t) ->
              let run = specialize_run ~ctx ~ty_cache:(fresh_ty_cache ()) run in
              (x, `Run (run, t)) :: acc
          | `Val val_ ->
              let val_ =
                specialize_val ~ctx ~ty_cache:(fresh_ty_cache ()) val_
              in
              (x, `Val val_) :: acc
          | `Fn _ ->
              (* these will get specialized when called *)
              acc
        in
        go acc defs
  in
  go [] program

let lower : Ctx.t -> M.program -> program =
 fun ctx program ->
  let val_run_defs = init_specializations ctx program in
  loop_specializations ctx;
  let fn_defs = Specializations.solved_specializations ctx.specializations in
  fn_defs @ val_run_defs
