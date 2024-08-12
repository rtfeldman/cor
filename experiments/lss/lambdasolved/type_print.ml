open Type
open Util
open Symbol

let pp_symbol f symbol =
  Format.pp_print_string f (Symbol.show_symbol_raw symbol)

let int_of_parens_ctx = function `Free -> 1 | `AppHead -> 2 | `FnHead -> 3
let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2

let pp_named f var c =
  let (`Var i) = var in
  Format.fprintf f "<%c%d>" c i

let rec pp_tvar visited _ctx f t =
  let open Format in
  let rec go_lambda : variable list -> symbol -> captures -> unit =
   fun visited lambda captures ->
    fprintf f "@[<hov 2>%a" pp_symbol lambda;
    let captures = Symbol.SymbolMap.bindings captures in
    List.iter
      (fun (x, t) ->
        fprintf f "@ (%a: " pp_symbol x;
        (pp_ty visited `AppHead) f t;
        fprintf f ")")
      captures;
    fprintf f "@]"
  and go visited t =
    let t = unlink t in
    let var = tvar_v t in
    let inner f () =
      if List.mem var visited then
        (* This is a recursive type *)
        fprintf f "@[<rec>@]"
      else
        let visited = var :: visited in
        match tvar_deref t with
        | Unbd -> pp_named f var '?'
        | ForA -> pp_named f var '\''
        | Link t -> go visited t
        | Content (LSet lambdas) ->
            fprintf f "@[<hv 2>[@,";
            let lambdas = Symbol.SymbolMap.bindings lambdas in
            List.iteri
              (fun i (lambda, captures) ->
                go_lambda visited lambda captures;
                if i < List.length lambdas - 1 then fprintf f ",@ ")
              lambdas;
            fprintf f "@,]@]"
    in
    inner f ()
  in
  go visited t

and pp_ty visited ctx f ty =
  let open Format in
  let rec go_tag (tag_name, payloads) =
    fprintf f "@[<hov 2>%s" tag_name;
    List.iter
      (fun p ->
        fprintf f "@ ";
        go `AppHead p)
      payloads;
    fprintf f "@]"
  and go ctx ty =
    match ty with
    | TFn (a, tvar, b) ->
        fprintf f "@[<hov 2>";
        let pty () =
          go `FnHead a;
          fprintf f "@ -@[%a@]-> " (pp_tvar visited `Free) tvar;
          go `Free b
        in
        with_parens f (ctx >> `Free) pty;
        fprintf f "@]"
    | TTag tags ->
        fprintf f "@[<hv 2>[@,";
        List.iteri
          (fun i t ->
            go_tag t;
            if i < List.length tags - 1 then fprintf f ",@ ")
          tags;
        fprintf f "@,]@]"
    | TPrim `Str -> pp_print_string f "Str"
    | TPrim `Int -> pp_print_string f "Int"
    | TPrim `Unit -> pp_print_string f "{}"
  in
  go ctx ty

let pp_ty_top f ty = pp_ty [] `Free f ty
let show_tvar tvar = Format.asprintf "%a" (pp_tvar [] `Free) tvar
let show_ty ty = Format.asprintf "%a" (pp_ty [] `Free) ty
