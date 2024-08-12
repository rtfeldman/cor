open Symbol
open Lambdasolved.Type

type ty_cache = (variable * tvar) list ref

let show_ty_cache (cache : ty_cache) =
  let show_entry (var, tvar) =
    "(" ^ show_variable var ^ ", "
    ^ Lambdasolved.Type_print.show_tvar tvar
    ^ ")"
  in
  List.map show_entry !cache |> String.concat ", "

let clone_inst : fresh_tvar -> ty_cache -> ty -> ty =
 fun fresh_tvar cache ty ->
  let rec go_tvar : tvar -> tvar =
   fun tvar ->
    let { var; tref } = unlink tvar in
    match List.assoc_opt var !cache with
    | Some tvar -> tvar
    | None ->
        let tvar = fresh_tvar @@ Unbd in
        cache := (var, tvar) :: !cache;

        let ty =
          match !tref with
          | Link _ -> failwith "clone_type: Link"
          | Unbd -> Unbd
          | ForA -> Unbd
          | Content (LSet lambdas) ->
              let go_captures captures = SymbolMap.map go_ty captures in
              let lambdas = SymbolMap.map go_captures lambdas in
              Content (LSet lambdas)
        in
        tvar_set tvar ty;
        tvar
  and go_ty : ty -> ty =
   fun ty ->
    match ty with
    | TFn (in', lset, out') ->
        let in' = go_ty in' in
        let lset = go_tvar lset in
        let out' = go_ty out' in
        TFn (in', lset, out')
    | TTag tags ->
        let go_tag (tag, args) = (tag, List.map go_ty args) in
        let tags = List.map go_tag tags in
        TTag tags
    | TPrim p -> TPrim p
  in
  go_ty ty
