open Symbol
open Type
open Ast
open Lower_type
module M = Lambdasolved.Ast
module T = Lambdasolved.Type

type specialization_key = { name : symbol; arg : ty; captures : ty; ret : ty }

type specialization = {
  t_fn : T.ty;
  fn : M.fn;
  t_new : T.ty;
  name_new : symbol;
  specialized : fn option ref;
}

type t = {
  symbols : Symbol.t;
  fenv : (symbol * (T.ty * M.fn)) list;
  specializations : (specialization_key * specialization) list ref;
}

let make : Symbol.t -> M.program -> t =
 fun symbols program ->
  let fenv =
    List.filter_map
      (function (t, name), `Fn fn -> Some (name, (t, fn)) | _ -> None)
      program
  in
  { symbols; fenv; specializations = ref [] }

let specialize_fn : t -> symbol -> T.ty -> symbol option =
 fun t name t_new ->
  let ( let* ) = Option.bind in
  let* t_fn, fn = List.assoc_opt name t.fenv in
  let { ty = captures; _ } = extract_closure_captures t_new name in
  let in', _lset, out' = extract_fn t_new in
  let specialization_key =
    { name; captures; arg = lower_type in'; ret = lower_type out' }
  in
  match List.assoc_opt specialization_key !(t.specializations) with
  | Some { name_new; _ } -> Some name_new
  | None ->
      let name_new =
        t.symbols.fresh_symbol_named @@ Symbol.syn_of t.symbols name
      in
      let specialization =
        { t_fn; fn; name_new; t_new; specialized = ref None }
      in
      t.specializations :=
        (specialization_key, specialization) :: !(t.specializations);
      Some name_new

let solved_specializations : t -> def list =
 fun t ->
  !(t.specializations) |> List.map snd
  |> List.filter_map (fun { specialized; name_new; _ } ->
         Option.map (fun fn -> (name_new, `Fn fn)) !specialized)

let next_specialization : t -> specialization option =
 fun t ->
  List.find_opt (fun { specialized; _ } -> !specialized = None)
  @@ List.map snd !(t.specializations)
