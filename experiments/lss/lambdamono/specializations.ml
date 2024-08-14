open Symbol
open Type
open Ast
open Lower_type
module M = Lambdasolved.Ast
module T = Lambdasolved.Type

type specialization_key = {
  name : symbol;
  arg : tvar;
  captures : tvar option;
  ret : tvar;
}
[@@deriving eq]

type specialization = {
  name : symbol;
  t_fn : T.tvar;
  fn : M.fn;
  t_new : T.tvar;
  name_new : symbol;
  specialized : fn option ref;
}

type t = {
  symbols : Symbol.t;
  fenv : (symbol * (T.tvar * M.fn)) list;
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

let specialize_fn :
    t -> mono_cache -> fresh_tvar -> symbol -> T.tvar -> symbol option =
 fun t mono_cache fresh_tvar name t_new ->
  let ( let* ) = Option.bind in
  let* t_fn, fn = List.assoc_opt name t.fenv in
  let captures =
    extract_closure_captures mono_cache fresh_tvar t_new name
    |> Option.map (fun { ty; _ } -> ty)
  in
  let in', _lset, out' = extract_fn t_new in
  let specialization_key =
    {
      name;
      captures;
      arg = lower_type mono_cache fresh_tvar in';
      ret = lower_type mono_cache fresh_tvar out';
    }
  in
  let matched =
    List.find_opt (fun (key, _) ->
        equal_specialization_key key specialization_key)
    @@ !(t.specializations)
  in

  match matched with
  | Some (_, { name_new; _ }) -> Some name_new
  | None ->
      let name_new =
        t.symbols.fresh_symbol_named @@ Symbol.syn_of t.symbols name
      in
      let specialization =
        { name; t_fn; fn; name_new; t_new; specialized = ref None }
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
