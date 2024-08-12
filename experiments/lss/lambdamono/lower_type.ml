open Symbol
open Type
module T = Lambdasolved.Type
module P = Lambdasolved.Type_print

let extract_fn ty =
  match ty with
  | T.TFn (in', lset, out') -> (in', lset, out')
  | _ -> failwith @@ "expected function type, got " ^ P.show_ty ty

let lambda_tag_name s =
  match Symbol.show_symbol_raw s |> String.to_seq |> List.of_seq with
  | c :: rest -> Char.uppercase_ascii c :: rest |> List.to_seq |> String.of_seq
  | _ -> assert false

let rec lower_content : T.ty_content -> ty = function
  | T.LSet lset -> lower_lambda_set lset

and lower_lambda_set (lambdas : T.lambda_set) =
  let bindings = SymbolMap.bindings lambdas in
  let bindings =
    List.map
      (fun (name, captures) ->
        ( lambda_tag_name name,
          if SymbolMap.is_empty captures then []
          else [ lower_captures captures ] ))
      bindings
  in
  TTag bindings

and lower_captures (captures : T.captures) =
  let bindings = SymbolMap.bindings captures in
  let bindings =
    List.map
      (fun (name, ty) -> (Symbol.show_symbol_raw name, lower_type ty))
      bindings
  in
  TRecord bindings

and lower_tag (tag, args) = (tag, List.map lower_type args)

and lower_tvar tvar =
  let ty = T.tvar_deref @@ T.unlink tvar in
  match ty with
  | T.Link _ -> failwith "unexpected link"
  | T.Unbd -> TTag []
  | T.ForA -> failwith "unexpected generalized type"
  | T.Content c -> lower_content c

and lower_type : T.ty -> ty = function
  | T.TFn (_in, lset, _out) -> lower_tvar lset
  | T.TTag tags -> TTag (List.map lower_tag tags)
  | T.TPrim p -> TPrim p

type extracted_closure_captures = { captures : (symbol * ty) list; ty : ty }

let extract_lambda_set ty =
  let _in, lset, _out = extract_fn ty in
  match T.tvar_deref @@ T.unlink lset with
  | T.Content (LSet lset) -> lset
  | _ -> failwith @@ "expected function type, got " ^ P.show_ty ty

let extract_closure_captures : T.ty -> symbol -> extracted_closure_captures =
 fun ty name ->
  let lset = extract_lambda_set ty in
  let captures = SymbolMap.find name lset in
  let ty = lower_captures captures in
  let captures =
    SymbolMap.bindings captures |> List.map (fun (k, v) -> (k, lower_type v))
  in
  { captures; ty }
