open Type

let with_parens f needs_parens inside =
  let open Format in
  if needs_parens then pp_print_string f "(";
  inside f;
  if needs_parens then pp_print_string f ")"

let int_of_parens_ctx = function `Free -> 1 | `AppHead -> 2 | `FnHead -> 3
let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2

let pp_ty : Format.formatter -> ty -> unit =
  let open Format in
  fun f ty ->
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
      | TFn (a, b) ->
          fprintf f "@[<hov 2>";
          let pty f =
            go `FnHead a;
            fprintf f "@ -> ";
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
    go `Free ty

let show_ty ty = Format.asprintf "%a" pp_ty ty
