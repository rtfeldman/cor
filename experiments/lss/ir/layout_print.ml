open Layout

let pp_layout : Format.formatter -> layout -> unit =
 fun f l ->
  let rec go f l =
    let open Format in
    match l with
    | Str -> fprintf f "str"
    | Int -> fprintf f "int"
    | Struct [] -> fprintf f "{}"
    | Struct layouts ->
        (* format as { lay1, lay2, lay3 } *)
        (* or
           {
             lay1,
             lay2,
             lay3,
           }
           if a break is required
        *)
        fprintf f "@[<hv 0>{@[<hv 0>@ ";
        List.iteri
          (fun i lay ->
            go f lay;
            if i < List.length layouts - 1 then fprintf f ",@, ")
          layouts;
        fprintf f "@ @]%t}@]"
          (pp_print_custom_break ~fits:("", 0, "") ~breaks:(",", 0, ""))
    | Union [] -> fprintf f "[]"
    | Union variants ->
        (* format as [ lay1, lay2, lay3 ] *)
        (* or
           [
             lay1,
             lay2,
             lay3,
           ]
           if a break is required
        *)
        fprintf f "@[<hv 0>[@[<hv 2>";
        List.iteri
          (fun i lay ->
            fprintf f "@ `%d %a" i go lay;
            if i < List.length variants - 1 then fprintf f ",")
          variants;
        fprintf f "@]@ ]@]"
  in
  go f l

let show_layout = Format.asprintf "%a" pp_layout
