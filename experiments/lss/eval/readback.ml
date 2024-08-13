open Memory
open Runtime

let rec pp_memory_cell f = function
  | Word i -> Format.fprintf f "%d" i
  | Block l ->
      Format.fprintf f "@[[%a]@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_memory_cell)
        l

let pp_evaled f { symbol; cell } =
  Format.fprintf f "@[%s @[<v 0>= %a@,>@]@]" (Symbol.norm_of symbol)
    pp_memory_cell cell

let pp_evaled_list f (l : evaled list) =
  Format.fprintf f "@[<v 0>%a@]" (Format.pp_print_list pp_evaled) l

let string_of_evaled ?(width = Util.default_width) (list : evaled list) =
  Util.with_buffer (fun f -> pp_evaled_list f list) width
