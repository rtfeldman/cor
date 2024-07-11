let default_width = 50

type lineco = int * int [@@deriving show]
(** line * col *)

let string_of_lineco (l, c) = string_of_int l ^ ":" ^ string_of_int c

type loc = lineco * lineco [@@deriving show]
(** start * end *)

let noloc = ((0, 0), (0, 0))
let string_of_loc (l1, l2) = string_of_lineco l1 ^ "-" ^ string_of_lineco l2
let deeper (l1, c1) (l2, c2) = l1 > l2 || (l1 = l2 && c1 >= c2)
let shallower lc1 lc2 = deeper lc2 lc1
let within (lc11, lc12) (lc21, lc22) = deeper lc11 lc21 && shallower lc12 lc22

type hover_info = {
  range : loc;
  md_docs : string list;
      (** Regions of markdown documentation.
          Each region should roughly correspond to a paragraph. *)
}

let reflow_lines prefix lines =
  String.split_on_char '\n' lines
  |> List.map (( ^ ) prefix)
  |> String.concat "\n"

module type LANGUAGE = sig
  val name : string

  val run : stage:string -> string -> (string, string) result
  (** Compile a program to a given stage. *)

  val type_at : loc -> string -> (string option, string) result
  (** Return the string type at the given location. *)

  val hover_info : lineco -> string -> (hover_info option, string) result
  (** Return hover information for the given location. *)
end
