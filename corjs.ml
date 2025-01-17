open Libcor
open Language
open Js_of_ocaml

let wrap doit =
  Printexc.record_backtrace true;
  try doit () with
  | Failure s -> Error s
  | e ->
      Error
        (Printexc.record_backtrace false;
         "Internal error. Please report this.\n\n" ^ Printexc.to_string e ^ "\n"
         ^ Printexc.get_backtrace ())

let n num = Js.float_of_number num |> int_of_float

let js_pos (line, col) =
  object%js
    val line = Js.number_of_float (float_of_int line)
    val col = Js.number_of_float (float_of_int col)
  end

let js_range (start, fin) =
  object%js
    val start = js_pos start
    val fin = js_pos fin
  end

let js_hover_info { range; md_docs } =
  object%js
    val range = js_range range
    val info = Js.array @@ Array.map Js.string @@ Array.of_list md_docs
  end

let ok s =
  object%js
    val result = Js.(some @@ string s)
    val error = Js.null
  end

let err s =
  object%js
    val result = Js.null
    val error = Js.(some @@ string s)
  end

let ret = function Ok s -> ok s | Error s -> err s

let _ =
  Js.export "languages"
    ((Js.array @@ Array.of_list
     @@ List.map Js.string languages)
     [@jsdoc {|Target languages|}])

let _ =
  Js.export "userProgram" (fun [@jsdoc {|Gets raw user program|}] prog ->
      Js.string @@ user_ann_program @@ raw_program_of_string
      @@ Js.to_string prog)

let find_language l =
  find_language l |> Option.to_result ~none:("No language " ^ l)

let emit_of_string s =
  emit_of_string s |> Option.to_result ~none:("No emit " ^ s)

let compile prog lang phase emit =
  let ( >>= ) = Result.bind in
  let raw_program = raw_program_of_string @@ prog in

  find_language lang >>= fun lang_mod ->
  emit_of_string emit >>= fun emit ->
  let { program; _ } = preprocess raw_program in

  process_one lang_mod program (phase, emit)
  |> Result.map_error string_of_compile_err
  |> Result.map string_of_compile_output

let hover_info prog lang lineco =
  let err_hover e =
    let range = (lineco, (fst lineco, snd lineco + 1)) in
    { range; md_docs = [ e ] }
  in
  let f () =
    let ( >>= ) = Result.bind in
    let prog = raw_program_of_string prog in
    find_language lang >>= fun lang ->
    let hover =
      match hover_info lang prog lineco with
      | Ok hover -> hover
      | Error e -> err_hover (string_of_compile_err e)
    in
    Ok hover
  in
  match wrap (fun () -> f ()) with Ok hover -> hover | Error e -> err_hover e

let _ =
  Js.export "compile"
    (fun
      [@jsdoc
        {|Compiles a program under a given language to a given phase, and returns the give emit|}] 
      ~prog
      ~lang
      ~phase
      ~emit
    ->
      let prog, lang, phase, emit =
        ( Js.to_string prog,
          Js.to_string lang,
          Js.to_string phase,
          Js.to_string emit )
      in
      ret @@ wrap (fun () -> compile prog lang phase emit))

let _ =
  Js.export "hover"
    (fun [@jsdoc {|Get hover information|}] ~prog ~lang ~line ~column ->
      let prog, lang, lineco =
        (Js.to_string prog, Js.to_string lang, (n line, n column))
      in
      let hover_info = hover_info prog lang lineco in
      js_hover_info hover_info)
