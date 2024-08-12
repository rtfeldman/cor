type t = { fresh_tvar : Type.fresh_tvar }

let make () =
  let next_tvar = ref 0 in
  let fresh_tvar ty =
    let tvar = { Type.tref = ref ty; var = `Var !next_tvar } in
    incr next_tvar;
    tvar
  in
  { fresh_tvar }
