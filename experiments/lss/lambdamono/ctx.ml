type t = {
  symbols : Symbol.t;
  fresh_tvar : Lambdasolved.Type.fresh_tvar;
  specializations : Specializations.t;
}

let make ~symbols ~fresh_tvar program =
  let specializations = Specializations.make symbols program in
  { symbols; fresh_tvar; specializations }
