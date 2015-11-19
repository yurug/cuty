open Id

type term =
  | Atom of string
  | Conj of term list

type formula = {
  identifier : Id.t;
  term       : term;
}

let mk =
  let c = ref 0 in
  fun term -> incr c; { identifier = Id !c; term }

let mk_atom a = mk (Atom a)

let mk_conj ts = mk (Conj ts)
