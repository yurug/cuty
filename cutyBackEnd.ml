let div_as_string (d : Dom_html.element Js.t) : string =
  Js.to_string d##outerHTML

let new_node id d : unit =
  Js.Unsafe.(eval_string (Printf.sprintf "new_node (%d, '%s');" id (div_as_string d)))

let new_text_node id text size borderColor =
  let s = Dom_html.(createSpan document) in
  s##innerHTML <- Js.string text;
  s##style##fontSize <- Js.string size;
  s##style##padding <- Js.string "5px";
  s##style##borderColor <- Js.string borderColor;
  s##style##borderWidth <- Js.string "5px";
  s##style##borderStyle <- Js.string "solid";
  new_node id s
