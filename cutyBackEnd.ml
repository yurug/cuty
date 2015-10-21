let div_as_string (d : Dom_html.element Js.t) : string =
  Js.to_string d##outerHTML

let get_element_dimension (e : Dom_html.element Js.t) =
  e##style##visibility <- Js.string "hidden";
  ignore (Dom_html.document##body##appendChild ((e :> Dom.node Js.t)));
  let dim = (e##offsetWidth, e##offsetHeight) in
  ignore (Dom_html.document##body##removeChild ((e :> Dom.node Js.t)));
  e##style##visibility <- Js.string "visible";
  dim

let new_node id d (width, height) : unit =
  let command =
    Printf.sprintf "new_node (%d, '%s', '%d', '%d');"
      id (div_as_string d) (10 + width) (10 + height)
  in
  Firebug.console##log (command);
  Js.Unsafe.eval_string command

let new_text_node id text size borderColor =
  let s = Dom_html.(createSpan document) in
  s##innerHTML <- Js.string text;
  s##style##fontSize <- Js.string size;
  s##style##padding <- Js.string "1px";
  s##style##margin <- Js.string "5px";
  s##style##borderColor <- Js.string borderColor;
  s##style##borderWidth <- Js.string "1px";
  s##style##borderStyle <- Js.string "solid";
  new_node id s (get_element_dimension s)

let onclick on_click_node e =
  Firebug.console##log (Js.string "click");
  Firebug.console##log (e);
  let nodes = Js.Unsafe.get e (Js.string "nodes") in
  Firebug.console##log (nodes);
  nodes##forEach (fun node _ _ -> on_click_node (Js.to_string node))

let init_backend on_click_node : unit = Js.Unsafe.(
  eval_string "draw ();";
  fun_call (eval_string "installOnClick") [|
    inject (Js.wrap_callback (onclick on_click_node))
  |]
)
