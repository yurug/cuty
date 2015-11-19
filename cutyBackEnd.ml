open Js.Unsafe

let board () =
  Dom_html.getElementById "playground"

let on_event id =
  fun event -> ()

let new_node id doc =
  let element = Tyxml_js.To_dom.of_element doc in
  let node = (element :> Dom.node Js.t) in
  ignore ((board ())##appendChild (node));
  ignore (fun_call (eval_string "backend_new_node") [|
    inject element;
    inject (Js.wrap_callback (on_event id))
  |]);
  element
