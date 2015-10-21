let init_backend () : unit =
  Js.Unsafe.eval_string "draw ();"

let onload _ =
  init_backend ();
  CutyBackEnd.new_text_node 3 "foo" "2em" "#FF00FF";
  CutyBackEnd.new_text_node 4 "bar" "2em" "#FF00FF";
  Js._false

let go =
  Dom_html.window##onload <- Dom_html.handler onload
