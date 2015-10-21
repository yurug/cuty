open Lwt
open Goal

type user_action =
  | Use of id

let action : user_action Lwt_mvar.t =
  Lwt_mvar.create_empty ()

let on_click id_string =
  Firebug.console##log (Js.string "Click fired!");
  Lwt.async (fun () ->
    Firebug.console##log (Js.string "Click done!");
    Lwt_mvar.put action (Use (Id (int_of_string id_string))) 
  )

let init () =
  CutyBackEnd.init_backend on_click

let next_user_action f =
  Lwt_mvar.take action >>= f

let push_formula color (Id id, Atom s) =
  CutyBackEnd.new_text_node id s "3em" color

let push_hypothesis = push_formula "#0000FF"
let push_conclusion = push_formula "#FF0000"

let push_goal g =
  Firebug.console##log (Js.string "Push goal");
  List.iter push_hypothesis g.hypothesis;
  push_conclusion g.conclusion;
  return ()

let pop_goal g =
  Firebug.console##log (Js.string "Pop goal");
  return ()

let interpret = function
  | PushGoal g -> push_goal g
  | PopGoal g -> pop_goal g
  | Identity -> return ()
