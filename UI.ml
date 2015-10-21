open Lwt
open Goal

let init_backend () : unit =
  Js.Unsafe.eval_string "draw ();"

let init () =
  init_backend ()

type user_action =
  | Use of id

let action : user_action Lwt_mvar.t =
  Lwt_mvar.create_empty ()

let next_user_action f =
  Lwt_mvar.take action >>= f

let push_formula color (Id id, Atom s) =
  CutyBackEnd.new_text_node id s "3em" color

let push_hypothesis = push_formula "#0000FF"
let push_conclusion = push_formula "#FF0000"

let push_goal g =
  List.iter push_hypothesis g.hypothesis;
  push_conclusion g.conclusion;
  return ()

let pop_goal g =
  return ()

let interpret = function
  | PushGoal g -> push_goal g
  | PopGoal g -> pop_goal g
  | Identity -> return ()
