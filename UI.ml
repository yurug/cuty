open Id
open Lwt
open Goal
open Goals
open Formula
open Physics

type user_action =
  | Use of Id.t

let (actions : user_action Lwt_stream.t), push =
  Lwt_stream.create ()

let next_user_action f =
  Lwt_stream.iter_s f actions

let world = ref (Physics.empty 200. 200.)

let nodes = Hashtbl.create 31
let move_box box =
  let node = Hashtbl.find nodes box.id in
  node##style##top <- Js.string (string_of_float (floor box.y) ^ "px");
  node##style##left <- Js.string (string_of_float (floor box.x) ^ "px")

let refresh () =
  let world', dworld = Physics.next !world in
  world := world';
  List.iter move_box dworld

let init () =
  ignore (Dom_html.window##setInterval (Js.wrap_callback refresh, 100.))

let get_element_dimension (e : Dom_html.element Js.t) =
  let b = e##getBoundingClientRect () in
  let width = Js.Optdef.get (b##width) (fun () -> float_of_int e##offsetWidth) in
  let height = Js.Optdef.get (b##height) (fun () -> float_of_int e##offsetHeight) in
  (width, height)

let create_node_in_world id node =
  let (width, height) = get_element_dimension node in
  Firebug.console##log_2 (width, height);
  Hashtbl.add nodes id node;
  let world', box = Physics.new_box !world id width height in
  world := world';
  move_box box

module T = Tyxml_js.Html5

let rec separated e = function
  | [] -> []
  | [x] -> [x]
  | x :: l -> x :: e :: separated e l

let wedge = T.pcdata "∧"

let rec element_of_formula = T.(function
  | Atom s ->
    div ~a:[a_class ["formula"]] [ pcdata s ]
  | Conj ts ->
    div ~a:[a_class ["formula"]] (separated wedge (List.map element_of_formula ts))
)

let element_of_formula t =
  T.(div ~a:[a_class ["formula_box"]] [element_of_formula t])

let push_formula color { identifier = Id id; term } =
  let elem = element_of_formula term in
  let node = CutyBackEnd.new_node id elem in
  create_node_in_world (Id id) node

let push_hypothesis =
  push_formula "#0000FF"

let push_conclusion =
  push_formula "#FF0000"

let push_goal g =
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
