type bbox = {
  id : Id.t;
  x : float;
  y : float;
  w : float;
  h : float;
  dx : float;
  dy : float;
}

type t = {
  width : float;
  height : float;
  boxes : bbox list;
  friction : float
}

type dt = bbox list

let empty width height = {
  boxes = [];
  friction = 0.5;
  width;
  height;
}

let epsilon f = if f < 0.01 then 0. else f

let rec intersect a b =
  a.id <> b.id
  && (abs_float(a.x -. b.x) *. 2. <= (a.w +. b.w))
  && (abs_float(a.y -. b.y) *. 2. <= (a.h +. b.h))

let collide boxes box =
  List.exists (intersect box) boxes

let rec move boxes friction box =
  let box' =
    { box with
      x = box.x +. box.dx;
      y = box.y +. box.dy;
      dx = epsilon (box.dx *. friction);
      dy = epsilon (box.dy *. friction)
    }
  in
  if collide boxes box' then
    move boxes friction { box with dx = -. box.dx; dy = -. box.dy }
  else
    box'

let next world =
  let boxes' = List.map (move world.boxes world.friction) world.boxes in
  let dboxes = List.(
    combine world.boxes boxes'
    |> filter (fun (b1, b2) -> b1 <> b2)
    |> split
    |> snd
  ) in
  { world with boxes = boxes' }, dboxes

let new_box world id width height =
  let rec find_place () =
    let box = {
      id;
      x = Random.float world.width;
      y = Random.float world.height;
      w = width;
      h = height;
      dx = Random.float 10.;
      dy = Random.float 10.;
    }
    in
    Firebug.console##log (box);
    if collide world.boxes box then find_place () else box
  in
  let box = find_place () in
  { world with boxes = box :: world.boxes }, box
