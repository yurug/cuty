open Formula
open Goals
open Goal

exception TacticFailed

type tactic = goal -> dgoals

let assumption : Id.t -> tactic =
  fun id goal ->
    try
      let h = List.find (fun f -> f.identifier = id) goal.hypothesis in
      if h = goal.conclusion then PopGoal goal else raise TacticFailed
    with Not_found ->
      raise TacticFailed

type proof_state = goal list

let change proof_state dgoals =
  match proof_state with
    | [] -> [] (* FIXME *)
    | g :: gs ->
      match dgoals with
	| PushGoal g' -> g' :: gs
	| PopGoal g' -> assert (g = g'); gs
	| Identity -> g :: gs

let apply proof_state tactic =
  match proof_state with
    | [] -> Identity
    | g :: _ -> tactic g

let start_proof_for (goal : goal) : proof_state = [goal]

