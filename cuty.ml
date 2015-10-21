let tactic_of_user_action = function
  | UI.Use id -> ProofEngine.assumption id

let start_with goal =
  Lwt.async (
    fun () ->
      let rec loop (proof_state : ProofEngine.proof_state) =
	UI.next_user_action (fun action ->
	  Firebug.console##log (Js.string "The engine processes the user action.");
	  let tactic : ProofEngine.tactic = tactic_of_user_action action  in
	  let dgoals = ProofEngine.apply proof_state tactic in
	  let proof_state' = ProofEngine.change proof_state dgoals in
	  UI.interpret dgoals
	  >> loop proof_state'
	)
      in
      UI.interpret (Goal.push_goal goal)
      >> loop (ProofEngine.start_proof_for goal)
  )

let onload _ =
  UI.init ();
  start_with (Goal.(make_goal [(Id 1, atom "A"); (Id 2, atom "B")] (Id 3, atom "A")));
  Js._false

let go =
  Dom_html.window##onload <- Dom_html.handler onload
