(** This module drives the web application. *)

(** [start_with goal] launches a worker that will interpret user
    actions to transform the state of the proof engine and to
    adjust the user interface accordingly.

    The system is initialized with [goal].
*)
let start_with goal =
  Lwt.async (
    fun () ->
      let rec loop (proof_state : ProofEngine.proof_state) =
	UI.next_user_action (fun action ->
	  Firebug.console##log (Js.string "The engine processes the user action.");
	  let tactic = ProofEngineUI.tactic_of_user_action action  in
	  let dgoals = ProofEngine.apply proof_state tactic in
	  let proof_state' = ProofEngine.change proof_state dgoals in
	  UI.interpret dgoals
	  >> loop proof_state'
	)
      in
      UI.interpret (Goals.push_goal goal)
      >> loop (ProofEngine.start_proof_for goal)
  )

(** Install the interaction reactor into the DOM. *)
let main =
  Dom_html.window##onload <- Dom_html.handler (fun _ ->
    Random.self_init ();
    UI.init ();
    start_with Formula.(Goal.(make_goal [mk_conj [Atom "A"; Atom "B"]] (mk_atom "A")));
    Js._false
  )
