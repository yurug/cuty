(** Interpret the user action in terms of tactics. *)
let tactic_of_user_action = function
  | UI.Use id -> ProofEngine.assumption id

