open Goal

type dgoals =
  | PushGoal of goal
  | PopGoal of goal
  | Identity

let push_goal goal = PushGoal goal
