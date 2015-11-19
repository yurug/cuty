open Formula

type goal = {
  hypothesis : formula list;
  conclusion : formula;
}

let make_goal hypothesis conclusion =
  { hypothesis; conclusion }

