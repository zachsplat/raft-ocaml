type node_id = int

type log_entry = {
  term : int;
  index : int;
  data : string;
}

type role =
  | Follower
  | Candidate
  | Leader

type state = {
  mutable id : node_id;
  mutable current_term : int;
  mutable voted_for : node_id option;
  mutable role : role;
  mutable log : log_entry list;
  mutable commit_index : int;
  mutable last_applied : int;
  (* leader state *)
  mutable next_index : (node_id, int) Hashtbl.t;
  mutable match_index : (node_id, int) Hashtbl.t;
}

let make_state id = {
  id;
  current_term = 0;
  voted_for = None;
  role = Follower;
  log = [];
  commit_index = 0;
  last_applied = 0;
  next_index = Hashtbl.create 8;
  match_index = Hashtbl.create 8;
}
