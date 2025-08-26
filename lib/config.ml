type t = {
  id : Types.node_id;
  peers : Types.node_id list;
  election_timeout_base : int;  (* ms *)
  heartbeat_interval : int;
}

let default id peers = {
  id;
  peers;
  election_timeout_base = 150;
  heartbeat_interval = 50;
}
