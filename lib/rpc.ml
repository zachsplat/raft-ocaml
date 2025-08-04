type request_vote = {
  term : int;
  candidate_id : Types.node_id;
  last_log_index : int;
  last_log_term : int;
}

type request_vote_resp = {
  term : int;
  vote_granted : bool;
}

type append_entries = {
  term : int;
  leader_id : Types.node_id;
  prev_log_index : int;
  prev_log_term : int;
  entries : Types.log_entry list;
  leader_commit : int;
}

type append_entries_resp = {
  term : int;
  success : bool;
  (* optimization: tell leader where we actually are *)
  match_index : int;
}

type msg =
  | ReqVote of request_vote
  | ReqVoteResp of request_vote_resp
  | AppendEnt of append_entries
  | AppendEntResp of append_entries_resp
