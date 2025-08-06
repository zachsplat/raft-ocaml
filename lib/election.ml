open Types

let last_log_info state =
  match state.log with
  | [] -> (0, 0)
  | entry :: _ -> (entry.index, entry.term)

let is_log_up_to_date state last_idx last_term =
  let our_idx, our_term = last_log_info state in
  if last_term > our_term then true
  else if last_term = our_term then last_idx >= our_idx
  else false

let start_election state peers =
  state.current_term <- state.current_term + 1;
  state.role <- Candidate;
  state.voted_for <- Some state.id;
  let last_idx, last_term = last_log_info state in
  let req : Rpc.request_vote = {
    term = state.current_term;
    candidate_id = state.id;
    last_log_index = last_idx;
    last_log_term = last_term;
  } in
  (* return list of (peer_id, message) to send *)
  List.map (fun p -> (p, Rpc.ReqVote req)) peers

let handle_vote_request state (req : Rpc.request_vote) =
  if req.term < state.current_term then
    { Rpc.term = state.current_term; vote_granted = false }
  else begin
    if req.term > state.current_term then begin
      state.current_term <- req.term;
      state.role <- Follower;
      state.voted_for <- None
    end;
    let dominated = match state.voted_for with
      | None -> true
      | Some id -> id = req.candidate_id
    in
    let grant = dominated &&
      is_log_up_to_date state req.last_log_index req.last_log_term in
    if grant then state.voted_for <- Some req.candidate_id;
    { Rpc.term = state.current_term; vote_granted = grant }
  end

(* returns true if we just became leader *)
let handle_vote_response state votes_received total_nodes (resp : Rpc.request_vote_resp) =
  if resp.term > state.current_term then begin
    state.current_term <- resp.term;
    state.role <- Follower;
    state.voted_for <- None;
    false
  end else if state.role <> Candidate then
    false
  else begin
    let votes = if resp.vote_granted then votes_received + 1 else votes_received in
    let majority = (total_nodes / 2) + 1 in
    if votes >= majority then begin
      state.role <- Leader;
      true
    end else
      false
  end
