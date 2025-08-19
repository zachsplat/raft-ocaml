open Types

let get_entry state idx =
  List.find_opt (fun e -> e.index = idx) state.log

let get_entries_from state idx =
  List.filter (fun e -> e.index >= idx) state.log
  |> List.sort (fun a b -> compare a.index b.index)

let append_entry state data =
  let last_idx = match state.log with
    | [] -> 0
    | e :: _ -> e.index
  in
  let entry = {
    term = state.current_term;
    index = last_idx + 1;
    data;
  } in
  state.log <- entry :: state.log;
  entry

let make_append_entries state peer_id =
  let next_idx = try Hashtbl.find state.next_index peer_id with Not_found -> 1 in
  let prev_idx = next_idx - 1 in
  let prev_term = match get_entry state prev_idx with
    | Some e -> e.term
    | None -> 0
  in
  let entries = get_entries_from state next_idx in
  let req : Rpc.append_entries = {
    term = state.current_term;
    leader_id = state.id;
    prev_log_index = prev_idx;
    prev_log_term = prev_term;
    entries;
    leader_commit = state.commit_index;
  } in
  Rpc.AppendEnt req

let handle_append_entries state (req : Rpc.append_entries) =
  if req.term < state.current_term then
    { Rpc.term = state.current_term; success = false; match_index = 0 }
  else begin
    state.current_term <- req.term;
    state.role <- Follower;
    state.voted_for <- None;
    let prev_ok = req.prev_log_index = 0 ||
      (match get_entry state req.prev_log_index with
       | Some e -> e.term = req.prev_log_term
       | None -> false)
    in
    if not prev_ok then
      { Rpc.term = state.current_term; success = false; match_index = 0 }
    else begin
      (* remove conflicting entries first, then append *)
      let sorted_entries = List.sort (fun a b -> compare a.index b.index) req.entries in
      List.iter (fun (entry : log_entry) ->
        match get_entry state entry.index with
        | Some existing when existing.term <> entry.term ->
          (* conflict - remove this and all following *)
          state.log <- List.filter (fun e -> e.index < entry.index) state.log;
          state.log <- entry :: state.log
        | None ->
          state.log <- entry :: state.log
        | Some _ -> ()  (* same entry, skip *)
      ) sorted_entries;
      if req.leader_commit > state.commit_index then begin
        let last_new_idx = match sorted_entries with
          | [] -> state.commit_index
          | l -> (List.nth l (List.length l - 1)).index
        in
        state.commit_index <- min req.leader_commit last_new_idx
      end;
      let mi = match state.log with [] -> 0 | e :: _ -> e.index in
      { Rpc.term = state.current_term; success = true; match_index = mi }
    end
  end

let handle_append_response state peer_id (resp : Rpc.append_entries_resp) =
  if resp.term > state.current_term then begin
    state.current_term <- resp.term;
    state.role <- Follower;
    state.voted_for <- None
  end else if resp.success then begin
    Hashtbl.replace state.next_index peer_id (resp.match_index + 1);
    Hashtbl.replace state.match_index peer_id resp.match_index
  end else begin
    let ni = try Hashtbl.find state.next_index peer_id with Not_found -> 1 in
    Hashtbl.replace state.next_index peer_id (max 1 (ni - 1))
  end
