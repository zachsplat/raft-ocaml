(* figure out if we can advance the commit index as leader.
   a log entry is committed if it's replicated on a majority. *)

let try_advance_commit state peers =
  if state.Types.role <> Types.Leader then ()
  else begin
    let n = List.length peers + 1 in
    let majority = (n / 2) + 1 in
    (* check each index from commit_index+1 upwards *)
    let rec check idx =
      match Replication.get_entry state idx with
      | None -> ()
      | Some entry ->
        if entry.term <> state.current_term then
          check (idx + 1)  (* only commit entries from current term per raft rules *)
        else begin
          let count = List.fold_left (fun acc peer_id ->
            let mi = try Hashtbl.find state.match_index peer_id with Not_found -> 0 in
            if mi >= idx then acc + 1 else acc
          ) 1 peers in  (* 1 because leader has it *)
          if count >= majority then begin
            state.commit_index <- idx;
            check (idx + 1)
          end
        end
    in
    check (state.commit_index + 1)
  end
