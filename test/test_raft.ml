let test_election () =
  let s1 = Raft.Types.make_state 1 in
  let s2 = Raft.Types.make_state 2 in
  let s3 = Raft.Types.make_state 3 in

  (* node 1 starts election *)
  let msgs = Raft.Election.start_election s1 [2; 3] in
  assert (s1.role = Candidate);
  assert (s1.current_term = 1);
  assert (List.length msgs = 2);

  (* node 2 votes yes *)
  let req = match snd (List.hd msgs) with
    | Raft.Rpc.ReqVote r -> r
    | _ -> failwith "expected ReqVote"
  in
  let resp2 = Raft.Election.handle_vote_request s2 req in
  assert (resp2.vote_granted = true);

  (* node 3 also votes yes *)
  let resp3 = Raft.Election.handle_vote_request s3 req in
  assert (resp3.vote_granted = true);

  (* node 1 gets majority *)
  let _won = Raft.Election.handle_vote_response s1 1 3 resp2 in
  (* 1 (self) + 1 = 2, majority of 3 is 2 *)
  assert (s1.role = Leader);
  Printf.printf "election test passed\n%!"

let test_replication () =
  let s = Raft.Types.make_state 1 in
  s.current_term <- 1;
  s.role <- Leader;

  let e1 = Raft.Replication.append_entry s "cmd1" in
  assert (e1.index = 1);
  let e2 = Raft.Replication.append_entry s "cmd2" in
  assert (e2.index = 2);

  (* follower receives entries *)
  let follower = Raft.Types.make_state 2 in
  let req = match Raft.Replication.make_append_entries s 2 with
    | Raft.Rpc.AppendEnt r -> r
    | _ -> failwith "nope"
  in
  let resp = Raft.Replication.handle_append_entries follower req in
  assert (resp.success = true);
  assert (resp.match_index = 2);
  Printf.printf "replication test passed\n%!"

let () =
  test_election ();
  test_replication ();
  Printf.printf "all good\n"

let test_log_ops () =
  let s = Raft.Types.make_state 1 in
  s.current_term <- 1;
  ignore (Raft.Replication.append_entry s "a");
  ignore (Raft.Replication.append_entry s "b");
  ignore (Raft.Replication.append_entry s "c");
  assert (Raft.Log.last_index s = 3);
  assert (Raft.Log.last_term s = 1);
  let slice = Raft.Log.get_slice s 2 3 in
  assert (List.length slice = 2);
  Printf.printf "log ops test passed\n%!"
