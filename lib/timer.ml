(* election timeouts. in real raft you'd use randomized timers
   to prevent split votes. this is a simple version *)

let election_timeout_ms = 150  (* base *)
let heartbeat_interval_ms = 50

let random_election_timeout () =
  election_timeout_ms + Random.int election_timeout_ms

let now_ms () =
  int_of_float (Unix.gettimeofday () *. 1000.0)

type t = {
  mutable deadline : int;
}

let create () =
  { deadline = now_ms () + random_election_timeout () }

let reset t =
  t.deadline <- now_ms () + random_election_timeout ()

let is_expired t =
  now_ms () >= t.deadline
