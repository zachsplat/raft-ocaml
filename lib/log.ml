(* separate log operations since replication.ml was getting big *)

let last_index state =
  match state.Types.log with
  | [] -> 0
  | e :: _ -> e.Types.index

let last_term state =
  match state.Types.log with
  | [] -> 0
  | e :: _ -> e.Types.term

let truncate_from state idx =
  state.Types.log <- List.filter (fun e -> e.Types.index < idx) state.Types.log

let append state entry =
  state.Types.log <- entry :: state.Types.log

let get_slice state from_idx to_idx =
  List.filter (fun e ->
    e.Types.index >= from_idx && e.Types.index <= to_idx
  ) state.Types.log
  |> List.sort (fun a b -> compare a.Types.index b.Types.index)
