(* janky serialization. should probably use protobuf or something
   but this works for testing *)

let write_int buf n =
  Buffer.add_char buf (Char.chr ((n lsr 24) land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr (n land 0xff))

let read_int s off =
  let b i = Char.code (String.get s (off + i)) in
  (b 0 lsl 24) lor (b 1 lsl 16) lor (b 2 lsl 8) lor (b 3),
  off + 4

let write_string buf s =
  write_int buf (String.length s);
  Buffer.add_string buf s

let read_string s off =
  let len, off = read_int s off in
  String.sub s off len, off + len

let write_entry buf (e : Types.log_entry) =
  write_int buf e.term;
  write_int buf e.index;
  write_string buf e.data

let read_entry s off =
  let term, off = read_int s off in
  let index, off = read_int s off in
  let data, off = read_string s off in
  { Types.term; index; data }, off

(* TODO: serialize full RPC messages *)
(* TODO: this is getting messy, just use a proper format *)
