# raft-ocaml

raft consensus implementation. leader election and log replication work,
snapshotting is half-done.

wrote this mostly to understand the paper better. turns out the tricky
part isn't the algorithm, it's all the edge cases around timing.

## build

```
dune build
```

## test

```
dune exec test/test_raft.exe
```

not really usable as a library yet, more of a reference implementation.
