# Golden tests for the spatio-temporal flattening

Each case is a spatio-temporal specification in `cases/`. `run.sh` pushes it
through the same three passes that `src/temporal.py` runs, and compares the
result of every pass with the file recorded in `expected/`:

1. `--savetaskgraphasprogram --providecontext n` — the spatial formulas become
   functions of the frame reference and the temporal operators are unrolled;
2. `--savetaskgraphasprogram` — the frame reference is instantiated at 0 and the
   whole program is inlined and shared again;
3. `--evaluatespatiotemporal` — the `inc` chains are computed away and what is
   left is a VoxLogicA 1 specification over the individual frames.

```
./run.sh                     run every case
./run.sh until-composed      run only some of them
./run.sh --update            record the current output as the expected one
```

The exit code is non-zero when any case differs from its golden file.

`cli.sh` is the companion of `run.sh` and takes no arguments. The golden tests
always invoke the tool with the same argument order, so they cannot see a
command that reads `argv` by position instead of asking the parser; `cli.sh`
checks that, and that a command which does not unroll anything keeps working
without `--numframes`.

## Reading a failure

The golden files contain generated identifiers (`op17`), so a change in the way
identifiers are handed out shows up as a diff even when the meaning of the
program is unchanged. This is intended: the point is that no change to the
translation passes unnoticed. When a diff is legitimate, look at it, then run
`./run.sh --update` and commit the new golden files together with the change.

The goldens of `until-final` at pass 2 and pass 3 agree, modulo whitespace, with
the reference files `src/test-temporal-new.imgql` and
`src/test-temporal-new-VL1.imgql` that predate this suite.

## Cases marked KNOWN BAD

A pass that fails is not an error for the harness: its exit code and message go
into the golden file the same way its output would. Two cases therefore pin the
current, *wrong* behaviour of bugs that are still open:

| case | what it pins |
| --- | --- |
| `horizon-overflow` | more `diamond` than frames: the generated program refers to `videoAt4`, which is never loaded, and nothing reports it |
| `wrong-context-name` | a frame reference that does not match `--providecontext` travels through two passes and surfaces at the third as `unbound value`, with no position and no identifier |

When one of those bugs is fixed its golden file has to be regenerated on
purpose, and the `KNOWN BAD` comment removed from the specification.
