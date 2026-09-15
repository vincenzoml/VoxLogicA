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

`sharing.sh` is not a test but a measurement: it reports how many operations a
specification flattens to per frame, against the same expansion with the
memoisation switched off. It builds a second copy of the tool in a temporary
directory to get that second figure, so it leaves the working tree alone. See
`notes/spatio-temporal.md` for what the numbers do and do not support.

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

## The locale

The cases run under `LC_ALL=de_DE.UTF-8`, a locale whose decimal separator is a
comma. Numbers used to be written with the separator of whatever machine the tool
ran on, so a threshold like `42.5` came out as `42,5` and parsed back nowhere;
running the cases under such a locale is what makes a return of that bug fail
here rather than only on the machines that happen to use one.

## Frames past the end of the video

A specification may look further ahead than the video is long: an `until` does it
at the innermost step of its unrolling, an `until` applied to another one does it
once more, and a chain of `diamond` does it as many times as it is long. Past the
end the last frame persists, so the generated program loads it again under the
names of the frames that do not exist, and the run says so:

```
[warn] 'video' has 2 frame(s) and the specification looks as far as frame 4:
       frames 2 to 4 repeat the last one
```

`until-nested` and `horizon-overflow` are the two cases that reach past the end,
the first because of the nesting and the second because of a horizon that is
probably a mistake. The warning tells them apart, and it is recorded in the
golden files together with the output: a warning that stops being emitted is as
much a regression as a wrong translation.

## Cases that fail on purpose

A pass that fails is not an error for the harness: its exit code and its message
go into the golden file the same way its output would. `wrong-context-name` uses
this to pin a diagnostic: a frame index that is not the frame reference has to be
rejected by the *first* pass, next to the specification that contains it, rather
than reach the end of the pipeline.

The same mechanism is how to record a bug that is still open: add the case, let
`--update` write down whatever the tool does today, and mark the specification
`KNOWN BAD`. Fixing the bug then shows up as a diff, and the golden file is
regenerated on purpose.

And it is how a feature is specified before it exists. `tracked-label` and
`exists-label` are marked `NOT IMPLEMENTED`: they are written in the syntax the
label propagation and the existential over labels are meant to have, and their
golden files record what the tool does with it today -- it passes `tracked`,
`exists` and the bound variable through untouched, like any identifier it does
not know, and emits a program VoxLogicA 1 would reject. The diff that appears
when the operators land is the check that they do what the comment in the
specification says. See `notes/spatio-temporal.md`, *Quantifying over labels*.
