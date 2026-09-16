# Golden tests for the spatio-temporal flattening

Each case is a spatio-temporal specification in `cases/`. `run.sh` pushes it
through the same three passes that `src/temporal.py` runs, and compares the
result of every pass with the file recorded in `expected/`:

1. `--savetaskgraphasprogram --providecontext n` — the spatial formulas become
   functions of the frame reference and the temporal operators are unrolled;
   a case that quantifies over labels also gets `--maxlabels K`, the third
   column of `cases.txt`, and its `exists` are unrolled here too;
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

And it is how a feature is specified before it exists. `tracked-label` is
marked `NOT IMPLEMENTED`: it is written in the syntax the label propagation is
meant to have, and its golden file records what the tool does with it today --
it passes `tracked` through untouched, like any identifier it does not know,
and emits a program VoxLogicA 1 would reject. The diff that appears when the
operator lands is the check that it does what the comment in the specification
says. `exists-label` was specified the same way and its golden now records the
existential unrolled around a `tracked` that still is not. See
`notes/spatio-temporal.md`, *Quantifying over labels*.

## The existential over labels

`exists(l, labels, psi)` makes `l` a parameter of every declaration beside the
frame reference -- `let op7(n,l)` -- and unrolls into `or(psi(1), ..., psi(K))`,
`K` being `--maxlabels`. `exists-footprint` is the case that shows the shape on
a region VoxLogicA 1 can compute today; `free-label` pins the diagnostic for a
label variable that reaches a `save` without meeting its `exists`, since the
goal would otherwise instantiate it silently, and `self-bound-labelling` the one
for a labelling that depends on the variable it gives its values to.

The bound is a hypothesis, not a fact: a label past `K` is a witness the
disjunction misses, and the result says false where the specification says
true. Naming the labelling is what makes that visible. The result travels
through the second pass wrapped as `bounded(labels, K, result)`, one instance
per frame it is evaluated at, and the third pass resolves it into the result
and a `print "labels opN within K" max(opN) .<=. K` beside the goals: whatever
drives the run reads it and stops on a false. And the golden of every
quantified case has a second part, the same specification under `--probe`: its
goals replaced by the highest label of each labelling at each frame, which needs
no bound and is what a driver runs first to compute `--maxlabels` from the data.

`initially(phi)` is phi at frame 0 whatever temporal operators stand around it,
the one absolute frame where `diamond` and `until` move relative to the current
one. `initially-nested` pins it on its own; `exists-footprint` is why it exists:
without it the footprint under a `diamond` is a different `lcc`, taken from the
next frame on.
