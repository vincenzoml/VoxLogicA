# Spatio-temporal flattening: state, measurements, open questions

A working note on the pipeline in `src/`, what it costs, what it assumes, and
what a paper built on it would have to answer. Written alongside the round of
fixes recorded between `a92ac26` and `7f9431c`.

## What the pipeline does

VoxLogicA 2 **evaluates nothing**. It parses a spatio-temporal specification,
reduces it to a DAG, unrolls the temporal operators against a frame reference,
and prints VoxLogicA 1 syntax. The model checking happens in VoxLogicA 1, on the
file the last pass writes. `src/temporal.py` drives three invocations:

1. `--savetaskgraphasprogram --providecontext n` — the spatial formulas become
   functions of the frame reference, and the temporal operators are unrolled;
2. `--savetaskgraphasprogram` — the frame reference is instantiated at 0 and the
   whole program is inlined and shared again;
3. `--evaluatespatiotemporal` — the `inc` chains are computed away, and what is
   left is a specification over the individual frames.

Two temporal operators exist: `diamond` (next) and `until`, in
`Reducer.fs`. `until` is unrolled bounded, with the number of frames as the
horizon: `U_N = psi`, `U_i = psi | (phi & X U_(i+1))`.

## What it costs

`tests/spatiotemporal/sharing.sh` measures the flattened specification against
the same expansion with the memoisation of the reducer switched off, which is
what it costs when no subterm is shared with any other. On a specification of the
shape a lesion tracking one would have — an intensity shared by two thresholds, a
region built from both and used twice, under an `until` — 14 operations before
flattening:

```
  frames   flattened    no sharing    ratio
       1          24            81     3.4x
       2          35           133     3.8x
      10         123           549     4.5x
      50         563          2629     4.7x

operations per frame: 11.0 flattened, 52.0 without sharing (4.7x)
```

**Both are linear in the number of frames.** The sharing is a constant factor,
not an asymptotic gain, and a claim that does not say so will not survive a
technical reviewer. What the numbers do support, precisely:

> The per-frame cost of the flattened specification is the number of *distinct*
> spatial subterms, not the size of the syntax tree of the formula. Here 11
> operations per frame against 14 in the source, and the unrolling of the `until`
> adds no spatial evaluation at all.

That is a real technical statement, and a defensible one. It says the flattening
does not pay the price one would expect, rather than that it saves an order of
magnitude.

## What the model assumes

**Co-registration.** The semantics is "the same spatial domain at different
times, indexed by the frame". For `frame(study,n)` and `frame(study,inc(n))` to
be comparable, the timepoints have to be aligned voxel by voxel in a common
space. Nothing in the pipeline can detect that they are not: a misregistered
series produces a program that runs and means nothing. `touch` and `surrounded`
are sensitive to displacements of a few voxels, so the registration error has to
be discussed rather than assumed. This is the first thing a reviewer will attack.

**Lesion identity.** The tool reasons about voxels and connected components, not
about objects with an identity that persists. What *is* expressible, with `lcc`
and the collective operators of `stdlib2.imgql` (`maskCC`, `minCC`, `maxCC`,
`collect`): "the connected component of the lesion at *t* that touches a
component at *t+1*". What is not: a persistent label, "component #3 at *t* is
component #7 at *t+1*". Stated this way the voxel-wise reformulation stops being
a concession and becomes the natural formulation of the model. How to get a
persistent label anyway is the subject of *Quantifying over labels* below.

**The boundary of the trace.** Past the last frame the last frame persists, and
the generated program loads it again under the names of the frames that do not
exist, with a warning. This matters because a specification reaches past the end
more often than it looks: an `until` does it at the innermost step of its
unrolling, and an `until` applied to another one does it once more, at any
horizon (`tests/spatiotemporal/cases/until-nested.imgql`). A past operator would
need the mirror convention at frame 0, and it would have to be stated: it changes
what formulas mean at the boundary.

**No past operators.** For longitudinal data "it was already there at the
previous timepoint" is often more natural than anything expressible with `next`
and `until` alone. Implementing them is symmetric and small — a `dec` beside
`inc`, and the lower bound of the range check is already in place — but the
boundary convention above is a design decision, not an implementation detail.

**The output format is fixed.** `PartialEvaluation.fs` writes
`frames/{video}_{j}.png` and `save "{name}.png"`. Volumes in `.nii.gz` need this
generalised: directory, naming and extension all belong in one place, and the
golden tests now surround it.

## Towards a paper

**The claim that holds.** Not "we expressed lesion tracking in a spatio-temporal
logic, here is the formula" — the answer to that is *so what*. The logic has to
buy something measurable. The workable shape: the network produces segmentations
per timepoint, some detections are false positives with no temporal coherence, a
spatio-temporal specification filters them, and a table shows precision, recall
and dice before and after. The baseline is the network itself, so nothing has to
beat the state of the art; the network is declared not to be the contribution.

**A purely temporal filter is not enough.** "Persists for at least two
timepoints" is three lines of numpy and a reviewer knows it. What only this
machinery gives is the combination with the spatial operators: a lesion that
*grows until* it touches a critical structure; a region that at some point
becomes surrounded by edema and stays so; a new component that appears adjacent
to a pre-existing one and nowhere else. If the properties are only temporal, the
contribution evaporates.

**Framing against the group's earlier work.** With topochecker and the STTT paper
behind it, "what is new with respect to your own previous work" is certain to be
asked. The strong answer is not the logic, it is the compilation: flattening
spatio-temporal to purely spatial to reuse the existing engine, with the sharing
that comes out of the reduction. That gives the paper the shape "technique plus
case study", which is more defensible at a workshop than a purely applied paper —
and the measurement above is the number that supports it, with its qualification.

**Dataset.** BraTS is single-timepoint and of no use here. ISBI 2015 has several
timepoints per patient; MSSEG-2 has two, and with two the unrolling of an `until`
is literally one step, so the temporal content is empty. A clinical series of the
group's own, even a few dozen patients, is perfectly acceptable at a workshop and
avoids a direct comparison with leaderboards.

**Venue.** OVERLAY is the closest fit. NeSy if the neural-symbolic hybrid is the
angle, a MICCAI workshop if the clinical one is; the interpretability ones are
sympathetic, since a declarative specification is by construction more readable
than a feature map.

## Quantifying over labels

The theory extends the logic with an existential over points and atomic
propositions: at runtime the labels of the connected components become atomic
propositions, and a formula may quantify over them -- "the region labelled L is
reachable from the region labelled K". This section is about whether the code
here can carry that, and about the one problem that has to be settled first.

### The compiler is already the right shape

Everything the pipeline does is "parametrise on an index, unroll to a bound,
partially evaluate". A label is a second index of that kind, and the `until`
branch of `Reducer.ToProgram` is the template: it unrolls over `1..numFrames`
building a chain of `or`; `exists L. psi(L)` unrolls over `1..maxLabels`
building a chain of `or`, and it is the simpler of the two, a fold rather than a
recursion. Concretely:

- `ctxArgs` is already a `string list`; today it is `[n]`, it would become
  `[n; l]`, and the declarations go from `let op7(n)` to `let op7(n,l)`;
- one more branch of `sem`, beside `until` and `diamond`;
- "the region labelled l" is `eq(lcc(phi), l)`, an application of identifiers the
  compiler does not have to understand;
- `PartialEvaluation` is almost untouched: a label index has no `inc` chains, no
  table of loads, no convention at the boundary. Once the first pass has unrolled
  the quantifier into literal constants, the memoisation of the second pass does
  the rest, as it already does for `frame(v,0)` against `frame(v,1)`.

VoxLogicA 1 needs nothing new: `lcc`, `mask`, `min`, `max` are there, and
`stdlib2.imgql` already builds the collective operators on them. Days of
compiler work, of the order of the round of fixes recorded above.

### Three difficulties, in increasing order

**The bound is not a fact about the data.** `--numframes` is known: it is the
length of the series. The number of connected components is a property of the
result of a computation, unknown until runtime. If `maxLabels` under-approximates
it, the existential is unsound: it misses witnesses silently. The generated
program can check it -- `max(lcc(phi)) <= K` is expressible -- but a declarative
specification cannot abort; at most it prints something a person has to read.
This goes in the paper as a hypothesis, not in a footnote. (Revisited under
*What landed*: with the labelling named in the binder, the toolchain computes
the bound before the run and checks it after.)

**The cost multiplies.** A quantifier multiplies the spatial work under it by K,
two nested ones by K squared. At the 11 operations per frame measured above, K=50
gives some 550 per frame on 9M-voxel volumes. Memoisation helps only where the
subterms do *not* depend on the label, which is exactly where one does not care.
`sharing.sh` measures this unchanged, given the quantified specification.

**Labels are not stable across frames.** This is the one that has to be settled
before the other two matter.

### Deterministic is not stable

`lcc` is deterministic: the same input gives the same labels. Two similar frames
are two different inputs. ITK assigns labels consecutively in the order it meets
the components in a raster scan, so the label of a component depends on what
lies *before it* in the image. A new lesion earlier in scan order at *t+1* shifts
every later label by one; a merge or a split renumbers everything after it. The
labels stay stable exactly as long as nothing happens, and break in the cases
one wants to observe. (The argument does not depend on the implementation: any
labelling of a single image is a function of that image, and no such function
can know the previous frame.)

Hence a formula such as `lcc(phi@t) = L and lcc(phi@(t+1)) = L` does not say
"the same component persists": it says two integers coincide, which means
nothing. The quantifier gives the binding *within* a frame for free; across
frames it gives only the vocabulary in which a criterion of identity has to be
stated. Two ways to state one, both of which remove the dependence on how `lcc`
numbers, and both of which make the criterion explicit -- which is the honest
way to present it.

**A. Label the temporal footprint, not the frame.** Compute `lcc` once, on the
union of the lesion over all times:

```
let footprint = lcc(eventually(lesion))
let region(L) = eq(footprint, L)        // an atomic proposition, frame-independent
```

`region(L) and lesion@t` is the part of footprint L present at *t*. Consistent
by construction, and expressible today with no change to the compiler:
`eventually phi` is `until(true, phi)`, whose unrolling is the disjunction over
the frames, and `lcc` is then computed once on it rather than once per frame
(checked: the third pass emits a single `lcc` over `lesion@0 or ... or
lesion@N`). The bound K becomes `max(footprint)`, still unknown at compile time
but computed once rather than per frame.

The limitation is semantic and has to be said: two lesions that *merge* have a
single footprint from the start, so "two lesions merged" is not expressible; and
two distinct lesions occupying the same space at different times collapse into
one. For lesions that do not move, given registration, only the first matters.

**B. Label the first frame and propagate with `~>`.** The labels come from
`lcc(lesion@0)`; at each later time, "region L" is what at *t+1* lies in a
component that touches region L at *t*:

```
region_0(L)     = eq(lcc(lesion@0), L)
region_{t+1}(L) = lesion@(t+1) ~> region_t(L)
```

A recursion along *t* -- exactly the shape the unrolling machinery already
handles; `until` is a recursion along *t* with an `or` where this has a `~>`. It
would be a new temporal operator beside `until`, of the same order of work.
Merges and splits are handled as they come: the regions stop being a partition
and become sets of labels, a merged component carrying both. New lesions are the
one gap: they inherit nothing and have to be labelled separately, with fresh
labels past those of frame 0.

And a dividend: with propagation the K squared disappears. One no longer asks
"exists K at *t+1* overlapping L at *t*"; one asks about L at *t+1* directly. The
criterion of identity -- overlap -- is baked into the propagation, and the
quantification costs K again.

### The choice

**B is the one to pursue.** It is the more expressive of the two -- it is the
only one that sees merges and splits -- and it is a contribution in its own
right, an operator the paper can present as such, beside the lesion tracking it
serves. A is worth keeping as the zero-cost baseline: it needs no compiler
change, so it is what to run in the container first, and it is the comparison
that shows what B buys.

In either case the answer to "how do you know it is the same lesion" becomes a
line of the specification rather than an assumption about how ITK numbers its
components.

### A cheaper existential to try first

VoxLogicA already has an existential over components, implicit and at no cost:
`through`, written `~>`. `a ~> b` holds at *x* when *x* lies in a component of
`a` that meets `b`. So `lesion@(t+1) ~> lesion@t` is "this voxel is in a lesion
at *t+1* whose component overlapped a lesion at *t*" -- persistence per
component, with no quantifier and no factor K.

The line is clear: `~>` binds the component *implicitly to the point of
evaluation*, so it covers everything about *the* component one is standing in.
The quantifier is needed when a label has to be *bound and reused elsewhere*:
comparing two distinct components with each other, or following one through
three or more times. Before implementing, write down the properties actually
wanted and see how many fall on the `~>` side. If almost all do, the quantifier
is a theoretical contribution with a thin use case, and that is worth knowing
first. If the interesting ones remain -- comparisons between components are the
typical case -- the extension has its justification written above it, which is
what a reviewer asks for.

### What landed (2026-09-16, branch `existential-labels`)

The existential is in, as sketched above, with one change to the sketch:
the quantifier names its domain. `exists(l, labels, psi)` makes `l` a
parameter of every declaration beside the frame reference (`let op7(n,l)`),
unrolls into `or(psi(1), ..., psi(K))` with `K` from `--maxlabels`, and leaves
the second pass to share whatever does not depend on `l`. The bound is asked
for only when something is quantified; a label variable that reaches a goal
without meeting its `exists` is rejected by the first pass, since the goal
would otherwise instantiate it silently. Nested quantifiers over distinct
variables work, and the third pass shows the K squared the section above
predicts. Golden cases: `exists-footprint`, `exists-label`, `free-label`,
`self-bound-labelling`.

**The bound, revisited.** The first difficulty above -- `K` under-approximated
loses witnesses silently, and VoxLogicA 1 cannot abort -- looked like a
limitation to write down. It is not, once the compiler knows *which* labelling
`l` ranges over; with `exists(l, psi)` it did not, and could neither check nor
compute the bound. With the labelling named, two things, neither of which
touches VoxLogicA 1:

- *the check is emitted.* The unrolled result travels through the second pass
  as `bounded(labels, K, result)`, a pseudo-operator like `inc`, so that every
  instance the memoisation makes -- one per frame the existential is evaluated
  at -- keeps its own labelling; the third pass resolves it to the result plus
  `print "labels opN within K" max(opN) .<=. K` beside the goals. The labelling
  is computed for the formula anyway, so the check costs nothing. A false there
  is the driver's signal to stop.
- *the bound is computed, not guessed.* `--probe` emits the same specification
  with its goals replaced by `print max(labelling)` at every frame, and needs
  no bound. The driver runs the probe first, reads the numbers, and unrolls with
  their maximum: `K` becomes a fact about the data, and the check above is the
  belt to that pair of braces. One labelling that depends on another label
  variable cannot be probed (it would have to be probed once per value of that
  variable), and says so.

What remains for the paper is then the honest statement: the existential is
bounded, the bound is measured on the data before the run and verified after
it, and the price is one extra evaluation of the labellings.

**The propagation operator (2026-09-17).** `tracked(phi, l)` is in, as B
above defines it: `eq(lcc(phi@0), l)` at frame 0, `through(region@t,
phi@(t+1))` after. What made it different from `until` is that the recursion
runs along *absolute* frames, so it cannot be unrolled relative to the frame
reference: the first pass writes the chain out with literal frames, one step
per frame of the video, and wraps it in `select(n, step_0, ..., step_N)`, a
pseudo-operator the third pass resolves by the value of `n` as it resolves
`inc`. Past the last frame the last step persists, which is what the
semantics gives anyway (the last step is a union of components of the last
frame, and touching it gives it back), and the run warns as it does for a
frame past the end. The steps not selected are left as declarations nothing
refers to; VoxLogicA 1 evaluates from its goals, so they cost nothing, but a
reachability sweep in the third pass would tidy the output.

New components stay outside every region, as the definition says; no fresh
labels. For lesion tracking that is enough, because "new lesion" is definable
as the complement, `and(phi, not(exists(l, initially(lcc(phi)), tracked(phi,
l))))`, and the labelling the existential ranges over is that of frame 0,
which `--probe` measures.

The three `merge-*` cases are the semantic check the goldens cannot make: on
a drawn series where two lesions merge and the merged one moves on, the
footprint sees nothing, the labels of the first frame see the merge between
the first two frames, and only `tracked` sees, at frame 2, what descends from
both. Their expected images are drawn by hand in `tests/spatiotemporal/
frames/draw.py`; running them is what the container is for.

Writing the footprint baseline as a golden case turned up something the
sketch of A above glosses over. `lcc(until(true, lesion))` under a `diamond`
is *re-evaluated from the next frame*: `until` is relative to the current
frame, so the footprint at *t+1* is the union from *t+1* on, a different
image and a different labelling, and `eq(footprint, L)` at *t* and at *t+1*
compare labels of two `lcc`. The frame-independent footprint needs an
**absolute** frame, which the compiler did not have: `diamond` and `until`
only move relative to `n`. Hence `initially(phi)`, phi at frame 0 whatever
stands around it -- a one-line branch, `op(0,l)` in place of `op(n,l)` -- and
the footprint is `initially(lcc(until(true, lesion)))`. With it the third pass
emits a single `lcc` over the lesion at every frame and one `eq` per label,
which is what A promised. It is also the first piece of what `tracked` needs:
its recursion is along absolute frames too.

## Not verified here

**The generated VoxLogicA 1 program has run, in another repository.** It is not
reproducible from this one: there is no `frames/` directory, no VoxLogicA 1
binary, and the reference outputs point at a `video.avi` that does not exist. The
round of fixes recorded here also changed what the third pass emits -- comment
lines beside the repeated frames, and as many repeated frames as the
specification reaches rather than one -- so the run is worth repeating before
anything is built on top of it.

The way to make it reproducible is a container that fetches a stable release of
VoxLogicA 1 and runs the whole toolchain, throwaway frames included. That turns
"it worked once, elsewhere" into something the golden tests can sit next to.

## Next, in order

1. A container that fetches a stable VoxLogicA 1 and runs the toolchain end to
   end on throwaway frames.
2. One patient, two timepoints, a trivial formula, checked by hand end to end.
   This is what shakes out the format assumptions.
3. Confirm the cohort is co-registered, or scope the registration work.
4. Generalise the output format to volumes.
5. Past operators, with the boundary convention written down.
6. Done: the propagation operator (B above), the quantifier over its labels,
   and `initially` for the absolute frame. What is left here is the run of
   the `merge-*` cases against their drawn expectations, which needs 1.
7. The formulas, which are the research and not an estimate.
