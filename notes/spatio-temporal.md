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
a concession and becomes the natural formulation of the model.

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

## Not verified yet

**The generated VoxLogicA 1 program has never been run.** There is no `frames/`
directory in this repository, no VoxLogicA 1 binary, and the reference outputs
point at a `video.avi` that does not exist. The pipeline is verified as far as
the text it generates — reasonably well now, with the golden tests — and no step
further.

Before datasets, registration and formulas: take
`src/test-temporal-new-VL1.imgql`, produce ten arbitrary PNGs, and give it to
VoxLogicA 1. Half a day. If it turns out the output is not accepted — an operator
that does not exist, a different `load` syntax, an `import` that does not
resolve — it is better to know now than after training a network.

## Next, in order

1. Run the generated program through VoxLogicA 1 on throwaway data.
2. One patient, two timepoints, a trivial formula, checked by hand end to end.
   This is what shakes out the format assumptions.
3. Confirm the cohort is co-registered, or scope the registration work.
4. Generalise the output format to volumes.
5. Past operators, with the boundary convention written down.
6. The formulas, which are the research and not an estimate.
