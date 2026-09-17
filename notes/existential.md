# The existential over labels: how each of its forms works

A companion to `spatio-temporal.md`. That note records why an existential
over connected components was wanted and which criterion of identity was
chosen; this one explains what is actually in the code on branch
`existential-labels`, form by form, with the programs the pipeline emits.
Everything quoted below is taken from the golden files in
`tests/spatiotemporal/expected/`, and the semantic claims were run with
`tests/spatiotemporal/semantics.sh` against VoxLogicA 1.

There are four things one may mean by "existential over components", and
they are best seen as a ladder:

| form | quantifier | criterion of identity across time | cost | sees a merge |
|---|---|---|---|---|
| 0. `through` | implicit, bound to the point of evaluation | overlap, one step | none | as a point property only |
| A. footprint | `exists(l, footprint, psi)` | one labelling of the union over time | K | no |
| B0. first frame | `exists(l, initially(lcc(phi)), psi)` | labels of frame 0, no propagation | K | between frames 0 and 1 |
| B. propagation | `exists(l, initially(lcc(phi)), ... tracked(phi, l) ...)` | overlap, propagated frame by frame | K, one chain per label | yes, at any frame |

The quantifier itself (section 2) is the same machinery in A, B0 and B; what
changes is the region `l` names (section 3). `initially` (section 4) is the
piece both need.

## 1. The point of departure: two indices, three passes

VoxLogicA 2 evaluates nothing. It reduces a specification to a DAG, turns
every node into a declaration parametrised on the *frame reference* `n`,
unrolls the temporal operators, and hands VoxLogicA 1 a program over the
individual frames. Three passes:

1. `--savetaskgraphasprogram --providecontext n`: every declaration becomes
   `let opK(n) = ...`; `diamond` is `opK(inc(n))`; `until` is unrolled to the
   horizon `--numframes`.
2. `--savetaskgraphasprogram`: the goals apply the declarations at `n = 0`,
   everything is inlined, and the reduction memoises: two identical
   applications become one declaration. This is where sharing happens.
3. `--evaluatespatiotemporal`: `inc` chains are computed away, `frame(v, k)`
   becomes a load of frame `k`, and what is left is VoxLogicA 1.

The existential adds a **second index**. A label variable is one more
parameter of every declaration, beside `n`, and the two operators that move
an index are symmetrical:

```
                n (frame)  --->  diamond:    opK(inc(n), l)
                                 initially:  opK(0, l)
   opK(n, l)
                l (label)  --->  exists:     opK(n, 1) | opK(n, 2) | ... | opK(n, K)
```

Nothing else in the pipeline had to learn about labels: pass 2 memoises
`opK(0, 1)` against `opK(0, 2)` exactly as it memoises `frame(v, 0)` against
`frame(v, 1)`, and pass 3 sees literal numbers.

## 2. The quantifier: `exists(l, labels, psi)`

### 2.1 Syntax and meaning

```
exists(l, labels, psi)
```

- `l` is a **bare lower-case identifier** (an initial capital parses as an
  operator). It is free in the specification, like `n`; the binder is what
  tells the two apart.
- `labels` is an **image of labels**, the domain `l` ranges over: `lcc` of
  something, or `initially` of one. Naming it is what makes the bound
  checkable and computable (2.4, 2.5).
- `psi` is the body, in which `l` is used as a number: `eq(labels, l)`,
  `tracked(phi, l)`.

Meaning: `exists(l, labels, psi)` holds at a voxel when `psi[l := i]` holds
there for some label `i` of `labels`. Operationally, for some `i` in `1..K`,
with `K` the bound of 2.4.

### 2.2 Pass 1: the variable becomes a parameter, the binder unrolls

`Reducer.ToProgram` first collects the binders of every `exists` in the DAG
(`labelVars`), in order of first appearance, and appends them to the context
arguments: `ctxArgs = [n] @ labelVars`. Every declaration is then written with
all of them. From `exists-footprint`, 3 frames, `--maxlabels 2`:

```
let op0(n,l)=l                              -- the variable: its own value
let op8(n,l)=gt(op6(n,l),op7(n,l))          -- lesion at frame n
...
let op10(n,l)=lcc(op22(n,l))                -- lcc of the union over time (until, unrolled above)
let op11(n,l)=op10(0,l)                     -- initially: frame 0 whatever n is
let op12(n,l)=eq(op11(n,l),op0(n,l))        -- region l
let op13(n,l)=and(op12(n,l),op8(n,l))
let op14(n,l)=op13(inc(n),l)                -- diamond: n advances, l passes through
let op15(n,l)=and(op13(n,l),op14(n,l))
let op16(n,l)=bounded(op11(n,l),2,or(op15(n,1),op15(n,2)))
save "persistent" op16(0,0)
```

Three things to read off:

- **The binder is a fold**, not a recursion: `or(psi(n,1), or(psi(n,2), ...
  psi(n,K)))`, one declaration. `until` needs declarations for its
  intermediate steps because each is re-applied at `inc(n)`; the existential
  does not.
- **`l` is instantiated only where the binder says**: `op15(n,1)` and
  `op15(n,2)`. Everywhere else `l` is passed along unchanged, including
  through `diamond` (`op13(inc(n),l)`) and through the unrolling of `until`.
- **The goal passes `0` for every parameter**: `op16(0,0)`. For `n` this is
  frame 0; for `l` it is a value that is never read, because of the check in
  2.3.

With two binders the parameters are `(n,k,l)` and each `exists` instantiates
its own variable, leaving the other in place:

```
let op14(n,k,l)=or(op13(n,1,l),op13(n,2,l))     -- exists k
let op15(n,k,l)=or(op14(n,k,1),op14(n,k,2))     -- exists l
```

The result is wrapped in **`bounded(labels, K, disjunction)`**, a
pseudo-operator in the sense `inc` is one: pass 2 does not know it and treats
it as an application, which is what makes it useful (2.4).

### 2.3 Pass 1: what is rejected

The reduction has already inlined every declared name, so the binder's
argument is a DAG node, and the checks are on nodes:

| specification | error |
|---|---|
| `exists(3, ...)`, or `let l = 3` then `exists(l, ...)` | `exists binds '3', which is not an identifier` |
| `exists(n, ...)` | `exists cannot bind 'n', which is the frame reference` |
| `exists(l, psi)` | `exists must take three arguments: the label variable, the labelling it ranges over, and the formula it is bound in` |
| `and(exists(l, L, region(l)), region(l))` | `'l' is free in save "wrong": no exists binds it` |
| `exists(l, lcc(eq(c, l)), ...)` | `the labelling that exists ranges over for 'l' depends on 'l' itself` |

The free-variable check is the important one. `ToProgram` computes, for every
node, the set of label variables that reach it without meeting their binder
(`freeLabels`: a bare `l` contributes `{l}`, an `exists(l, labels, body)`
contributes `free(labels) ∪ (free(body) \ {l})`, anything else the union of
its arguments). A goal whose root has a non-empty set is refused. Without
this, the `0` the goal passes for `l` would silently stand in for a value the
specification never gave.

### 2.4 Pass 2 and pass 3: sharing, and the check of the bound

Pass 2 applies `op16(0,0)`, inlines, and memoises. What does not depend on
`l` is written K times in pass 1 and becomes one declaration here; what does
depend on `l` is computed once per label. From `exists-footprint`, pass 3:

```
let op26 = lcc(op25)                -- one lcc, for both labels
let op28 = 1
let op29 = eq(op26,op28)            -- region 1 ...
let op30 = and(op29,op6)
let op31 = and(op29,op11)
let op32 = and(op30,op31)
let op33 = eq(op26,op27)            -- region 2 ...
let op34 = and(op33,op6)
let op35 = and(op33,op11)
let op36 = and(op34,op35)
let op37 = or(op32,op36)
let op38 = op37                     -- bounded, resolved
save "persistent.png" op38
// the bounds the existentials were unrolled to: false here means witnesses were missed
print "labels op26 within 2" max(op26) .<=. 2
```

The cost is therefore **K times the subterms that depend on `l`**, and
nothing else. Two nested quantifiers cost K² on the subterms that depend on
both (the golden with two variables shows four `and`s for K = 2). This is the
same property `sharing.sh` measures along the frame index.

`bounded` survives pass 2 because pass 2 treats it as any application: each
frame at which the existential is evaluated yields its own
`bounded(labels-at-that-frame, K, result)`, memoised with everything else, so
each instance keeps the labelling it actually ranged over. Pass 3 resolves it
to an alias (`let op38 = op37`) and records the pair `(labels, K)`; after the
goals it prints, once per distinct pair:

```
print "labels opN within K" max(opN) .<=. K
```

`max` of an image is a number in VoxLogicA 1 and `.<=.` compares numbers, so
this prints `true` or `false`. The labelling is computed for the formula
anyway, so the check is free. VoxLogicA 1 cannot abort on it, but a driver
can: `semantics.sh` refuses a run on which any of these prints is `false`.

### 2.5 The bound: a hypothesis turned into a measurement

`K` cannot be known before `lcc` has run. Guessing it low loses witnesses
silently; guessing it high costs K. `--probe` removes the guess:

```
pass 1 --probe  →  the goals are replaced by, for every exists and every frame j,
                      print "labels of l in opL at frame j" max(opL(j, 0, ...))
pass 2, pass 3  →  a VoxLogicA 1 program that computes the labellings and nothing else
run it          →  labels of l in op11 at frame 0=1.0   ...
K               →  the maximum of what it printed
```

Then the specification is unrolled with `--maxlabels K`, and the check of
2.4 confirms it after the run. The probe needs no bound (its goals never
reach the existentials, which are unrolled with a placeholder the next pass
drops). It refuses a labelling that depends on an outer label variable —
`exists(k, C, exists(l, f(k), ...))` — since that would have to be probed
once per value of `k`, which is the bound being asked for; such a
specification takes `--maxlabels` by hand and still gets the check.

On the drawn merge series the probe measured 1 label for the footprint and 2
for the labels of the first frame, as the semantics predicts.

### 2.6 What the quantifier does not do

It binds `l` **within a frame**. `region(l)` at `t` and `region(l)` at `t+1`
are "the same region" only if `labels` makes them so, and `lcc` of a single
frame does not: it numbers components in raster order, so a new component
earlier in scan order shifts every later label. The quantifier supplies the
vocabulary; the identity across time has to come from `labels`, which is what
section 3 is about.

## 3. What `l` names: the three criteria of identity

The three forms below differ only in the `labels` argument and in how
`region(l)` is defined. Each was written as a golden case and run.

### 3.A The footprint (option A of the note)

```
let footprint = initially(lcc(until(tt, lesion)))
let region(l) = and(eq(footprint, l), lesion)
save "persistent" exists(l, footprint, and(region(l), diamond(region(l))))
```

`until(tt, lesion)` is "eventually lesion": its unrolling is the disjunction
of the lesion over the frames from the current one on. `lcc` of that is one
labelling for the whole series; `region(l)` at `t` is the part of footprint
component `l` present at `t`. Consistent by construction: there is a single
numbering.

Two details of the form:

- **`tt`, not `true`.** In VoxLogicA 1 `true` is a scalar boolean and
  `and(true, image)` is a type error; `tt` is the image that holds everywhere.
  The first real run is what surfaced this.
- **`initially` is not optional.** Without it, `footprint` under `diamond`
  becomes `lcc(until(tt, lesion))` *from frame `t+1` on*: a different union,
  a different `lcc`, a different numbering, and `eq(footprint, l)` at `t` and
  at `t+1` compare labels of two labellings. This is the "deterministic is not
  stable" trap of the note, reappearing inside the very form meant to avoid
  it. `initially` pins the footprint to frame 0 (section 4). With it, pass 3
  emits a single `lcc` over `lesion@0 | ... | lesion@N` (`op26` in 2.4).

What it cannot see: two lesions that merge have one footprint from the start,
so at frame 0 "region 1" is already both of them and no lesion lies outside
it; two different lesions occupying the same place at different times
collapse into one. On the merge series `merge-footprint` gives false
everywhere, as expected. It is the zero-cost baseline (one `lcc` per series,
no propagation) and the comparison that shows what B buys.

### 3.B0 The labels of the first frame, without propagation

```
let labels = initially(lcc(lesion))
let region(l) = eq(labels, l)
```

This is `tracked(lesion, l)` at frame 0 exactly, and it is expressible
without `tracked`. It tells the lesions of frame 0 apart, so a merge between
frames 0 and 1 is visible:

```
let merged(l) = and(through(region(l), diamond(lesion)),
                    through(and(lesion, not(region(l))), diamond(lesion)))
```

reads "the component of the lesion at `t+1` that touches region `l` at `t`
and also touches, at `t`, a lesion outside region `l`". `merge-initially`
gives the merged lesion of frame 1, as expected. It says nothing about any
frame after 1, because `region(l)` is only defined at frame 0.

### 3.B Propagation: `tracked(phi, l)`

```
tracked(phi, l) at 0    = eq(lcc(phi at 0), l)
tracked(phi, l) at t+1  = through(tracked(phi, l) at t, phi at t+1)
```

"What at `t+1` lies in a component of `phi` that touches region `l` at `t`".
The identity is overlap between consecutive frames, stated in the formula
rather than assumed of `lcc`. Merges and splits come out as they are: a
merged component carries every label it touched, a split component keeps its
one label on both halves; the regions stop being a partition.

**Why it is compiled differently from `until`.** `until`'s recursion is
relative to the current frame (`n`, `inc(n)`), so pass 1 can unroll it into
declarations that are valid at any `n`. `tracked`'s recursion is anchored at
frame 0 and its value at `n` depends on the whole history `0..n`; a
declaration parametrised on `n` cannot express "n steps". So pass 1 writes
the chain out with **literal frames**, one step per frame of the video, and
defers the choice to a `select` that pass 3 resolves by the value of `n`,
as it resolves `inc`. From `merge-tracked`, 3 frames:

```
let op14(n)=eq(lcc(op6(0)),op7(n))          -- step 0: region 1 at frame 0
let op15(n)=through(op14(n),op6(1))         -- step 1
let op16(n)=through(op15(n),op6(2))         -- step 2
let op17(n)=select(n,op14(n),op15(n),op16(n))
```

and in pass 3, for `diamond(diamond(...))`, that is `select(2, ...)`:

```
let op9 = lcc(op8)                  -- lcc of the lesion at frame 0
let op11 = eq(op9,op10)             -- region 1 at frame 0
let op15 = through(op11,op14)       -- at frame 1
let op20 = through(op15,op19)       -- at frame 2
let op21 = op20                     -- select(2, ...)
```

which is the semantics written out. Under an `exists`, `l` is the parameter
`op0(n,l)` in step 0 and the chain is instantiated once per label in pass 2,
with `lcc(phi at 0)` shared between them.

**Past the end.** A `select` whose index is past the last step clamps to the
last one, and the run warns (`tracked-overflow`). This is what the semantics
gives anyway: the last step is a union of components of the last frame, and
`through` with that frame returns it unchanged — the same "the last frame
persists" convention pass 3 applies to frames.

**New components.** A component at `t+1` that touches nothing at `t` belongs
to no region. No fresh labels are minted; for lesion tracking none are needed,
because "new lesion" is the complement:

```
and(lesion, not(exists(l, initially(lcc(lesion)), tracked(lesion, l))))
```

and the labelling the existential ranges over is that of frame 0, which the
probe measures.

**What only B sees.** On the merge series, `merge-tracked` asks for
`and(tracked(lesion,1), tracked(lesion,2))` at frame 2: what descends from
*both* lesions of frame 0, through the merged frame 1, to the shifted lesion
of frame 2. Under A the two regions never overlap; under B0 neither; under B
the result is the whole lesion of frame 2. All three cases were run and match
the images drawn by hand in `tests/spatiotemporal/frames/draw.py`.

### 3.0 The implicit existential: `through`

Worth keeping in mind, because it is free. `a ~> b` (`through(b, a)`) holds
at `x` when `x` lies in a component of `a` that meets `b`: an existential over
components, bound to the point of evaluation, with no `K`. "This voxel is in a
lesion at `t+1` whose component overlapped a lesion at `t`" is
`through(lesion, diamond(lesion))` — persistence per component, no quantifier.
Section 3.B is built out of it: `tracked` is `through` iterated, and the
quantifier is needed only when the label has to be **reused** — compared with
another one, or followed through more than one step and then asked about.

## 4. The absolute frame: `initially(phi)`

`diamond` and `until` only move relative to `n`. `initially(phi)` is `phi` at
frame 0 whatever stands around it: in pass 1, `opK(0, l)` in place of
`opK(n, l)`; pass 2 then memoises every instance into one. It is a one-line
branch of the reducer, and it is what both A and B0 need to make their
labelling frame-independent. `initially-nested` pins it on its own:
`diamond(and(diamond(initially(a)), initially(diamond(a))))` flattens to
`and(a@0, a@1)`.

`tracked` does not use `initially` (its chain has literal frames of its own),
but it is of the same kind: the first piece of absolute-frame machinery the
compiler acquired.

## 5. The command line, and what runs what

| flag | pass | meaning |
|---|---|---|
| `--providecontext n` | 1 | the frame reference |
| `--numframes N` | 1, 2, 3 | length of the video; horizon of `until`, length of `tracked`'s chain |
| `--maxlabels K` | 1 | the bound the existentials unroll to; asked for only if something is quantified |
| `--probe` | 1 | replace the goals by the highest label of each labelling, per frame |

- `tests/spatiotemporal/run.sh` — goldens on the text of all three passes,
  plus the probe chain for quantified cases (third column of `cases.txt` is
  `K`).
- `tests/spatiotemporal/cli.sh` — the flags: `--maxlabels` missing or 0,
  `--probe` without a bound, the check being printed.
- `tests/spatiotemporal/semantics.sh` — the real run: probe, bound from the
  probe, flatten, VoxLogicA 1 (fetched by `vl1.sh`), check the print,
  compare the saved image with the drawn expectation by another VoxLogicA 1
  program. Two facts of VoxLogicA 1 it needed: `stdlib2.imgql` has to sit
  beside the program (the release ships `stdlib.imgql`), and numbers print
  as `2.0`.

## 6. Costs, in one place

Per frame, the flattened program has one operation per *distinct* spatial
subterm (see `spatio-temporal.md`, *What it costs*). The existential
multiplies by K only the subterms that depend on the bound variable; nested
existentials multiply by K per variable on the subterms that depend on each.
`tracked` adds one `lcc` at frame 0 and one `through` per later frame, per
label. The footprint adds one `lcc` per series. The probe costs the
labellings once more, in a separate run.

## 7. Open, and known

- The steps of a `tracked` chain that no `select` picks, and the `bounded`
  aliases, are left as declarations nothing refers to. VoxLogicA 1 evaluates
  from its goals, so they cost nothing; a reachability sweep in pass 3 would
  tidy the output, at the price of touching every golden.
- Comparing two labels (`l ≠ k`) inside a formula has no operator yet. It
  would be a pseudo-operator on two literals, folded to `tt`/`ff` in pass 3.
  The merge formula of 3.B0 avoids it by phrasing "outside region `l`" as
  `and(lesion, not(region(l)))`.
- Fresh labels for components born after frame 0: not minted, by decision;
  definable as the complement above.
- A labelling that depends on an outer label variable cannot be probed; the
  bound is given by hand and still checked.
