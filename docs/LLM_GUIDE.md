# LLM Guide to the Music Notation Grammar

A practical, code-grounded manual for writing grammar rules in this repository.
It is written for an LLM agent (or careful human) whose job is to **add or change a
notation grammar rule** and to verify it with the build/test loop.

Scope: the **current** state of the project — the MEI-based bidirectional DCG and the
ninja test pipeline. Historical paths (MusicXML, DVC, GNU Make, the old `docs/`) and
the ML/COCO dataset side are intentionally excluded.

---

## 1. One-screen mental model

This is an *executable specification of modern music notation*, written as a single
bidirectional Prolog DCG. The **same** grammar runs in two directions:

| Direction | What it does | Entry point | Reads | Writes |
|---|---|---|---|---|
| **Typesetting** (generation) | MEI semantics → graphics | `mainGen` | `data/<stem>-ids.mei` | `data/<stem>-music.pl` |
| **Recognition** (OMR) | graphics → MEI semantics | `mainReco` | `data/<stem>-verovio-noscope.pl` + `settings/<stem>-test.txt` | `data/<stem>-music.mei`, `settings/<stem>-reco.txt` |
| **Calibration** | learn thresholds from labeled data | `mainTest` | `-ids.mei` + `-verovio.pl` | `settings/<stem>-test.txt` |

The trick that makes one grammar run both ways:

- **Semantics** are MEI documents represented as Prolog terms
  `element(Tag, [Attr=Val, ...], Children)` (plus `pi(...)` XML header terms).
- **Graphics** are a *list* of two primitive kinds, each tagged with a scope-path of
  `Id-Class` pairs:
  - `seg(Start, End, Etiqs, Thickness)` — a stroked line (staff lines, stems, beams, barlines);
  - `ccx(LeftTop, RightBottom, Etiqs, Origin)` — a closed glyph bounding box (noteheads, clefs, time signature digits, accidentals, rests, dynamics), where `Origin` is the glyph anchor point.
- Both primitive kinds are **unordered** in the list. The grammar consumes them in any
  order; this is what makes recognition robust to the fact that an SVG has no reading order.
- All arithmetic is **pure** via `library(clpBNR)` (`{ ... }` constraints over reals) and
  `library(delay)` coroutining (`delay`/`when`/`freeze`), so a relationship like
  "notehead vertical position = pitch" works whether the position or the pitch is the input.
- Every DCG rule threads a two-element state list `[state(Tree), Struct]`, where
  `state(Tree)` is an RB-tree holding semantic state + all settings, and `Struct` is the
  primitive list being generated or consumed.

The heart of the grammar is `mei//2` in `music.pl`. Everything else exists to serve it.

---

## 2. The two worlds and the bridge between them

### 2.1 Semantics: MEI as Prolog terms

Input MEI files are loaded with `load_xml(..., [space(remove), number(integer)])`
(`music.pl:load_mei`), producing a tree of `element/3` and `pi/3` terms. Grammar rules
destructure these directly. Example (`music.pl:624`):

```prolog
clef(element(clef, ['xml:id'=IdDef, shape=Shape, line=LineAtom], []), _Id) -->
  ...
```

Notice: `xml:id` is bound to a fresh logical variable, `add_id(IdDef)` ensures the
generated id is unique, and string-valued attributes (`line='2'`) are converted to
numbers lazily via `delay(atom_number(LineAtom, Line))` inside `{ ... }` so the same
code reads an atom (reco) or writes one (gen).

### 2.2 Graphics: `seg` and `ccx`

`seg.pl` and `ccx.pl` define the two primitives and a full set of **bidirectional
accessors** that work on either concrete numbers or unbound CLP variables:

- `seg`: `segStart/segEnd/segEtiqs/segThickness/segStartEndThickness`, `segLength/2`,
  `segNormal/2`, `segHV/5`+`segHVCoeff/5` (sample a point on the segment at h/v
  coefficient, thickness-aware → used for stem/beam/barline edges),
  `segCorners/5`, `segTop/Bottom/Left/Right`, `segHeight/Width`.
- `ccx`: `ccxEtiqs/ccxOrigin/ccxLeftTop/ccxRightBottom/ccxLeft/ccxRight/ccxTop/ccxBottom/
  ccxWidth/ccxHeight/ccxCenterX`.

### 2.3 The bridge: `Etiqs` scope paths

The junction is the `Etiqs` scope label: a list like
`[Id-Class, ..., "1"-page]` running from innermost glyph to the page. Because:
- Verovio is run with the stable-id pass on MEI **before** engraving, each glyph's SVG
  `id` matches the MEI `xml:id` of the element that produced it (`data/<stem>-ids.mei`);
- `svg2pl.py` turns each SVG node into a primitive whose `Etiqs` records that id + CSS class;

…recognition can know *which* MEI element a box came from by matching ids, and the grammar
can walk the scope stack to enforce that the MEI nesting matches the SVG nesting.
Scopes are managed by `state:scope/scope/1,2` (push/pop on the `scope` state key) and are
enforced when consuming primitives (`epf_geo:in_scope`).

---

## 3. The threaded state

Every DCG rule keeps `[state(Tree), Struct]`. The `state` mini-language (`state.pl`) reads
and writes the RB-tree. Inside `Music/applied` rules it is used through the `o/+/−/[...]`
operators and, most importantly, through **`statep//2`**.

### `statep(Goal, KeyValues)`

`statep//2` (`state.pl:104-110`) is *the* idiom for triggering a geometric condition
predicate with the values it needs from the state:

```prolog
statep(clefCond(Shape, Line, Clef),
       [o(stafflines), -(anchor), +(pitchAnchor), o(clefSettings),
        o(clefLeftMargin), o(clefRightMargin), o(unit), o(eps)])
```

It evaluates each state op, collects the resulting values into a list, *appends them as
extra trailing arguments* to the goal, and calls it. So the above is equivalent to:

```prolog
clefCond(Shape, Line, Clef, StaffLines, Anchor, NewAnchor, PitchAnchor,
         ClefSettings, ClefLeftMargin, ClefRightMargin, Unit, Eps)
```

- `o(Key)` → read current value of `Key`;
- `o(Key, Default)` → read, or `Default` if absent;
- `+(Key, Val)` → insert/set `Key` to `Val`;
- `-(Key, Old, New)` → update `Key`, returning both old and new (used to thread values
  through a rule, e.g. updating the anchor as glyphs are laid out);
- `[Key]`/`[Val]:Key` → pop/push a stack stored under `Key`.

`stateg//2` (`state.pl:114`) is the same but defers the actual call so it runs in DCG
position.

This means: **a condition predicate never reads the state itself.** It receives its
inputs as plain arguments. That keeps it a pure, bidirectional relation.

### The scope/bbox/contour guards

When you consume a primitive inside a nested MEI element you must constrain it to the
current scope and bounding box, and fold it into the running contour union. The
`epf_geo` layer does this for you via `termp` / `selectp`.

---

## 4. Backtracking: where it is, and only there

This is the single most important discipline for writing a correct rule. The project
*avoids* deep search-backtracking almost everywhere, deliberately:

- **Plain search-backtracking is used for exactly one thing: choosing *which* graphical
  primitive satisfies an element.** That is `term//1` / `select//1` (and their geometric
  wrappers `termp//1` / `selectp//1`) in **consumption (recognition)** mode
  (`epf.pl:26-39`):

  ```prolog
  term_(Mode, X, [CurX | L], [CurX | R]) --> term_(Mode, X, L, R).
  ```

  This "skip and recurse" walks the unordered `Struct` list until `CurX` unifies with the
  sought element `X`. The searched object is **fixed** (it is the predicate argument) and
  the search is over *positions* in the list. This is the README's famous `term//1`
  exhaustive search, made concrete in `epf.pl`.

- **`find(Goal, Arg)` is a different, complementary mechanism** (`epf_geo.pl:74-82`).
  Here the searched object is a **logical variable** `Arg`. `find` enumerates candidate
  primitives **once each** and runs the grammar `at most once` per candidate:

  ```prolog
  find(Goal, Arg) -->
    state(o(cursor, Cursor)),
    find_(Cursor, Goal, Arg).
  find_(cursor(_), Goal, Arg) -->
    call(Goal, Arg).                 % primitive already preselected by cursor
  find_(noEl, Goal, Arg) -->
    term(Term),                      % enumerate: pull the next primitive
    state(+(cursor, cursor(Term))),  % remember it in the cursor
    call(Goal, Arg).                 % run grammar once for this candidate
  ```

  So the search structure is flipped versus `term`: instead of a fixed element searching
  over positions (grammar fixed, search inside), `find` *enumerates the candidates as the
  outer loop* and re-runs the grammar per candidate. This is why an element-type
  disjunction is only *shallow*:

  ```prolog
  layerChild(Child) --> ( scope(note(Child)) ; scope(rest(Child)) ).
  ```

  Invoked via `find(music:layerChild)` (as in `layer//3`, `music.pl:687`) and, through the
  cursor protocol, the primitive this child must explain is already chosen/anchored when
  `layerChild` runs. The OR is therefore dispatched by *which glyph is at the cursor*, not
  by re-searching the primitive list. The failing branch dies immediately.

  **This changes answer ordering**, which matters for OMR output stability: `find` yields
  answers in primitive-list order and at most one grammar run per candidate, whereas
  running the same DCG bare would interleave positions and elements and give a different
  order. Prefer `find` for "for each semantic child, grab the matching graphic" patterns.

### Everything else is made deterministic

| Mechanism | Where | Effect |
|---|---|---|
| Cut in `term_` gen/chk branch | `epf.pl:28` | In **generation** (`var(CurX),var(L)`) the if-then-else `->`+`!` commits: the head primitive is *invented*, not searched. |
| Soft-cut reification | `utils.pl:153-165` (`reify*/2`) | Runs a candidate once, **commits to its first success**, returns a `true`/`false` flag. |
| Greedy loops on reify | `longuest_*`, `optional`, `epf.pl:116-253` | Each step is reified and committed; `nCond(PredN,_)` counters prevent re-eating / infinite loops. |
| Cuts in `sequence2` | `epf.pl:131-134, 142-144` | Pairwise relations are committed per step. |
| Index-by-constraint, not enumeration | `nth0u/nth1u`, `utils.pl:114-119` | `if_` + integer CLP finds *the* index; never backtracks over positions. |
| Instantiation-mode dispatch | `var(Childs) -> ... ; ...`, e.g. `measureChilds` `music.pl:307-310` | Splits generation vs recognition by boundness, not clause search. |
| Coroutining | `when`/`freeze`/`delay` + `delay:mode` | Defers a goal until its operands are ground, forcing the *data direction*, not exploration. |

### Golden rule for rule-writers

> Write each `XxxCond` predicate and each DCG rule so it is **deterministic given a
> committed element choice** — it should settle on exactly one geometric arrangement.
> The *only* nondeterminism you may rely on is **"which primitive"**, via
> `term`/`select`/`termp`/`selectp` (and the outer-enumeration `find`). Greedy nesting,
> optionality, indexing and arithmetic must go through the provided combinators
> (`sequence*`, `longuest_*`, `optional`, `reify`) and pure direction-independent
> constraints — never through multi-clause exploration.

Don'ts: no bare `is/2` (not bidirectional); no index lookup by enumerating positions; do
not rely on deep backtracking to find a solution inside a `Cond`; keep element-alternative
clauses together and let `find`/the cursor decide; put the simplest/most-likely element
alternative first when order is observable.

---

## 5. Anatomy of a grammar rule (the template)

Every semantic element is handled by a DCG rule made of three parts, plus a pure
`XxxCond` predicate. The canonical reference example is the **clef** (`music.pl:597-631`).

### 5.1 The pure condition predicate

```prolog
delay:mode(music:clefCond(ground, _, _, _)).
delay:mode(music:clefCond(_, ground, ground, _)).
clefCond(gClef, 'G', 2, '4').
clefCond(fClef, 'F', 4, '3').

clefCond(Shape, N, Clef, StaffLines, Anchor, NewAnchor, Pitch-N,
         AllSettings, LeftMargin, RightMargin, Unit, Eps) :-
  etiqsCond(Clef, Etiq),                              % read the glyph's scope id/class
  freeze(Etiq, memberchk(Etiq-[Width,Height,XOffset,YOffset], AllSettings)), % its size settings
  delay(clefCond(Etiq, Shape, N, Octave)),            % id -> (shape,line,octave)
  delay(downcase_atom(Shape, PName)),                 % shape -> pitch name
  ccxOrigin(Clef, point(X, Y)),
  eps(Eps, Anchor + Unit * LeftMargin, X),            % horizontal anchor
  length(StaffLines, NumLines),
  { Index == NumLines - N + 1 },
  freeze(Index, nth1(Index, StaffLines, Line)),       % line index in the staff
  segYAtX(Line, SegY, X),
  eps(Eps, SegY, Y),                                  % glyph origin sits on that staff line
  ccxWidthHeightCond(Clef, Width, Height, Unit, Eps),
  ccxLeft(Clef, Left),
  eps(Eps, Left + XOffset*Unit, X),                   % width/offset set anchor
  ccxTop(Clef, Top),
  eps(Eps, Top + YOffset*Unit, Y),
  ccxRight(Clef, ClefRight),
  eps(Eps, ClefRight + RightMargin*Unit, NewAnchor),  % advance the layout anchor
  pitch_octave_pname(Pitch, Octave, PName).
```

Notes on the condition:
- It is a **pure relation**: every numeric step is a CLP(BNR) `{ ... }` or a delayed
  (`delay`/`freeze`) goal, so it reads the same whether `Clef` is grounded (reco) or
  unbound (gen).
- `delay:mode/1` declarations tell the coroutining when it may still fire. Add them for
  your condition with the right ground/`_`/`nonvar` patterns.
- `eps(Group, A, B)` (from `geo.pl`) is the tolerance comparison `{abs(A-B) =< Eps}`. It is
  the universal slack in this grammar.
- Constants like glyph sizes and offsets **never** appear literally here — they come from
  the `-Settings` lists, which are built from `music_settings` (see §6).

### 5.2 The DCG rule

```prolog
clef(element(clef, ['xml:id'=IdDef, shape=Shape, line=LineAtom], []), _Id) -->
  add_id(IdDef),                                      % 1. claim the semantic element's id
  { delay(atom_number(LineAtom, Line)) },             %    lazily parse the attr
  statep(clefCond(Shape, Line, Clef),                 % 2. state the relation, feeding state in
         [o(stafflines), -(anchor), +(pitchAnchor), o(clefSettings),
          o(clefLeftMargin), o(clefRightMargin),
          o(unit), o(eps)]),                          %    (reads: lines, anchor(old)->new,
  termp(Clef).                                        %     settings, margins, unit, eps)
```                                                     % 3. consume the graphic primitive

The three-part shape is consistent everywhere:

1. **`add_id(IdDef)`** — bind & de-duplicate the MEI `xml:id` (even for elements with no
   glyph, like `keySig`).
2. **`statep(_Cond, [_o(Key) ...])`** — invoke the pure condition with the state values it
   needs. Any `-(Old,New)` keys are the "current position" that the element advances.
3. **`termp(Term)` / `selectp(Term)` / nested grammar** — consume (remove from struct)
   or select (keep, for later rules) the one or more primitives that realize this element.

A **multi-glyph** element shows part 3 expanding into a sub-grammar — the time signature
(`music.pl:641-683`):
```prolog
meterSig(element(meterSig, ['xml:id'=IdDef, count=Count, unit=Unit], []), _Id) -->
  add_id(IdDef),
  statep(meterSigMarginCond(Box),
         [-(anchor), o(timeSigLeftMargin), o(timeSigRightMargin), o(unit)]),
  contour(meterSig_(Count, Unit), Box).               % bounding box of the two digits...

meterSig_(Count, Unit) -->
  state(o(staffN, StaffN)),
  statep(meterSigCond(2, Count, MeterSigUp, Center), [...]),  termp(MeterSigUp),
  statep(meterSigCond(4, Unit, MeterSigDown, Center), [...]), termp(MeterSigDown).
```

Note `contour(Grammar, Box)` (from `state.pl`) wraps a sub-grammar and captures the
bounding box its primitives occupy — used when a parent element's geometry depends on the
union of its children (here, the time signature's total width anchors layout).

---

## 6. Settings: every threshold is a setting (never hardcode)

`music_settings.pl` declares ~50 tunables via SWI `library(settings)` as a **clpBNR
`Domain-Value` pair**:

```prolog
:- setting(music:noteheadBlackWidth, pair, real(0,10)-2.5, 'Width ... in units of MEI unit').
:- setting(music:eps,               pair, real(0,5)-0.5,   'Global epsilon (pixels)').
```

Two access modes (`get_settings/3`):
- `value` — the stored midpoint, used by generation (`mainGen`).
- `domain` — a *fresh CLP interval variable* within `real(lo,hi)`, used by recognition so
  the solved constraints can narrow it and thereby *learn* the threshold.

Per-glyph dimension settings are grouped into `-Settings` lists
(`group_settings/2`, `music_settings.pl`): `clefSettings`, `timeSigSettings`,
`noteheadSettings`, `restSettings`, `accidentalSettings`, `stemSettings`, `flagSettings`,
`dynamSettings`. Each maps a glyph etiq to `[Width,Height,XOffset,YOffset]`. Your `Cond`
reads the right one with:

```prolog
freeze(Etiq, memberchk(Etiq-[Width,Height,XOffset,YOffset], NoteHeadSettings)),
```

After recognition `update_settings/1` minimizes the global `eps` and writes the narrowed
intervals to `settings/<stem>-reco.txt`.

**Consequence:** when you add a notation element with *any* geometric constant (a glyph
size, an offset, a margin), add `setting/4` entries to `music_settings.pl`, wire them into
the appropriate `-Settings` group, and pull them through `statep` with `o(Key)` — never a
literal number in the grammar body.

---

## 7. Recipe: add a new notation element / glyph

Follow these steps in order. The win/lose check is always the ninja `gen`/`reco` loop (§8).

1. **Write the semantics input** — a `data/<stem>-in.mei` file, or better, extend
   `gen_beam.pl` to emit a family of them (that is how the 27 beam cases are mass-produced).
2. **Add a `setting` (and draw the glyph's real geometry)** if the element has a glyph:
   declare the width/height/offset ranges in `music_settings.pl` and add the etiq to the
   matching `-Settings` group.
3. **Write the pure `XxxCond`** predicate (see §5.1): the bidirectional relation between
   the MEI attrs and the primitive geometry. Add the needed `delay:mode` declarations.
4. **Write the DCG rule** (`Xxx` clause): `add_id` → `statep(Cond, [...])` → `termp`/
   `selectp`/sub-grammar. Insert it in the right place of the grammar hierarchy
   (`music.pl`: `mei → music → body → score → scoreDef → section → page → system →
   measure → staff → layer → note/rest/...`).
   - If the element is a *kind of layer child* (a new glyph between notes), add it as a
     branch in `layerChild` so `find` and the cursor select it (`music.pl:689`).
   - If it introduces a new *flavor of measure child* (staff / dynam / beamSpan), extend
     `measureChilds` (both `measureChildsGen` and `measureChildsReco`, `music.pl:306-343`)
     and keep its unit test honest.
   - If it spans a staff vertically (e.g. a dynamic, a mounted symbol), think about
     `vertical_layout` / `contour` and the per-staff anchoring.
5. **Regenerate and run the loop** (§8). In `gen` mode you should get a grounded
   `data/<stem>-music.pl`; in `reco` mode a normalized `settings/<stem>-test.txt` and a
   reconstructed `data/<stem>-music.mei`. If reco diverges, the `Cond` is over-/under-
   constrained: loosen an `eps` or widen a setting domain.

---

## 8. Build & test loop (ninja)

The pipeline is driven by a generated `build.ninja` produced from `ninja.pl` (via the
`library(ninja)` pack). Run **from the repo root**:

```sh
# regenerate build.ninja after editing ninja.pl (a `generate` edge runs
# `swipl -g main` on ninja.pl, the library(ninja) generator pattern):
ninja build.ninja

# built-all phonies:
ninja all      # everything (gen + reco + test for every stem)
ninja svgs     # just the MEI -> SVG engraving step
ninja test     # calibration settings for all stems

# per-stem, e.g. a beam case:
ninja data/beam-2-16th-16th-16th-music.pl   # generation ground truth
ninja data/beam-2-16th-16th-16th-music.mei   # recognition output (needs test settings first)
ninja settings/beam-2-16th-16th-16th-test.txt
```

Per-stem file chain (also see `build.ninja` edges):

```
<stem>-in.mei            # (hand / gen_beam input)
  |  mei_add_ids (verovio) — add stable xml:id, bump header to 6.0-dev
  v
<stem>-ids.mei
  |  mei2svg (verovio)  — engrave to SVG
  v
<stem>-verovio.svg
  |  svg_add_ids (inkscape) — merge ids into SVG
  v
<stem>-verovio_ids.svg
  |  svg2pl (svg2pl.py) — SVG -> primitives (keeps ids + classes)
  v
<stem>-verovio.pl
  |  remove_scopes (main -- remove_scopes) — strip per-element ids ("blind" OMR)
  v
<stem>-verovio-noscope.pl
  |  test (main -- mainTest)   — calibration -> settings/<stem>-test.txt
  v
settings/<stem>-test.txt
  |  reco (main -- mainReco)   -> <stem>-music.mei + settings/<stem>-reco.txt
  |  gen  (main -- mainGen)    -> <stem>-music.pl
```

How to read the outputs:
- `<stem>-music.pl` — list of gen primitives with CLP intervals grounded to
  `real(lo,hi)` midpoints, ending with `dif/2` constraints guaranteeing distinct ids.
- `<stem>-music.mei` — reconstructed semantics as a Prolog DOM term (a list of
  `pi/3`/`element/3`). **Not a serializable XML file**: it can carry unresolved CLP
  constraint blobs for the learned geometry. `music.pl` only calls `xml_write` if the term
  is fully ground.
- `settings/<stem>-test.txt` — learned per-stem thresholds (each a `setting/2` line).

**How stems are enumerated** (`ninja.pl`): a fixed list of single-feature stems
(`stafflines`, `clef-G-2`, `accid-sharp`, `note-whole`, ...) plus the programmatically
generated `beam-2-<d1>-<d2>-<d3>` for durations in `{8,16,32}` (27 cases, emitted by
`gen_beam.pl`). To add a new test case, add it to `stem//1` and add a
`data/<stem>-in.mei`; then `ninja` regenerates everything.

---

## 9. Predicate cheat-sheet (the ones you actually use in rules)

### Consuming / selecting primitives (`epf.pl`, `epf_geo.pl`)
- `term(X)//1`, `select(X)//1`, `add(X)//1`, `update(In,Out)//2` — raw, unordered.
  `select` keeps the element for later rules; `term` removes it.
- `termp(X)//1`, `selectp(X)//1` (`epf_geo`) — the geo-guarded versions: enforce current
  **scope**, **bounding box**, and **contour** membership, and drive the **cursor**.
  Use these 99% of the time.
- `find(Goal, Arg)//2` — enumerate a candidate primitive (via cursor/`term`), run
  `Goal(Arg)` at most once per candidate. The "for each semantic child grab the matching
  graphic" combinator (§4).
- `sequence*`, `optional//3`, `longuest_*` — greedy/ordered application; deterministic
  (reified). See §4.

### State (`state.pl`)
- `statep(Goal, [keys])//2` — feed state values into a `Cond` (§3). **The** idiom.
- `stateg(Goal, [keys])//2` — same, deferred to DCG position.
- `scope(Mod:Goal)//1,2` — push/pop a scope; the nested rule runs inside it.
- `bbox(Grammar, Box)//2`, `contour(Grammar, Box)//2` — capture a sub-grammar's box/union.
- `add_id(Id)`, `ground_all_ids/1` — id uniqueness/grounding.

### Geometry (`geo.pl`, `cond.pl`) — all pure/bidirectional
- `eps(Eps, A, B)` — `{abs(A-B) =< Eps}`; `eps(p,Eps,Pa,Pb)`, `eps(px,...)`, `eps(py,...)`
  point components; `eps(Dir,Eps,PlaneA,PlaneB,Dist)` for distances.
- `above(Dist, A, B)`, `leftof(A, B)` — relative-position constraints.
- `segYAtX(Seg, Y, X)` — Y of a line segment at abscissa X (crucial for anchoring glyphs
  on staff lines).
- `inside(Term, BBox)`, `union(A, B, U)`, `intersect`, `center`, `contour/2`.
- `ccxWidthHeightCond(Ccx, W, H, Unit, Eps)` — glyph is W×H in MEI units.
- `ccxOnSegCond(Ccx, Seg, Eps)` — glyph origin lies on a segment.
- `etiqsCond(Term, Etiq)`, `etiqsCond(Term, N, Etiq)` — read scope labels.
- `centerCond(A, B, Unit, Eps)` — centers coincide (used by `meterSig`).

### Pitch (`pitch_cond.pl`)
- `pitchCond/9`, `pitchCondLine/9`, `pitchCondCcx/9` — map a notehead's vertical offset
  (in half-interline units) to a diatonic step/octave, bidirectionally.
- `numIntervals/5`, `inRange/6`.

### Misc (`utils.pl`)
- `nth0u/Nth1u` — index lookup by constraint (not enumeration).
- `chain/chaing`, `maplist2`, `convlist2` — pairwise list applications.
- `reify//2`, `reify/2` — reified success test (the engine of `longuest_*`/`optional`).
- `delay:mode/1` declarations for ISO predicates — the bidirectionality registry.

---

## 10. Guardrails & pitfalls (read before editing)

- **Never hardcode.** No literal pixel sizes, offsets, margins, or margins-of-thumb in the
  grammar. Everything numeric is a setting or a CLP relation; relative positions are
  expressed as `Unit`-scaled offsets with `eps` tolerance.
- **No impure arithmetic.** Avoid `is/2`, `atom_number/2` etc. in a rule body unless wrapped
  for bidirectionality (`delay(atom_number(A, N))`, plus a `delay:mode` declaration).
- **Prefer `termp`/`selectp` over raw `term`/`select`.** They maintain the scope/bbox/
  contour and cursor invariants that make recovery tractable.
- **`selectp` when you'll revisit the primitive** (e.g. ledger lines are selected from a
  surrounding rules); **`termp` when it is exclusively owned** by this element.
- **Match MEI nesting with scope nesting.** If your element is a descendant, wrap it in
  `scope(Mod:Goal)` so its primitives are required to carry the ancestor id in their etiqs.
- **Keep `Cond` predicates deterministically satisfiable** for a given element — the search
  is "which primitive", not "backtrack to fix my over-constrained relation" (§4).
- **`mainGen` output must be ground-able.** If generation leaves an unbound var in a
  primitive, `ground_elem/2` will turn it into a midpoint; ungroundable vars signal a
  missing constraint.
- **Regenerate after touching `music.pl`/module deps.** The ninja edges list transitive
  Prolog module dependencies, so a rebuild picks up edits without manual `--up-to-date`
  fights — but you must be running from the repo root with the machine's toolchain.
- **The `.mei` "outputs" are Prolog terms, not XML.** If you need real XML, the term must
  be ground; otherwise it stays a constraint-annotated DOM. Do not expect `xsltproc`/
  `xmllint` to consume `*-music.mei` directly.

---

## 11. File map (current state)

| File | Role |
|---|---|
| `music.pl` | The grammar: `mei//1` + all element DCGs and their `*Cond` predicates; `main`/`mainGen`/`mainReco`/`mainTest` CLI. **The file to read and mostly edit.** |
| `note.pl` | A parallel, self-contained note DCG reusing the same lower layers. Not wired into the active `music.pl` pipeline. |
| `music_settings.pl` | All tunable thresholds as `setting/4` pairs; `get_settings/3`, `update_settings/1`, `group_settings/2`. |
| `state.pl` | Threaded RB-tree state, `o/+/−/[]` ops, `statep`/`stateg`, `scope`/`bbox`/`contour`, id handling. |
| `epf.pl` | Primitive store: `term`/`select`/`add`, `sequence*`, `longuest_*`, `optional`, reifd combinators. |
| `epf_geo.pl` | Geo-wrapped consumption: `termp`/`selectp`, `find`, cursor, `multi_seg`, vertical layout, in-scope/bbox/contour guards. |
| `geo.pl` | Pure spatial relations: `eps`, `inside`, `above`, `leftof`, `segYAtX`, `contour`, `center`, `union`, grounding. |
| `seg.pl`, `ccx.pl` | The two primitives and their accessors. |
| `cond.pl` | `etiqsCond`, `ccxWidthHeightCond`, `ccxOnSegCond`, `centerCond`. |
| `pitch_cond.pl` | Pitch <-> vertical-offset mapping. |
| `music_utils.pl` | Staff interline helpers. |
| `utils.pl` | `delay:mode` registry, list mappers, indices-by-constraint, `reify`. |
| `mei.pl` | MEI attribute value-type conversion helpers. |
| `gen_beam.pl` | Emits the `beam-2-<d1>-<d2>-<d3>` MEI test family. |
| `ninja.pl` | Generator for `build.ninja`; the test pipeline and `stem//1` enumeration. |
| `svg2pl.py` | Verovio SVG → Prolog primitive list (prolog mode). |
| `data/` | Input `*-in.mei` (tracked) + generated `*-ids.mei`, `*-verovio*.pl/svg`, `*-music.pl`, `*-music.mei` (mostly gitignored/regenerated). |
| `settings/` | `default.txt`, per-stem `*-test.txt` (tracked) and `*-reco.txt` (regenerated). |