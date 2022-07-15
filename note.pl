:- module(note, [note//1]).

:- use_module(library(clpBNR)).
:- use_module(library(delay)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(ccx).
:- use_module(seg).
:- use_module(geo).
:- use_module(cond).
:- use_module(epf).
:- use_module(epf_geo).
:- use_module(utils).
:- use_module(music_utils).
:- use_module(constraint_math).

:- multifile delay:mode/1.

:- use_module(library(settings)).

:- setting(stemEW_eps, number, 0.05, 'stem up/down horizontal note jonction threshold').
:- setting(stemUp, list(pair), [
  'noteheadHalf'-0.16,
  'noteheadBlack'-0.16
  ], 'stem up vertical note jonction').
:- setting(stemDown, list(pair), [
  'noteheadHalf'-(-0.14),
  'noteheadBlack'-(-0.16)
  ], 'stem down vertical note jonction').
:- setting(stemNS_eps, number, 0.01, 'stem up/down vertical note jonction threshold').

durationCond(DurationAtom, Div, NoteheadDuration, DotsDuration) :-
  delay(atom_number(DurationAtom, Duration)),
  [Duration, Div, NoteheadDuration]::integer(1, _),
  DotsDuration::integer(0, _),
  N::integer(0, 20),
  {
    NoteheadDuration == 2 ** N,
    DotsDuration < NoteheadDuration,
    Duration == NoteheadDuration + DotsDuration
  },
  debug(note, "durationCond: Duration ~p~n", [Duration]),
  debug(note, "durationCond: NoteheadDuration ~p~n", [NoteheadDuration]),
  debug(note, "durationCond: DotsDuration ~p~n", [DotsDuration]),
  debug(note, "durationCond: Div ~p~n", [Div]).

types(['1024th', '512th', '256th', '128th', '64th', '32nd', '16th',
       'eighth', 'quarter', 'half', 'whole', 'breve']).
types(N, T) :-
  types(Types),
  nth0(N, Types, T).
typeCond(Type, Duration, Div) :-
  types(Types),
  length(Types, NumTypes),
  nth0u(Q, Types, 'quarter'),
  delay(nth0u(N, Types, Type)),
  [DivN, N]::integer(0, NumTypes),
  {
    Div == 2 ** DivN,
    Duration == 2 ** (N - Q + DivN)
  },
  debug(note, "typeCond: Duration ~p~n", [Duration]),
  debug(note, "typeCond: Q ~p~n", [Q]),
  debug(note, "typeCond: N ~p~n", [N]).

noteheads(['noteheadBlack', 'noteheadHalf', 'noteheadWhole', 'noteheadDoubleWhole']).

delay:mode(note:noteheadDuration(ground, _     , _)).
delay:mode(note:noteheadDuration(_     , ground, ground)).

noteheadDuration('noteheadBlack', Duration, Division) :-
  {Duration / Division =< 1}.
noteheadDuration('noteheadHalf', Duration, Division) :-
  {Duration / Division == 2}.
noteheadDuration('noteheadWhole', Duration, Division) :-
  {Duration / Division == 4}.
noteheadDuration('noteheadDoubleWhole', Duration, Division) :-
  {Duration / Division == 8}.

noteheadCond(Notehead, Duration, Division) :-
  ccxEtiqsCond(Notehead, NoteheadEtiq),
  ccxEtiqsCond(Notehead, 1, notehead),
  delay(noteheadDuration(NoteheadEtiq, Duration, Division)),
  debug(note, "noteheadCond: Notehead ~p~n", [Notehead]),
  debug(note, "noteheadCond: Duration ~p~n", [Duration]),
  debug(note, "noteheadCond: Division ~p~n", [Division]).

durationNoStem(Duration, Division) :-
  { Duration / Division >= 4 }.

noteheadStemCond(Notehead, Stem, Duration, Division, Dir) :-
  { Duration / Division =< 2 },
  stemDirCond(Dir, Notehead, Stem).

stemDirCond(Dir, Notehead, Stem) :-
  ccxLeftRight(Notehead, NoteLeft, NoteRight),
  segCorner(v, right-bottom, Stem, point(StemBottom, _)),
  segCorner(v, left-top, Stem, point(StemTop, _)),
  delay(stemDir(Dir, NoteRight, StemBottom, NoteLeft, StemTop)).

delay:mode(note:stemDir(ground , _      , _      , _      , _)).
delay:mode(note:stemDir(_      , ground , ground , _      , _)).
delay:mode(note:stemDir(_      , _      , _      , ground , ground)).

stemDir(up, NoteRight, StemBottom, _, _) :-
  diffEps(0, NoteRight, StemBottom).
stemDir(down, _, _, NoteLeft, StemTop) :-
  diffEps(0, NoteLeft, StemTop).

stemFlagCond(Stem, Flag, Duration, Division, Dir) :-
  debug(note, "stemFlagCond: ~p, ~p, ~p, ~p~n", [Stem, Flag, Duration, Dir]),
  { Duration / Division =< 1r2 },
  ccxEtiqsCond(Flag, FlagEtiq),
  flagValDir(FlagEtiq, Val, FlagDir),
  flagDuration(Val, Duration, Division),
  flagDirCond(Dir, FlagDir, Stem, Flag).

delay:mode(system:atom_concat(ground , ground , _)).
delay:mode(system:atom_concat(_      , _      , ground)).

flagValDir(Flag, Val, Dir) :-
  debug(note, "flagValDir: ~p, ~p, ~p~n", [Flag, Val, Dir]),
  delay(atom_codes(Flag, Codes)),
  delay(phrase(flagValDir(Val, Dir), Codes)).
flagValDir(Val, Dir) -->
  "flag",
  flagVal(Val),
  flagDir(Dir),
  eos.
flagVal(32) -->
  integer(32),
  "nd".
flagVal(Val) -->
  {
    N::integer(3, 10),
    Val::integer(8, 1024),
    {
      Val == 2 ** N,
      Val <> 32
    }
  },
  integer(Val),
  "th".
flagDir('Up') -->
  atom('Up').
flagDir('Down') -->
  atom('Down').

flagVals(['8th', '16th', '32nd', '64th', '128th', '256th', '512th', '1024th']).

flagDuration(FlagVal, Duration, Division) :-
  FlagVal::integer(8, 1024),
  { Duration / Division == 4 / FlagVal },
  debug(note, "flagDuration: ~p, ~p~n", [FlagVal, Duration]).

flagDirCond(Dir, FlagDir, Stem, Flag) :-
  ccxOrigin(Flag, FlagOrigin),
  segCorner(v, left-top, Stem, StemTop),
  segCorner(v, left-bottom, Stem, StemBottom),
  delay(flagDir(Dir, FlagDir, FlagOrigin, StemTop, StemBottom)).

delay:mode(note:flagDir(ground , _      , _      , _      , _     )).
delay:mode(note:flagDir(_      , ground , _      , _      , _     )).
delay:mode(note:flagDir(_      , _      , ground , ground , _     )).
delay:mode(note:flagDir(_      , _      , ground , _      , ground)).
flagDir(up, 'Dir', FlagOrigin, StemTop, _) :-
  pointDiffEps(0, FlagOrigin, StemTop).
flagDir(down, 'Down', FlagOrigin, _, StemBottom) :-
  pointDiffEps(0, FlagOrigin, StemBottom).

durationNoFlag(Duration, Division) :-
  { Duration / Division >= 1 }.

delay:mode(note:step(ground, _)).
delay:mode(note:step(_, ground)).
step(0, 'C').
step(1, 'D').
step(2, 'E').
step(3, 'F').
step(4, 'G').
step(5, 'A').
step(6, 'B').

numIntervals(N, Step1, Octave1, Step2, Octave2) :-
  N::integer(-70, 70),
  [Octave1, Octave2]::integer(0, 9),
  delay(step(P1, Step1)),
  delay(step(P2, Step2)),
  [P1, P2]::integer(0, 6),
  {
    NPitch == P2 - P1,
    N == (Octave2 - Octave1) * 7 + NPitch
  }.

notePitchCond(Notehead, Step, Octave,
              BaseLine, BaseStep, BaseOctave,
              PitchIntervals, Stafflines) :-
  ccxOrigin(Notehead, point(NoteX, NoteY)),
  interlineAtX(Stafflines, NoteX, Interline),
  debug(note, "notePitchCond Interline ~p~n", [Interline]),
  segYAtX(BaseLine, BaseLineY, NoteX),
  {
    HalfInterline == Interline / 2,
    NoteOffset == NoteY - BaseLineY,
    GraphicalIntervals == NoteOffset / HalfInterline
  },
  debug(note, "GraphicalIntervals: ~p~n", [GraphicalIntervals]),
  diffEps(0, GraphicalIntervals, PitchIntervals),
  numIntervals(PitchIntervals, Step, Octave, BaseStep, BaseOctave),
  debug(note, "PitchIntervals: ~p~n", [PitchIntervals]).

dotCond(Ref, Dot, NumIntervals, Stafflines) :-
  ccxEtiqsCond(Ref, 1, 'notehead'),
  ccxRight(Ref, NoteX),
  ccxOrigin(Ref, point(_, NoteY)),
  interlineAtX(Stafflines, NoteX, Interline),
  { PredX == NoteX + 1r2 * Interline },
  dotCondStart(NumIntervals, PredX, NoteY, Dot, Interline).
dotCond(Ref, Dot, _, Stafflines) :-
  ccxEtiqsCond(Ref, 'dots'),
  ccxOrigin(Ref, point(RefX, RefY)),
  interlineAtX(Stafflines, RefX, Interline),
  { PredX == RefX + 3r4 * Interline },
  dotCond_(point(PredX, RefY), Dot).
dotCondStart(NumIntervals, PredX, NoteY, Dot, Interline) :-
  N::integer,
  { NumIntervals == 2 * N },
  { PredY == NoteY - 1r2 * Interline },
  dotCond_(point(PredX, PredY), Dot).
dotCondStart(NumIntervals, PredX, NoteY, Dot, _) :-
  N::integer,
  { NumIntervals == 2 * N + 1 },
  dotCond_(point(PredX, NoteY), Dot).
dotCond_(Point, Dot) :-
  ccxEtiqsCond(Dot, 'dots'),
  ccxOrigin(Dot, DotCenter),
  pointDiffEps(0.01, DotCenter, Point).

dotsCond(Dots, DotsDur, Dur) :-
  delay(dotsDuration(Dots, DotsDur, Dur)).

delay:mode(note:dotsDuration(ground, _, _)).
delay:mode(note:dotsDuration(_, ground, ground)).
dotsDuration([], 0, _).
dotsDuration([_ | Dots], DotsDur, Dur) :-
  {
    DotsDur > 0,
    HalfDur == Dur / 2,
    DotsDur == NextDotsDur + HalfDur
  },
  dotsDuration(Dots, NextDotsDur, HalfDur).

note(element(note, [], [Pitch, Duration, Type | NoteAttributes])) -->
  {debug(note, "note: ~p, ~p, ~p~n", [Pitch, Duration, Type])},
  duration(Duration),
  type(Type),
  notePitch(Pitch),
  noteGraphique(NoteAttributes).

duration(element(duration, [], [DurationAtom])) -->
  state(division, duration, dots, durationCond(DurationAtom)).

type(element(type, [], [Type])) -->
  state(duration, division, typeCond(Type)).

notePitch(element(pitch, [], [Step, Octave])) -->
  {debug(note, "notePitch~n", [])},
  step(Step),
  octave(Octave),
  state(notehead, step, octave,
        baseLine, baseStep, baseOctave,
        intervals, stafflines,
        notePitchCond).

step(element(step, [], [Step])) -->
  state_selectchk(step(Step)).

octaveCond(OctaveAtom, Octave) :-
  Octave::integer(0, 9),
  delay(atom_number(OctaveAtom, Octave)).

octave(element(octave, [], [Octave])) -->
  state(octave, octaveCond(Octave)).

noteGraphique(Dots) -->
  {debug(note, "noteGraphique~n", [])},
  notehead(Dots),
  noteStem.

notehead(Dots) -->
  state(notehead(Notehead), duration, division, noteheadCond),
  termp(Notehead),
  {debug(note, "notehead: ~p~n", [Notehead])},
  dots(Notehead, Dots),
  ledgerlines.

dots(Notehead, Dots) -->
  {debug(note, "dots: In ~p~n", [Dots])},
  state(dots, duration, dotsCond(Dots)),
  sequence2(dot, [_ | Dots], [Notehead | _]),
  {debug(note, "dots: Out ~p~n", [Dots])}.

dot(_, element(dot, [], []), Ref, Dot) -->
  state(numIntervals, stafflines, dotCond(Ref, Dot)),
  termp(Dot).

ccxAboveSeg(Seg, Ccx) :-
  ccxOrigin(Ccx, point(CcxX, CcxY)),
  segYAtX(Seg, SegY, CcxX),
  { CcxY =< SegY }.

ccxBelowSeg(Seg, Ccx) :-
  ccxOrigin(Ccx, point(CcxX, CcxY)),
  segYAtX(Seg, SegY, CcxX),
  { CcxY >= SegY }.

lineOffset(X, Y, Seg, Offset) :-
  segYAtX(Seg, SegY, X),
  { Offset == Y - SegY }.

ledgerlineCond(LedgerLines, Notehead, Stafflines, Num, PitchIntervals) :-
  length(Stafflines, NumStafflines),
  NumLedgerLines::integer(0, 70),
  { NumLedgerLines =< abs(PitchIntervals) / 2 - (NumStafflines - Num) },
  { NumLedgerLines >= (abs(PitchIntervals) - 1) / 2 - (NumStafflines - Num) },
  ccxOrigin(Notehead, point(NoteX, NoteY)),
  interlineAtX(Stafflines, NoteX, Interline),
  last(Stafflines, TopLine),
  segYAtX(TopLine, TopY, NoteX),
  { NoteY =< TopY },
  NumLedgerLinesGraphique::integer(0, 70),
  { NumLedgerLinesGraphique =< (TopY - NoteY) / Interline },
  { NumLedgerLinesGraphique >= (TopY - (NoteY + Interline / 2)) / Interline },
  diffEps(0, NumLedgerLines, NumLedgerLinesGraphique),
  length(LedgerLines, NumLedgerLines),
  maplist({NoteX}/[Seg, Y]>>(segYAtX(Seg, Y, NoteX)), LedgerLines, Ys),
  numlist(1, NumLedgerLines, Coeffs),
  maplist({TopY, Interline}/[Coeff, Expr]>>({Expr == TopY - Coeff * Interline}),
          Coeffs, Exprs),
  setting(music:staff_interline_eps, Eps),
  maplist(diffEps(Eps), Ys, Exprs).
ledgerlineCond(LedgerLines, Notehead, Stafflines, Num, PitchIntervals) :-
  { NumLedgerLines =< PitchIntervals / 2 - (Num - 1) },
  { NumLedgerLines >= (PitchIntervals - 1) / 2 - (Num - 1) },
  ccxOrigin(Notehead, point(NoteX, NoteY)),
  debug(note, "ledgerlineCond below, Notehead origin point(~p, ~p)~n", [NoteX, NoteY]),
  interlineAtX(Stafflines, NoteX, Interline),
  nth0(0, Stafflines, BottomLine),
  segYAtX(BottomLine, BottomY, NoteX),
  { BottomY =< NoteY },
  NumLedgerLinesGraphique::integer(0, 70),
  { NumLedgerLinesGraphique =< (NoteY - BottomY) / Interline },
  { NumLedgerLinesGraphique >= ((NoteY - Interline / 2) - BottomY) / Interline },
  diffEps(0, NumLedgerLines, NumLedgerLinesGraphique),
  length(LedgerLines, NumLedgerLines),
  maplist({NoteX}/[Seg, Y]>>(segYAtX(Seg, Y, NoteX)), LedgerLines, Ys),
  numlist(1, NumLedgerLines, Coeffs),
  maplist({BottomY, Interline}/[Coeff, Expr]>>({Expr == BottomY + Coeff * Interline}),
          Coeffs, Exprs),
  setting(music:staff_interline_eps, Eps),
  maplist(diffEps(Eps), Ys, Exprs).
ledgerlineCond([], Notehead, Stafflines, Num, PitchIntervals) :-
  { PitchIntervals =< 7 },
  ccxOrigin(Notehead, point(NoteX, NoteY)),
  interlineAtX(Stafflines, NoteX, Interline),
  last(Stafflines, TopLine),
  segYAtX(TopLine, TopY, NoteX),
  nth1(Num, Stafflines, BaseLine),
  segYAtX(BaseLine, BaseY, NoteX),
  {
    TopY - Interline / 2 =< NoteY,
    NoteY =< BaseY
  }.
ledgerlineCond([], Notehead, Stafflines, Num, PitchIntervals) :-
  { 1 =< PitchIntervals, PitchIntervals =< 3 },
  ccxOrigin(Notehead, point(NoteX, NoteY)),
  interlineAtX(Stafflines, NoteX, Interline),
  nth0(0, Stafflines, BottomLine),
  segYAtX(BottomLine, BottomY, NoteX),
  nth1(Num, Stafflines, BaseLine),
  segYAtX(BaseLine, BaseY, NoteX),
  {
    BaseY + Interline / 2 =< NoteY,
    NoteY =< BottomY + Interline / 2
  }.

ledgerlines -->
  state(notehead, stafflines, num, intervals, ledgerlineCond(LedgerLines)),
  sequence(selectp, LedgerLines).

noteStem -->
  state(duration, division, durationNoStem),
  {debug(note, "noteStem: without stem~n", [])}.
noteStem -->
  state(notehead, stem(Stem), duration, division, dir, noteheadStemCond),
  verticalSeg(Stem),
  {debug(note, "noteStem: with stem~n", [])},
  noteFlag.
noteFlag -->
  state(stem, flag(Flag), duration, division, dir, stemFlagCond),
  termp(Flag),
  {debug(note, "noteFlag: with flag~n", [])}.
noteFlag -->
  state(duration, division, durationNoFlag),
  {debug(note, "noteFlag: without flag~n", [])}.
