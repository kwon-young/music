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
:- use_module(pitch_cond).
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
  epsGround(0.1, StemBottom, StemBottomG),
  segCorner(v, left-top, Stem, point(StemTop, _)),
  epsGround(0.1, StemTop, StemTopG),
  debug(note, "stemDirCond ~p, ~p, ~p, ~p, ~p~n", [Dir, NoteRight, StemBottom, NoteLeft, StemTop]),
  delay(stemDir(Dir, NoteRight, StemBottomG, NoteLeft, StemTopG)).

delay:mode(note:stemDir(ground , _      , _      , _      , _)).
delay:mode(note:stemDir(_      , ground , ground , _      , _)).
delay:mode(note:stemDir(_      , _      , _      , ground , ground)).

stemDir(up, NoteRight, StemBottom, _, _) :-
  diffEps(0.1, NoteRight, StemBottom).
stemDir(down, _, _, NoteLeft, StemTop) :-
  diffEps(0.1, NoteLeft, StemTop).

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

durationNoFlagBeam(Duration, Division) :-
  { Duration / Division >= 1 }.

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
  state2(+duration, +dots, +notehead, +stem, +dir, +step, +octave, +intervals, +alter),
  {debug(note, "note: ~p, ~p, ~p~n", [Pitch, Duration, Type])},
  duration(Duration),
  type(Type),
  notePitch(Pitch),
  noteGraphique(NoteAttributes).

duration(element(duration, [], [DurationAtom])) -->
  state(division, duration, dots, durationCond(DurationAtom)).

type(element(type, [], [Type])) -->
  state(duration, division, typeCond(Type)).

notePitch(element(pitch, [], Pitch)) -->
  {debug(note, "notePitch~n", [])},
  {when((ground(Alter) ; ground(Pitch)), append([[Step], Alter, [Octave]], Pitch))},
  step(Step),
  alter(Alter),
  octave(Octave),
  state(notehead, step, octave,
        baseLine, baseStep, baseOctave,
        intervals, stafflines,
        pitchCondLine).

step(element(step, [], [Step])) -->
  state2(step(Step)).

alterCond(Element, Alter) :-
  delay(alterCond_(Element, Alter)).

delay:mode(note:alterCond_(ground, _)).
delay:mode(note:alterCond_(_, ground)).

alterCond_([element(alter, [], [AlterAtom])], Alter) :-
  Alter::real(-2.0, 2.0),
  NumQuarterTones::integer(-4, 4),
  {
    Alter == 0.5 * NumQuarterTones,
    NumQuarterTones <> 0
  },
  delay(atom_number(AlterAtom, Alter)).
alterCond_([], 0).

alter(Alter) -->
  state(alter, alterCond(Alter)).

octaveCond(OctaveAtom, Octave) :-
  Octave::integer(0, 9),
  delay(atom_number(OctaveAtom, Octave)).

octave(element(octave, [], [Octave])) -->
  state(octave, octaveCond(Octave)).

delay:mode(system:append(ground, ground, _)).
delay:mode(system:append(ground, _, ground)).
delay:mode(system:append(_, ground, ground)).
noteGraphique(Attributes) -->
  {debug(note, "noteGraphique~n", [])},
  notehead(NoteheadAttributes),
  {delay(system:append(NoteheadAttributes, Stem, Attributes))},
  noteStem(Stem).

notehead(Attributes) -->
  state(notehead(Notehead), duration, division, noteheadCond),
  termp(Notehead),
  {debug(note, "notehead: ~p~n", [Notehead])},
  dots(Notehead, Dots),
  accidental(Accidental),
  {append(Dots, Accidental, Attributes)},
  ledgerlines.

dots(Notehead, Dots) -->
  {debug(note, "dots: In ~p~n", [Dots])},
  state(dots, duration, dotsCond(Dots)),
  sequence2(dot, [_ | Dots], [Notehead | _]),
  {debug(note, "dots: Out ~p~n", [Dots])}.

dot(_, element(dot, [], []), Ref, Dot) -->
  state(numIntervals, stafflines, dotCond(Ref, Dot)),
  termp(Dot).

delay:mode(note:accidName(ground, _)).
delay:mode(note:accidName(_, ground)).
accidName(accidentalFlat, flat).
accidName(accidentalSharp, sharp).
accidName(accidentalNatural, natural).

% accidAlter(flat, -1).
% accidAlter(sharp, 1).
% fifthsPitchAlter(0, _Pitch, 0).
% fifthsPitchAlter(Fifths, Pitch, Alter) :-
%   {Fifths < 0},
%   Pattern = ['B', 'E', 'A', 'D', 'G', 'C', 'F'],
%   nth1(N, Pattern, Pitch),
%   {N =< abs(Fifths)}.
% fifthsPitchAlter(Fifths, Pitch, Alter) :-
%   {Fifths > 0},
%   reverse(Pattern, ['B', 'E', 'A', 'D', 'G', 'C', 'F']),
%   nth1(N, Pattern, Pitch),
%   {N =< abs(Fifths)}.
% fifthsPitchAlter(_Fifths, _Pitch, 0) :-

% fifthsPitchAlter(-2, E, -1).

% fifthsCond(0, _Pitch, Alter, Accid) :-
%   accidAlter(Accid, Alter).
% fifthsCond(1, Pitch, Alter, Accid) :-

noteAlter(0, noKey, noAccid).
noteAlter(0, sharp, natural).
noteAlter(0, flat, natural).
noteAlter(1, sharp, noAccid).
noteAlter(1, noKey, sharp).
noteAlter(-1, flat, noAccid).
noteAlter(-1, noKey, flat).

delay:mode(note:accidAlter(ground, ground, ground, ground, _, ground, ground)).
delay:mode(note:accidAlter(_, _, ground, ground, ground, ground, ground)).
accidAlter(0, natural, Step, _, _, KeySteps, _) :-
  memberchk(Step, KeySteps).
accidAlter(0, natural, Step, Octave, AccidStepOctaves, _, _) :-
  memberchk(_-Step-Octave, AccidStepOctaves).
accidAlter(Alter, Name, Step, Octave, AccidStepOctaves, KeySteps, _) :-
  \+ memberchk(Step, KeySteps),
  accidAlter_(Alter, Name),
  \+ memberchk(Name-Step-Octave, AccidStepOctaves).

% accidAlter_(0, natural).
accidAlter_(1, sharp).
accidAlter_(-1, flat).

accidAlter2_(0, natural).
accidAlter2_(1, sharp).
accidAlter2_(-1, flat).

noAccidAlter(Alter, Step, Octave, AccidStepOctaves, KeySteps, Alter) :-
  memberchk(Step, KeySteps),
  \+ memberchk(_-Step-Octave, AccidStepOctaves).
noAccidAlter(Alter, Step, Octave, AccidStepOctaves, _, _) :-
  memberchk(Accid-Step-Octave, AccidStepOctaves),
  accidAlter2_(Alter, Accid).
noAccidAlter(0, Step, Octave, AccidStepOctaves, _, _) :-
  \+ memberchk(_-Step-Octave, AccidStepOctaves).

accidentalCond(Accidental, AccidentalName, Notehead, Step, Octave,
               Alter, KeySteps, KeyAlter,
               AccidStepsOctaves, [AccidentalName-Step-Octave | AccidStepsOctaves]) :-
  ccxEtiqsCond(Accidental, AccidentalEtiq),
  ccxEtiqsCond(Accidental, 1, accid),
  ccxOrigin(Notehead, point(NoteX, NoteY)),
  ccxOrigin(Accidental, point(AccidX, AccidY)),
  diffEps(0, NoteY, AccidY),
  { AccidX =< NoteX },
  delay(accidName(AccidentalEtiq, AccidentalName)),
  delay(accidAlter(Alter, AccidentalName, Step, Octave,
                   AccidStepsOctaves, KeySteps, KeyAlter)).

accidental([element(accidental, [], [AccidentalName])]) -->
  state(notehead, step, octave,
        alter, keySteps, keyAlter, -accidStepsOctaves,
        accidentalCond(Accidental, AccidentalName)),
  termp(Accidental).
accidental([]) -->
  state(alter, step, octave, accidStepsOctaves, keySteps, keyAlter, noAccidAlter).

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

noteStem([]) -->
  state(duration, division, durationNoStem),
  {debug(note, "noteStem: without stem~n", [])}.
noteStem([element(stem, [], [Dir]) | Beams]) -->
  verticalSeg(Stem),
  state(notehead, stem(Stem), duration, division, dir(Dir), noteheadStemCond),
  {debug(note, "noteStem: with stem~n", [])},
  ( noteFlag(Beams)
  | noteBeams(Beams)
  | noFlagBeam(Beams)
  ).
noteFlag([]) -->
  state(stem, flag(Flag), duration, division, dir, stemFlagCond),
  termp(Flag),
  {debug(note, "noteFlag: with flag~n", [])}.

stemBeamCond(State, Ref, Beam, Notehead, Stem, BeamIn, BeamOut, Dir, Stafflines) :-
  stemBeamState(State, Notehead, Stem, StemX, StemY, BeamIn, BeamOut, Beam, Dir),
  interlineAtX(Stafflines, StemX, Interline),
  stemBeamRef(Ref, StemY, Beam, Dir, Interline).

stemBeamDir(up, H, Stem, StemTop) :-
  segCorner(v, H-top, Stem, StemTop).
stemBeamDir(down, H, Stem, StemBottom) :-
  segCorner(v, H-bottom, Stem, StemBottom).

% delay:mode(note:stemBeamState(
%               ground,         _,        _,    _,     _,     _,      _,      _,    _)).
% delay:mode(note:stemBeamState(
%               _,              ground, ground, _,     _,     ground, ground, ground, _)).
stemBeamState('forward hook', Notehead, Stem, StemX, StemY, noBeam, noBeam, Beam, Dir) :-
  gtrace,
  stemBeamState(begin, Notehead, Stem, StemX, StemY, noBeam, Beam, Beam, Dir),
  ccxWidth(Notehead, NoteheadWidth),
  segLength(Beam, BeamLength),
  diffEps(0, NoteheadWidth, BeamLength).
stemBeamState(begin, _Notehead, Stem, StemX, StemY, noBeam, Beam, Beam, Dir) :-
  segStartX(Beam, BeamX),
  stemBeamDir(Dir, left, Stem, point(StemX, StemY)),
  segThickness(Stem, StemThickness),
  { HalfThickness == StemThickness },
  diffEps(HalfThickness, BeamX, StemX).
stemBeamState(continue, _Notehead, Stem, StemX, StemY, Beam, Beam, Beam, Dir) :-
  segStartX(Beam, BeamStartX),
  segEndX(Beam, BeamEndX),
  stemBeamDir(Dir, mid, Stem, point(StemX, StemY)),
  { BeamStartX + 1 =< StemX },
  { StemX =< BeamEndX - 1}.
stemBeamState(end, _Notehead, Stem, StemX, StemY, Beam, noBeam, Beam, Dir) :-
  segEndX(Beam, BeamX),
  stemBeamDir(Dir, right, Stem, point(StemX, StemY)),
  segThickness(Stem, StemThickness),
  { HalfThickness == StemThickness },
  diffEps(HalfThickness, BeamX, StemX).
stemBeamState('backward hook', Notehead, Stem, StemX, StemY, noBeam, noBeam, Beam, Dir) :-
  stemBeamState(end, Notehead, Stem, StemX, StemY, Beam, noBeam, Beam, Dir),
  ccxWidth(Notehead, NoteheadWidth),
  segLength(Beam, BeamLength),
  diffEps(0, NoteheadWidth, BeamLength).

beamRefDir(up, +).
beamRefDir(down, -).

stemBeamRef(noRef, StemY, Beam, _Dir, _Interline) :-
  segStartY(Beam, BeamY),
  diffEps(2.7, BeamY, StemY).
stemBeamRef(BeamRef, _StemY, Beam, Dir, Interline) :-
  segStartY(BeamRef, BeamRefY),
  segStartY(Beam, BeamY),
  segThickness(BeamRef, RefThickness),
  segThickness(Beam, BeamThickness),
  beamRefDir(Dir, Op),
  Expr =.. [Op, BeamRefY, RefThickness / 2 + Interline / 4 + BeamThickness / 2],
  { YFromRef == Expr },
  diffEps(2.7, BeamY, YFromRef).

beamsCond(Beams, Duration, Division) :-
  delay(length(Beams, NumBeams)),
  NumBeams::integer(1, 10),
  { Duration / Division == 1 / (2 ** NumBeams) }.

%!  noteBeams(?Beams) is nondet.
%
%   Specify the relationship between a note and its beams.
%   For more information, see beam.md
noteBeams(Beams) -->
  state(duration, division, beamsCond(Beams)),
  sequence2(noteBeam, [element(beam, [number='0'], [_]) | Beams], [noRef | _]).

atom_inc(N, N1, BeamN) :-
  atom_number(N, NInt),
  { N1Int == NInt + 1},
  atom_number(N1, N1Int),
  atom_concat(beam, N1, BeamN).

noteBeam(element(beam, [number=N], [_]),
         element(beam, [number=N1], [State]), Ref, Beam) -->
  { atom_inc(N, N1, BeamN) },
  state(notehead, stem, -BeamN, dir, stafflines, stemBeamCond(State, Ref, Beam)),
  noteBeamState(State, Beam).
noteBeamState(begin, Beam) -->
  termp(Beam).
noteBeamState('forward hook', Beam) -->
  termp(Beam).
noteBeamState('backward hook', Beam) -->
  termp(Beam).
noteBeamState(continue, _) -->
  { true }.
noteBeamState(end, _) -->
  { true }.

noFlagBeam([]) -->
  state(duration, division, durationNoFlagBeam),
  {debug(note, "noteFlag: without flag~n", [])}.
