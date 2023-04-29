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
:- use_module(state).
:- use_module(epf_geo).
:- use_module(utils).
:- use_module(music_utils).
:- use_module(pitch_cond).
:- use_module(constraint_math).

:- multifile delay:mode/1.

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

noteheadStemCond(false, Notehead, Stem, Duration, Division, Dir, NoteStem, Eps) :-
  { Duration / Division =< 2 },
  delay(stemDirCond(Dir, Notehead, Stem, NoteStem, Eps)).
noteheadStemCond(true, _Notehead, _Stem, Duration, Division, _Dir, _NoteStem, _Eps) :-
  { Duration / Division =< 2 }.

delay:mode(note:stemDirCond(ground, _, _, _, _)).
delay:mode(note:stemDirCond(_, ground, ground, _, _)).
stemDirCond(up, Notehead, Stem, NoteStem, Eps) :-
  ccxRight(Notehead, NoteRight),
  ccxOrigin(Notehead, point(_, NoteY)),
  segHV(v, right, bottom, Stem, point(StemRight, StemBottom)),
  eps(Eps, NoteRight, StemRight),
  { NoteY == StemBottom + NoteStem + Eps}.
stemDirCond(down, Notehead, Stem, NoteStem, Eps) :-
  ccxLeft(Notehead, NoteLeft),
  ccxOrigin(Notehead, point(_, NoteY)),
  segHV(v, left, top, Stem, point(StemLeft, StemTop)),
  eps(Eps, NoteLeft, StemLeft),
  { StemTop == NoteY + NoteStem + Eps}.

stemFlagCond(Stem, Flag, Duration, Division, Dir, Eps) :-
  debug(note, "stemFlagCond: ~p, ~p, ~p, ~p~n", [Stem, Flag, Duration, Dir]),
  { Duration / Division =< 1r2 },
  ccxEtiqsCond(Flag, FlagEtiq),
  flagValDir(FlagEtiq, Val, FlagDir),
  flagDuration(Val, Duration, Division),
  flagDirCond(Dir, FlagDir, Stem, Flag, Eps).

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

flagDirCond(Dir, FlagDir, Stem, Flag, Eps) :-
  ccxOrigin(Flag, FlagOrigin),
  segHV(v, left, top, Stem, StemTop),
  segHV(v, left, bottom, Stem, StemBottom),
  delay(flagDir(Dir, FlagDir, FlagOrigin, StemTop, StemBottom, Eps)).

delay:mode(note:flagDir(ground , _      , _      , _      , _     )).
delay:mode(note:flagDir(_      , ground , _      , _      , _     )).
delay:mode(note:flagDir(_      , _      , ground , ground , _     )).
delay:mode(note:flagDir(_      , _      , ground , _      , ground)).
flagDir(up, 'Up', FlagOrigin, StemTop, _, Eps) :-
  eps(p, Eps, FlagOrigin, StemTop).
flagDir(down, 'Down', FlagOrigin, _, StemBottom, Eps) :-
  eps(p, Eps, FlagOrigin, StemBottom).

durationNoFlagBeam(Duration, Division) :-
  { Duration / Division >= 1 },
  debug(note, "Division: ~p~n", [Division]).

dotCond(Ref, Dot, NumIntervals, Interline, Eps) :-
  ccxEtiqsCond(Ref, 1, 'notehead'),
  ccxRight(Ref, NoteX),
  ccxOrigin(Ref, point(_, NoteY)),
  { PredX == NoteX + 1r2 * Interline },
  dotCondStart(NumIntervals, PredX, NoteY, Dot, Interline, Eps).
dotCond(Ref, Dot, _, Interline, Eps) :-
  ccxEtiqsCond(Ref, 'dots'),
  ccxOrigin(Ref, point(RefX, RefY)),
  { PredX == RefX + 3r4 * Interline },
  dotCond_(point(PredX, RefY), Dot, Eps).
dotCondStart(NumIntervals, PredX, NoteY, Dot, Interline, Eps) :-
  N::integer,
  { NumIntervals == 2 * N },
  { PredY == NoteY - 1r2 * Interline },
  dotCond_(point(PredX, PredY), Dot, Eps).
dotCondStart(NumIntervals, PredX, NoteY, Dot, _, Eps) :-
  N::integer,
  { NumIntervals == 2 * N + 1 },
  dotCond_(point(PredX, NoteY), Dot, Eps).
dotCond_(Point, Dot, Eps) :-
  ccxEtiqsCond(Dot, 'dots'),
  ccxOrigin(Dot, DotCenter),
  eps(p, Eps, DotCenter, Point).

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

note(element(note, [id=], [element(chord, [], []), Pitch, Duration, Type | NoteAttributes])) -->
  chord,
  % does not renew stem, dir, flag
  states([
    +(chord, true), +duration, +dots, +step, +octave, +intervals, +alter, +type]),
  duration(Duration),
  type(Type),
  notePitch(Pitch),
  noteGraphique(NoteAttributes).
note(element(note, [], [Pitch, Duration, Type | NoteAttributes])) -->
  states([
    +(chord, false), +duration, +dots, +notehead, +stem, +dir, +step, +octave,
    +intervals, +alter, +flag, +type]),
  {debug(note, "note: ~p, ~p, ~p~n", [Pitch, Duration, Type])},
  duration(Duration),
  type(Type),
  notePitch(Pitch),
  noteGraphique(NoteAttributes).
note(element(note, [], [Rest, Duration, Type])) -->
  states([+duration, +(dots, 0)]),
  duration(Duration),
  type(Type),
  rest(Rest).

chordCond(Stem, Notehead, Eps) :-
  ccxEtiqsCond(Notehead, 1, notehead),
  ccxLeftRight(Notehead, NoteLeft, NoteRight),
  ccxOrigin(Notehead, point(_, NoteheadY)),
  segTop(v, Stem, StemTop),
  segBottom(v, Stem, StemBottom),
  segYAtX(Stem, NoteheadY, StemX),
  segThickness(Stem, StemThickness),
  {
    StemTop =< NoteheadY,
    NoteheadY =< StemBottom,
    StemLeft == StemX - StemThickness / 2,
    StemRight == StemX + StemThickness / 2
  },
  when(ground(Notehead), stemNoteheadChord(StemLeft, StemRight, NoteLeft, NoteRight,
                                           Eps)).

stemNoteheadChord(StemLeft, StemRight, NoteLeft, NoteRight, Eps) :-
  debug(note, "stemNoteheadChord: ~p, ~p~n", [StemLeft, StemRight]),
  (
    eps(Eps, StemLeft, NoteLeft)
  ; eps(Eps, StemRight, NoteRight)
  ).


chord -->
  statep(chordCond, [o(stem), +(notehead, Notehead), o(eps)]),
  selectp(Notehead).

restCond(Rest, Duration, Division, Stafflines, Eps) :-
  ccxEtiqsCond(Rest, 1, 'rest'),
  delay(restCond_(Rest, Duration, Division, Stafflines, Eps)).

delay:mode(note:restCond_(ground, _, _, _)).
delay:mode(note:restCond_(_, ground, ground, _)).
restCond_(Rest, Duration, Division, Stafflines, Eps) :-
  ccxEtiqsCond(Rest, 'restWhole'),
  { Duration / Division == 4 },
  ccxOrigin(Rest, point(RestX, RestY)),
  nth1(4, Stafflines, Line),
  segYAtX(Line, LineY, RestX),
  eps(Eps, LineY, RestY).
restCond_(Rest, Duration, Division, Stafflines, Eps) :-
  ccxEtiqsCond(Rest, 'restHalf'),
  { Duration / Division == 2 },
  ccxOrigin(Rest, point(RestX, RestY)),
  nth1(3, Stafflines, Line),
  segYAtX(Line, LineY, RestX),
  eps(Eps, LineY, RestY).
restCond_(Rest, Duration, Division, Stafflines, Eps) :-
  ccxEtiqsCond(Rest, 'restQuarter'),
  { Duration / Division == 1 },
  ccxOrigin(Rest, point(RestX, RestY)),
  nth1(3, Stafflines, Line),
  segYAtX(Line, LineY, RestX),
  eps(Eps, LineY, RestY).
restCond_(Rest, Duration, Division, Stafflines, Eps) :-
  restVal(RestName, Val),
  ccxEtiqsCond(Rest, RestName),
  { Duration / Division == 4 / Val },
  ccxOrigin(Rest, point(RestX, RestY)),
  nth1(3, Stafflines, Line),
  segYAtX(Line, LineY, RestX),
  eps(Eps, LineY, RestY).

restVal(Rest, Val) :-
  delay(atom_codes(Rest, Codes)),
  delay(phrase(restVal(Val), Codes)).
restVal(Val) -->
  "rest",
  flagVal(Val).

rest(element(rest, [], [])) -->
  statep(restCond(Rest), [o(duration), o(division), o(stafflines), o(eps)]),
  termp(Rest).

duration(element(duration, [], [DurationAtom])) -->
  statep(durationCond(DurationAtom), [o(division), o(duration), o(dots)]).

type(element(type, [], [Type])) -->
  statep(typeCond(Type), [o(duration), o(division)]).

notePitch(element(pitch, [], Pitch)) -->
  {debug(note, "notePitch~n", [])},
  {when((ground(Alter) ; ground(Pitch)), append([[Step], Alter, [Octave]], Pitch))},
  step(Step),
  alter(Alter),
  octave(Octave),
  statep(
    pitchCondLine,
    [o(notehead), o(step), o(octave), o(baseLine), o(baseStep), o(baseOctave),
     o(intervals), o(halfInterline), o(eps)]).

step(element(step, [], [Step])) -->
  state(o(step, Step)).

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
  statep(alterCond(Alter), [o(alter)]).

octaveCond(OctaveAtom, Octave) :-
  Octave::integer(0, 9),
  delay(atom_number(OctaveAtom, Octave)).

octave(element(octave, [], [Octave])) -->
  statep(octaveCond(Octave), [o(octave)]).

delay:mode(system:append(ground, ground, _)).
delay:mode(system:append(ground, _, ground)).
delay:mode(system:append(_, ground, ground)).
noteGraphique(Attributes) -->
  {debug(note, "noteGraphique~n", [])},
  notehead(NoteheadAttributes),
  {delay(system:append(NoteheadAttributes, Stem, Attributes))},
  noteStem(Stem).

notehead(Attributes) -->
  statep(noteheadCond, [o(notehead, Notehead), o(duration), o(division)]),
  termp(Notehead),
  {debug(note, "notehead: ~p~n", [Notehead])},
  dots(Notehead, Dots),
  accidental(Accidental),
  {append(Dots, Accidental, Attributes)},
  ledgerlines.

dots(Notehead, Dots) -->
  {debug(note, "dots: In ~p~n", [Dots])},
  statep(dotsCond(Dots), [o(dots), o(duration)]),
  sequence2(dot, [_ | Dots], [Notehead | _]),
  {debug(note, "dots: Out ~p~n", [Dots])}.

dot(_, element(dot, [], []), Ref, Dot) -->
  statep(dotCond(Ref, Dot), [o(numIntervals), o(interline), o(eps)]),
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
               AccidStepsOctaves, [AccidentalName-Step-Octave | AccidStepsOctaves],
               Eps) :-
  ccxEtiqsCond(Accidental, AccidentalEtiq),
  ccxEtiqsCond(Accidental, 1, accid),
  ccxOrigin(Notehead, point(NoteX, NoteY)),
  ccxOrigin(Accidental, point(AccidX, AccidY)),
  eps(Eps, NoteY, AccidY),
  { AccidX =< NoteX },
  delay(accidName(AccidentalEtiq, AccidentalName)),
  delay(accidAlter(Alter, AccidentalName, Step, Octave,
                   AccidStepsOctaves, KeySteps, KeyAlter)).

accidental([element(accidental, [], [AccidentalName])]) -->
  statep(
    accidentalCond(Accidental, AccidentalName),
    [o(notehead), o(step), o(octave), o(alter), o(keySteps), o(keyAlter),
    -accidStepsOctaves, o(eps)]),
  termp(Accidental).
accidental([]) -->
  statep(noAccidAlter,
         [o(alter), o(step), o(octave), o(accidStepsOctaves), o(keySteps), o(keyAlter)]).

ledgerlineCond(LedgerLines, Notehead, Stafflines, Num, PitchIntervals, Interline, Eps) :-
  length(Stafflines, NumStafflines),
  NumLedgerLines::integer(0, 70),
  { NumLedgerLines =< abs(PitchIntervals) / 2 - (NumStafflines - Num) },
  { NumLedgerLines >= (abs(PitchIntervals) - 1) / 2 - (NumStafflines - Num) },
  ccxOrigin(Notehead, point(NoteX, NoteY)),
  last(Stafflines, TopLine),
  segYAtX(TopLine, TopY, NoteX),
  { NoteY =< TopY },
  NumLedgerLinesGraphique::integer(0, 70),
  { NumLedgerLinesGraphique =< (TopY - NoteY) / Interline },
  { NumLedgerLinesGraphique >= (TopY - (NoteY + Interline / 2)) / Interline },
  eps(Eps, NumLedgerLines, NumLedgerLinesGraphique),
  length(LedgerLines, NumLedgerLines),
  maplist({NoteX}/[Seg, Y]>>(segYAtX(Seg, Y, NoteX)), LedgerLines, Ys),
  numlist(1, NumLedgerLines, Coeffs),
  maplist({TopY, Interline}/[Coeff, Expr]>>({Expr == TopY - Coeff * Interline}),
          Coeffs, Exprs),
  setting(music:staff_interline_eps, Eps),
  maplist(eps(Eps), Ys, Exprs).
ledgerlineCond(LedgerLines, Notehead, Stafflines, Num, PitchIntervals, Interline, Eps) :-
  { NumLedgerLines =< PitchIntervals / 2 - (Num - 1) },
  { NumLedgerLines >= (PitchIntervals - 1) / 2 - (Num - 1) },
  ccxOrigin(Notehead, point(NoteX, NoteY)),
  debug(note, "ledgerlineCond below, Notehead origin point(~p, ~p)~n", [NoteX, NoteY]),
  nth0(0, Stafflines, BottomLine),
  segYAtX(BottomLine, BottomY, NoteX),
  { BottomY =< NoteY },
  NumLedgerLinesGraphique::integer(0, 70),
  { NumLedgerLinesGraphique =< (NoteY - BottomY) / Interline },
  { NumLedgerLinesGraphique >= ((NoteY - Interline / 2) - BottomY) / Interline },
  eps(Eps, NumLedgerLines, NumLedgerLinesGraphique),
  length(LedgerLines, NumLedgerLines),
  maplist({NoteX}/[Seg, Y]>>(segYAtX(Seg, Y, NoteX)), LedgerLines, Ys),
  numlist(1, NumLedgerLines, Coeffs),
  maplist({BottomY, Interline}/[Coeff, Expr]>>({Expr == BottomY + Coeff * Interline}),
          Coeffs, Exprs),
  setting(music:staff_interline_eps, Eps),
  maplist(eps(Eps), Ys, Exprs).
ledgerlineCond([], Notehead, Stafflines, Num, PitchIntervals, Interline, _Eps) :-
  { PitchIntervals =< 7 },
  ccxOrigin(Notehead, point(NoteX, NoteY)),
  last(Stafflines, TopLine),
  segYAtX(TopLine, TopY, NoteX),
  nth1(Num, Stafflines, BaseLine),
  segYAtX(BaseLine, BaseY, NoteX),
  {
    TopY - Interline / 2 =< NoteY,
    NoteY =< BaseY
  }.
ledgerlineCond([], Notehead, Stafflines, Num, PitchIntervals, Interline, _Eps) :-
  { 1 =< PitchIntervals, PitchIntervals =< 3 },
  ccxOrigin(Notehead, point(NoteX, NoteY)),
  nth0(0, Stafflines, BottomLine),
  segYAtX(BottomLine, BottomY, NoteX),
  nth1(Num, Stafflines, BaseLine),
  segYAtX(BaseLine, BaseY, NoteX),
  {
    BaseY + Interline / 2 =< NoteY,
    NoteY =< BottomY + Interline / 2
  }.

ledgerlines -->
  statep(ledgerlineCond(LedgerLines),
        [o(notehead), o(stafflines), o(num), o(intervals), o(interline), o(eps)]),
  sequence(selectp, LedgerLines).

noteStem([]) -->
  statep(durationNoStem, [o(duration), o(division)]),
  {debug(note, "noteStem: without stem~n", [])}.
noteStem([element(stem, [], [Dir]) | Beams]) -->
  statep(
    noteheadStemCond,
    [o(chord, false), o(notehead), o(stem, Stem), o(duration), o(division),
     o(dir, Dir), o(noteStem), o(eps)]),
  verticalSeg(Stem),
  {debug(note, "noteStem: with stem~n", [])},
  noteStemEnd(Beams).
noteStem([element(stem, [], [Dir]) | Beams]) -->
  statep(
    noteheadStemCond,
    [o(chord, true), o(notehead), o(stem), o(duration), o(division),
     o(dir, Dir), o(noteStem), o(eps)]),
  noteStemEnd(Beams).
noteStemEnd(Beams) -->
  ( noteFlag(Beams)
  | noteBeams(Beams)
  | noFlagBeam(Beams)
  ).
noteFlag([]) -->
  statep(stemFlagCond, [o(stem), o(flag, Flag), o(duration), o(division), o(dir),
                        o(eps)]),
  termp(Flag),
  {debug(note, "noteFlag: with flag~n", [])}.
noteFlag([]) -->
  state(o(chord, true)),
  statep(stemFlagCond, [o(stem), o(flag), o(duration), o(division), o(dir), o(eps)]),
  {debug(note, "noteFlag: with flag~n", [])}.

stemBeamCond(State, Ref, Beam, Notehead, Stem, noBeam, BeamOut, Dir, Interline, Eps) :-
  delay(stemBeamState(State, Notehead, Stem, StemY, BeamOut, Beam, Dir, Eps)),
  stemBeamRef(Ref, StemY, Beam, Dir, Interline, Eps).
stemNoBeamCond(State, Ref, Beam, Stem, BeamIn, BeamOut, Dir, Interline, Eps) :-
  stemNoBeamState(State, Stem, _StemX, StemY, BeamIn, BeamOut, Beam, Dir, Eps),
  stemBeamRef(Ref, StemY, Beam, Dir, Interline, Eps).

stemBeamDir(up, H, Stem, StemTop) :-
  segHV(v, H, top, Stem, StemTop).
stemBeamDir(down, H, Stem, StemBottom) :-
  segHV(v, H, bottom, Stem, StemBottom).

delay:mode(note:stemBeamState(
              ground , _      , _      , _ , _ , _ , _      , _, _)).
delay:mode(note:stemBeamState(
              _      , ground , ground , _ , _ , _ , ground , _, _)).
stemBeamState('forward hook', Notehead, Stem, StemY, noBeam, Beam, Dir, Eps) :-
  stemBeamState(begin, Notehead, Stem, StemY, Beam, Beam, Dir, Eps),
  ccxWidth(Notehead, NoteheadWidth),
  segLength(Beam, BeamLength),
  eps(Eps, NoteheadWidth, BeamLength).
stemBeamState(begin, _Notehead, Stem, StemY, Beam, Beam, Dir, Eps) :-
  segStartX(Beam, BeamX),
  stemBeamDir(Dir, left, Stem, point(StemX, StemY)),
  eps(Eps, BeamX, StemX).
stemBeamState('backward hook', Notehead, Stem, StemX, StemY, noBeam, Beam, Dir, Eps) :-
  stemNoBeamState(end, Stem, StemX, StemY, Beam, noBeam, Beam, Dir, Eps),
  ccxWidth(Notehead, NoteheadWidth),
  segLength(Beam, BeamLength),
  eps(Eps, NoteheadWidth, BeamLength).

stemNoBeamState(continue, Stem, StemX, StemY, Beam, Beam, Beam, Dir, _Eps) :-
  segStartX(Beam, BeamStartX),
  segEndX(Beam, BeamEndX),
  stemBeamDir(Dir, mid, Stem, point(StemX, StemY)),
  { BeamStartX + 1 =< StemX },
  { StemX =< BeamEndX - 1}.
stemNoBeamState(end, Stem, StemX, StemY, Beam, noBeam, Beam, Dir, Eps) :-
  segEndX(Beam, BeamX),
  stemBeamDir(Dir, right, Stem, point(StemX, StemY)),
  eps(Eps, BeamX, StemX).

beamRefDir(up, +).
beamRefDir(down, -).

stemBeamRef(noRef, StemY, Beam, _Dir, _Interline, Eps) :-
  segStartY(Beam, BeamY),
  eps(Eps, BeamY, StemY).
stemBeamRef(BeamRef, _StemY, Beam, Dir, Interline, Eps) :-
  segStartY(BeamRef, BeamRefY),
  segStartY(Beam, BeamY),
  segThickness(BeamRef, RefThickness),
  segThickness(Beam, BeamThickness),
  beamRefDir(Dir, Op),
  Expr =.. [Op, BeamRefY, RefThickness / 2 + Interline / 4 + BeamThickness / 2],
  { YFromRef == Expr },
  eps(Eps, BeamY, YFromRef).

beamsCond(Beams, Duration, Division) :-
  delay(length(Beams, NumBeams)),
  beamsChordCond(NumBeams, Duration, Division).
beamsChordCond(NumBeams, Duration, Division) :-
  NumBeams::integer(1, 10),
  { Duration / Division == 1 / (2 ** NumBeams) }.

%!  noteBeams(?Beams) is nondet.
%
%   Specify the relationship between a note and its beams.
%   For more information, see beam.md
noteBeams(Beams) -->
  state(o(chord, false)),
  statep(beamsCond(Beams), [o(duration), o(division)]),
  sequence2(noteBeam, [element(beam, [number='0'], [_]) | Beams], [noRef | _]).
noteBeams([]) -->
  state(o(chord, true)),
  statep(beamsChordCond, [o(numBeams), o(duration), o(division)]).

atom_inc(N, N1, beam-N1Int) :-
  atom_number(N, NInt),
  { N1Int == NInt + 1},
  atom_number(N1, N1Int).

noteBeam(element(beam, [number=N], [_]),
         element(beam, [number=N1], [State]), Ref, Beam) -->
  { atom_inc(N, N1, beam-N1Int) },
  state(+(numBeams, N1Int)),
  statep(stemNoBeamCond(State, Ref, Beam), [o(stem), -(beam-N1Int), o(dir), o(stafflines)]).
noteBeam(element(beam, [number=N], [_]),
         element(beam, [number=N1], [State]), Ref, Beam) -->
  { atom_inc(N, N1, beam-N1Int) },
  state(+(numBeams, N1Int)),
  statep(stemBeamCond(State, Ref, Beam),
         [o(notehead), o(stem), -(beam-N1Int), o(dir), o(interline), o(eps)]),
  termp(Beam).

noFlagBeam([]) -->
  states([+(flag, noFlag), +(numBeams, 0)]),
  statep(durationNoFlagBeam, [o(duration), o(division)]),
  {debug(note, "noteFlag: without flag~n", [])}.
