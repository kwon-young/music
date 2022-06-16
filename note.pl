:- module(note, [note//1]).

:- use_module(library(clpBNR)).
:- use_module(library(delay)).
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

durationCond(DurationAtom, Div, DurationPerQuarter) :-
  delay(atom_number(DurationAtom, Duration)),
  DurationPerQuarter::real(1r256, 8),
  [Duration, Div]::integer(1, _),
  { Duration == DurationPerQuarter * Div }.

types(['1024th', '512th', '256th', '128th', '64th', '32nd', '16th',
       'eighth', 'quarter', 'half', 'whole', 'breve']).
types(N, T) :-
  types(Types),
  nth0(N, Types, T).
typeCond(Type, Duration) :-
  types(Types),
  length(Types, NumTypes),
  nth0u(Q, Types, 'quarter'),
  delay(nth0u(N, Types, Type)),
  N::integer(0, NumTypes),
  % use a real of N because of imprecision of inverting powers
  roundCons(NReal, N),
  { Duration == 2 ** (NReal - Q)}.

noteheads(['noteheadBlack', 'noteheadHalf', 'noteheadWhole', 'noteheadDoubleWhole']).

delay:mode(note:noteheadDuration(ground, _     )).
delay:mode(note:noteheadDuration(_     , ground)).

noteheadDuration('noteheadBlack', Duration) :-
  {Duration =< 1}.
noteheadDuration('noteheadHalf', 2).
noteheadDuration('noteheadWhole', 4).
noteheadDuration('noteheadDoubleWhole', 8).

noteheadCond(Notehead, Duration) :-
  ccxEtiqsCond(Notehead, NoteheadEtiq),
  delay(noteheadDuration(NoteheadEtiq, Duration)).

durationNoStem(Duration) :-
  { Duration >= 4 }.

noteheadStemCond(Notehead, Stem, Duration, Dir) :-
  { Duration =< 2 },
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

stemFlagCond(Stem, Flag, Duration, Dir) :-
  debug(note, "stemFlagCond: ~p, ~p, ~p, ~p~n", [Stem, Flag, Duration, Dir]),
  { Duration =< 1r2 },
  ccxEtiqsCond(Flag, FlagEtiq),
  flagValDir(FlagEtiq, Val, FlagDir),
  flagDuration(Val, Duration),
  flagDirCond(Dir, FlagDir, Stem, Flag).

delay:mode(system:atom_concat(ground , ground , _)).
delay:mode(system:atom_concat(_      , _      , ground)).

% flagValDir(Flag, Val, Dir) :-
%   delay(atom_concat('flag', Val, FlagVal)),
%   delay(atom_concat(FlagVal, Dir, Flag)).
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

flagDuration(FlagVal, Duration) :-
  FlagVal::integer(8, 1024),
  { Duration == 4 / FlagVal },
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

durationNoFlag(Duration) :-
  { Duration >= 1 }.

succ('C', 'D').
succ('D', 'E').
succ('E', 'F').
succ('F', 'G').
succ('G', 'A').
succ('A', 'B').

numIntervals(N, Step1, Octave1, Step2, Octave2) :-
  N::integer(-70, 70),
  [Octave1, Octave2]::integer(0, 9),
  {N == (Octave2 - Octave1) * 7 + NPitch},
  numIntervals(NPitch, Step1, Step2).
numIntervals(0, Step, Step).
numIntervals(N1, Step1, Step3) :-
  [N, N1]::integer(0, 70),
  {N1 == N + 1},
  succ(Step1, Step2),
  numIntervals(N, Step2, Step3).
numIntervals(N1, Step1, Step3) :-
  [N, N1]::integer(-70, 0),
  {N1 == N - 1},
  succ(Step2, Step1),
  numIntervals(N, Step2, Step3).

notePitchCond(Notehead, Step, Octave,
              BaseLine, BaseStep, BaseOctave,
              Stafflines) :-
  ccxOrigin(Notehead, point(NoteX, NoteY)),
  interlineAtX(Stafflines, NoteX, Interline),
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

note(element(note, [], [Pitch, Duration, Type])) -->
  {debug(note, "note: ~p, ~p, ~p~n", [Pitch, Duration, Type])},
  duration(Duration),
  type(Type),
  noteGraphique,
  notePitch(Pitch).

duration(element(duration, [], [DurationAtom])) -->
  state(division, duration, durationCond(DurationAtom)).

type(element(type, [], [Type])) -->
  state(duration, typeCond(Type)).

notePitch(element(pitch, [], [Step, Octave])) -->
  {debug(note, "notePitch~n", [])},
  step(Step),
  octave(Octave),
  state(notehead, step, octave,
        baseLine, baseStep, baseOctave,
        stafflines,
        notePitchCond).

step(element(step, [], [Step])) -->
  stateAdd(step(Step)).

octaveCond(OctaveAtom, Octave) :-
  Octave::integer(0, 9),
  delay(atom_number(OctaveAtom, Octave)).

octave(element(octave, [], [Octave])) -->
  state(octave, octaveCond(Octave)).

noteGraphique() -->
  {debug(note, "noteGraphique~n", [])},
  term(Notehead),
  state(notehead(Notehead), duration, noteheadCond),
  noteStem(Notehead).
noteStem(_) -->
  state(duration, durationNoStem),
  {debug(note, "noteStem: without stem~n", [])}.
noteStem(Notehead) -->
  state(stem(Stem), duration, dir, noteheadStemCond(Notehead)),
  verticalSeg(Stem),
  {debug(note, "noteStem: with stem~n", [])},
  noteFlag(Stem).
noteFlag(Stem) -->
  state(flag(Flag), duration, dir, stemFlagCond(Stem)),
  term(Flag),
  {debug(note, "noteFlag: with flag~n", [])}.
noteFlag(_) -->
  state(duration, durationNoFlag),
  {debug(note, "noteFlag: without flag~n", [])}.
