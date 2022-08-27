:- module(pitch_cond, [pitchCond/8, pitchCondLine/8, pitchCondCcx/8,
                  numIntervals/5, inRange/6]).

:- use_module(library(delay)).
:- use_module(library(clpBNR)).
:- use_module(ccx).
:- use_module(seg).
:- use_module(geo).
:- use_module(music_utils).

delay:mode(pitch_cond:step(ground, _)).
delay:mode(pitch_cond:step(_, ground)).
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

inRange(S1, O1, S2, O2, S, O) :-
  numIntervals(N1, S1, O1, S, O),
  { N1 >= 0 },
  numIntervals(N2, S, O, S2, O2),
  { N2 >= 0 }.


pitchCondLine(Notehead, Step, Octave,
              BaseLine, BaseStep, BaseOctave,
              PitchIntervals, Stafflines) :-
  ccxOrigin(Notehead, point(NoteX, _)),
  segYAtX(BaseLine, BaseLineY, NoteX),
  pitchCond(Notehead, Step, Octave,
            BaseLineY, BaseStep, BaseOctave,
            PitchIntervals, Stafflines).
pitchCondCcx(Notehead, Step, Octave,
             BaseCcx, BaseStep, BaseOctave,
             PitchIntervals, Stafflines) :-
  ccxOrigin(BaseCcx, point(_, BaseY)),
  pitchCond(Notehead, Step, Octave,
            BaseY, BaseStep, BaseOctave,
            PitchIntervals, Stafflines).
pitchCond(Notehead, Step, Octave,
          BaseY, BaseStep, BaseOctave,
          PitchIntervals, Stafflines) :-
  ccxOrigin(Notehead, point(NoteX, NoteY)),
  interlineAtX(Stafflines, NoteX, Interline),
  debug(note, "notePitchCond Interline ~p~n", [Interline]),
  {
    HalfInterline == Interline / 2,
    NoteOffset == NoteY - BaseY,
    GraphicalIntervals == NoteOffset / HalfInterline
  },
  debug(note, "GraphicalIntervals: ~p~n", [GraphicalIntervals]),
  diffEps(0, GraphicalIntervals, PitchIntervals),
  numIntervals(PitchIntervals, Step, Octave, BaseStep, BaseOctave),
  debug(note, "PitchIntervals: ~p~n", [PitchIntervals]).

