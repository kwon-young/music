:- module(pitch, [notePitch//2]).

:- use_module(library(clpBNR)).
:- use_module(ccx).
:- use_module(geo).
:- use_module(epf).
:- use_module(epf_geo).
:- use_module(cond).
:- use_module(utils).
:- use_module(music_utils).
:- use_module(le).

:- use_module(library(settings)).
:- setting(notehead_pos_eps, number, 0, 'notehead vertical position').
:- setting(ledgerline_pos_eps, number, 0, 'ledgerline vertical position').

product(L1, L2, L3) :-
  product(L2, L1, L1, L3).
product([], _, _, []).
product([B | L2], L1R, L1, L3) :-
  product_(L1R, [B | L2], L1, L3).
product_([], [_ | L2], L1, L3) :-
  product(L2, L1, L1, L3).
product_([A | L1R], [B | L2], L1, [A-B | L3]) :-
  product([B | L2], L1R, L1, L3).

pitches(['C', 'D', 'E', 'F', 'G', 'A', 'B']).

:- table pitchOctaves/1.

pitchOctaves(PitchOctaves) :-
  pitches(Pitches),
  arange(0, 9, Range),
  maplist(atom_number, Octaves, Range),
  product(Pitches, Octaves, PitchOctaves).

ccxAboveSeg(Seg, Ccx) :-
  ccxOrigin(Ccx, point(CcxX, CcxY)),
  segYAtX(Seg, SegY, CcxX),
  { CcxY =< SegY }.

ccxBelowSeg(Seg, Ccx) :-
  ccxOrigin(Ccx, point(CcxX, CcxY)),
  segYAtX(Seg, SegY, CcxX),
  { CcxY >= SegY }.

noteDirectionCond(+, BaseLine, Note, Intervals) :-
  { Intervals >= 0 },
  ccxAboveSeg(BaseLine, Note).
noteDirectionCond(-, BaseLine, Note, Intervals) :-
  { Intervals < 0 },
  ccxBelowSeg(BaseLine, Note).

nth0When(N, L, El) :-
  when(;(ground(N), ground(El)), once(nth0(N, L, El))).

notePitchCond(BasePitch, NotePitch, NumIntervals) :-
  pitchOctaves(Pitches),
  length(Pitches, NumPitches),
  [BaseN, NoteN]::integer(0, NumPitches),
  NumIntervals::integer(-NumPitches, NumPitches),
  nth0When(BaseN, Pitches, BasePitch),
  nth0When(NoteN, Pitches, NotePitch),
  { NumIntervals == NoteN - BaseN }.

notePitch(Note, element(pitch, [], [element(step, [], [NotePitch]),
                                    element(octave, [], [NoteOctave])])) -->
  selectchk(baseLine(Num, BaseLine)),
  selectchk(basePitch(BasePitch)),
  {
    notePitchCond(BasePitch, NotePitch-NoteOctave, Intervals),
    noteDirectionCond(Dir, BaseLine, Note, Intervals),
    PosIntervals::integer,
    { PosIntervals == abs(Intervals) }
  },
  notePitch1(Dir, PosIntervals, Num, BaseLine, Note).

nextIntervals(Intervals, NextIntervals) :-
  NextIntervals::integer,
  { NextIntervals == Intervals - 1 }.

notePitch1(_, 0, _, BaseLine, Note) -->
  {
    setting(notehead_pos_eps, Eps),
    ccxOnSegCond(BaseLine, Note, Eps)
  }.
notePitch1(Dir, Intervals, Num, BaseLine, Note) -->
  { nextIntervals(Intervals, NextIntervals) },
  notePitch2(Dir, NextIntervals, Num, BaseLine, Note).

ledgerlineCond(+, Stafflines, BaseLine, Note, Num, NextNum, LedgerLine) :-
  % Baseline above the stafflines
  length(Stafflines, NumStafflines),
  {Num >= NumStafflines, NextNum == Num + 1},
  ledgerlineCond_(+, Stafflines, BaseLine, Note, LedgerLine).
ledgerlineCond(-, Stafflines, BaseLine, Note, Num, NextNum, LedgerLine) :-
  % Baseline below the stafflines
  {Num =< 1, NextNum == Num - 1},
  ledgerlineCond_(-, Stafflines, BaseLine, Note, LedgerLine).

ledgerlineCond_(Dir, Stafflines, BaseLine, Note, LedgerLine) :-
  ccxOrigin(Note, point(NoteX, _)),
  interlineAtX(Stafflines, NoteX, Interline),
  segYAtX(BaseLine, BaseLineY, NoteX),
  % BaseLineY +/- Interline
  BaseLineExpr =..[Dir, BaseLineY, -Interline],
  { BaseLineOffset == BaseLineExpr },
  segYAtX(LedgerLine, LedgerLineY, NoteX),
  setting(ledgerline_pos_eps, Eps),
  diffEps(Eps, LedgerLineY, BaseLineOffset).

notePitch2(Dir, 0, _, BaseLine, Note) -->
  selectchk(stafflines(Stafflines)),
  {
    ccxOrigin(Note, point(NoteX, NoteY)),
    interlineAtX(Stafflines, NoteX, Interline),
    segYAtX(BaseLine, BaseLineY, NoteX),
    % BaseLineY +/- (Interline / 2.0)
    Offset =..[Dir, BaseLineY, -(Interline / 2.0)],
    { Offset == MidBaseLineY },
    setting(notehead_pos_eps, Eps),
    diffEps(Eps, MidBaseLineY, NoteY)
  }.
notePitch2(Dir, Intervals, Num, BaseLine, Note) -->
  selectchk(stafflines(Stafflines)),
  {
    % Baseline inside the stafflines
    nth1(Num, Stafflines, BaseLine),
    % Num +/- 1
    NumExpr =..[Dir, Num, 1],
    { NextNum == NumExpr },
    nth1(NextNum, Stafflines, NewBaseLine),
    nextIntervals(Intervals, NextIntervals)
  },
  notePitch1(Dir, NextIntervals, NextNum, NewBaseLine, Note).
notePitch2(Dir, Intervals, Num, BaseLine, Note), [ledgerLine(LedgerLine)] -->
  selectchk(stafflines(Stafflines)),
  {
    ledgerlineCond(Dir, Stafflines, BaseLine, Note, Num, NextNum, LedgerLine),
    nextIntervals(Intervals, NextIntervals)
  },
  horizontalSeg(LedgerLine),
  notePitch1(Dir, NextIntervals, NextNum, LedgerLine, Note).
