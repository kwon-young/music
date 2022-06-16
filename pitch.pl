:- module(pitch, [notePitch//1]).

:- use_module(library(clpBNR)).
:- use_module(ccx).
:- use_module(geo).
:- use_module(epf).
:- use_module(epf_geo).
:- use_module(cond).
:- use_module(utils).
:- use_module(music_utils).
:- use_module(dom3).

:- use_module(library(settings)).
:- setting(notehead_pos_eps, number, 0, 'notehead vertical position').
:- setting(ledgerline_pos_eps, number, 0, 'ledgerline vertical position').

:- table pitches/1.

pitch(Pitch) :-
  member(Octave, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']),
  member(Step, ['C', 'D', 'E', 'F', 'G', 'A', 'B']),
  atomic_concat(Step, Octave, Pitch).
pitches(Pitches) :-
  findall(Pitch, pitch(Pitch), Pitches).

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

notePitchCond(Dir, NumIntervals, Step, Octave, BaseLine, BasePitch, Note, NotePitch) :-
  pitches(Pitches),
  dom(NotePitch, Pitches),
  when(
    (ground(NotePitch) ; ground(Step), ground(Octave)),
    atomic_concat(Step, Octave, NotePitch)),
  once(nth0(BaseN, Pitches, BasePitch)),
  getDom(NotePitch, NoteN, _),
  Intervals::integer,
  { Intervals == NoteN - BaseN },
  noteDirectionCond(Dir, BaseLine, Note, Intervals),
  { NumIntervals == abs(Intervals) }.

notePitch(element(pitch, [], [element(step, [], [Step]),
                              element(octave, [], [Octave])])) -->
  selectchk(baseLine(BaseLine)),
  state(basePitch, notehead, pitch, notePitchCond(Dir, Intervals, Step, Octave, BaseLine)),
  selectchk(num(Num)),
  notePitch1(Dir, Intervals, Num, BaseLine).

nextIntervals(Intervals, NextIntervals) :-
  NextIntervals::integer,
  { NextIntervals == Intervals - 1 }.

notePitch1Cond(BaseLine, Note) :-
  setting(notehead_pos_eps, Eps),
  ccxOnSegCond(BaseLine, Note, Eps).

notePitch1(_, 0, _, BaseLine) -->
  state(note, notePitch1Cond(BaseLine)).

notePitch1(Dir, Intervals, Num, BaseLine) -->
  { nextIntervals(Intervals, NextIntervals) },
  notePitch2(Dir, NextIntervals, Num, BaseLine).

notePitch2Cond(Dir, BaseLine, Stafflines, Note) :-
  ccxOrigin(Note, point(NoteX, NoteY)),
  interlineAtX(Stafflines, NoteX, Interline),
  segYAtX(BaseLine, BaseLineY, NoteX),
  % BaseLineY +/- (Interline / 2.0)
  Offset =..[Dir, BaseLineY, -(Interline / 2.0)],
  { Offset == MidBaseLineY },
  setting(notehead_pos_eps, Eps),
  diffEps(Eps, MidBaseLineY, NoteY).

nextNum(Dir, Num, NextNum, BaseLine, NewBaseLine, Stafflines) :-
  % Baseline inside the stafflines
  nth1(Num, Stafflines, BaseLine),
  % Num +/- 1
  NumExpr =..[Dir, Num, 1],
  { NextNum == NumExpr },
  nth1(NextNum, Stafflines, NewBaseLine).

ledgerlineCond(+, Num, NextNum, BaseLine, LedgerLine, Stafflines, Note) :-
  % Baseline above the stafflines
  length(Stafflines, NumStafflines),
  {Num >= NumStafflines, NextNum == Num + 1},
  ledgerlineCond_(+, Stafflines, BaseLine, Note, LedgerLine).
ledgerlineCond(-, Num, NextNum, BaseLine, LedgerLine, Stafflines, Note) :-
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

notePitch2(Dir, 0, _, BaseLine) -->
  state(stafflines, notehead, notePitch2Cond(Dir, BaseLine)).
notePitch2(Dir, Intervals, Num, BaseLine) -->
  state(stafflines, nextNum(Dir, Num, NextNum, BaseLine, NewBaseLine)),
  { nextIntervals(Intervals, NextIntervals) },
  notePitch1(Dir, NextIntervals, NextNum, NewBaseLine).
notePitch2(Dir, Intervals, Num, BaseLine), [ledgerLine(LedgerLine)] -->
  state(stafflines, notehead, ledgerlineCond(Dir, Num, NextNum, BaseLine, LedgerLine)),
  { nextIntervals(Intervals, NextIntervals) },
  horizontalSeg(LedgerLine),
  notePitch1(Dir, NextIntervals, NextNum, LedgerLine).
