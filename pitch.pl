:- module(pitch, [notePitch//2]).

:- use_module(library(clpBNR)).
:- use_module(ccx).
:- use_module(geo).
:- use_module(epf).
:- use_module(cond).
:- use_module(music_utils).

:- use_module(library(settings)).
:- setting(notehead_pos_eps, number, 0, 'notehead vertical position').
:- setting(ledgerline_pos_eps, number, 0, 'ledgerline vertical position').

pitches(['C', 'D', 'E', 'F', 'G', 'A', 'B']).

nextPitch(+, Pitch, Octave, NextPitch, NextOctave) :-
  nextPitch(Pitch, Octave, NextPitch, NextOctave).
nextPitch(-, Pitch, Octave, NextPitch, NextOctave) :-
  nextPitch(NextPitch, NextOctave, Pitch, Octave).
nextPitch(P1, O1, P2, O2) :-
  [O1, O2]::integer(0, 9),
  nextPitch_(P1, O1, P2, O2).
nextPitch_('B', Octave, 'C', NextOctave) :-
  {NextOctave == Octave + 1}.
nextPitch_(Pitch, Octave, NextPitch, Octave) :-
  pitches(Pitches),
  nextto(Pitch, NextPitch, Pitches).

nth0Constraint(N, L, El) :-
  ground(L),
  length(L, Len),
  [Len]::integer(0, inf),
  {Last == Len - 1},
  [N]::integer(0, Last),
  when(;(ground(N), ground(El)), user:nth0(N, L, El)).

comparePitch(P1, O1, Op, P2, O2) :-
  comparePitch_(Op, P1, O1, P2, O2).
comparePitch_(=, P, O, P, O).
comparePitch_(>, P1, O1, P2, O2) :-
  pitches(Pitches),
  nth0Constraint(N1, Pitches, P1),
  nth0Constraint(N2, Pitches, P2),
  [O1, O2]::integer(0, 9),
  {O1 * 10 + N1 > O2 * 10 + N2}.
comparePitch_(<, P1, O1, P2, O2) :-
  pitches(Pitches),
  nth0Constraint(N1, Pitches, P1),
  nth0Constraint(N2, Pitches, P2),
  [O1, O2]::integer(0, 9),
  {O1 * 10 + N1 < O2 * 10 + N2}.

ccxAboveSeg(Seg, Ccx) :-
  ccxOrigin(Ccx, point(CcxX, CcxY)),
  segYAtX(Seg, SegY, CcxX),
  { CcxY < SegY }.

ccxBelowSeg(Seg, Ccx) :-
  ccxOrigin(Ccx, point(CcxX, CcxY)),
  segYAtX(Seg, SegY, CcxX),
  { CcxY > SegY }.

noteDirectionCond(+, BaseLine, BasePitch, BaseOctave, Note, NotePitch, NoteOctave) :-
  comparePitch(NotePitch, NoteOctave, >, BasePitch, BaseOctave),
  ccxAboveSeg(BaseLine, Note).
noteDirectionCond(-, BaseLine, BasePitch, BaseOctave, Note, NotePitch, NoteOctave) :-
  comparePitch(NotePitch, NoteOctave, <, BasePitch, BaseOctave),
  ccxBelowSeg(BaseLine, Note).

notePitch(Note, element(pitch, [], [element(step, [], [NotePitch]),
                                    element(octave, [], [Octave])])) -->
  selectchk(baseLine(Num, BaseLine)),
  selectchk(basePitch(BasePitch, BaseOctave)),
  {
    when(;(ground(Octave), ground(NoteOctave)), atom_number(Octave, NoteOctave)),
    noteDirectionCond(Dir, BaseLine, BasePitch, BaseOctave, Note, NotePitch,
                      NoteOctave)
  },
  notePitch1(Dir, Num, BaseLine, BasePitch, BaseOctave, Note, NotePitch, NoteOctave).

notePitch1(_, _, BaseLine, NotePitch, NoteOctave, Note, NotePitch, NoteOctave) -->
  {
    setting(notehead_pos_eps, Eps),
    ccxOnSegCond(BaseLine, Note, Eps)
  }.
notePitch1(Dir, Num, BaseLine, BasePitch, BaseOctave, Note, NotePitch, NoteOctave) -->
  { nextPitch(Dir, BasePitch, BaseOctave, NextPitch, NextOctave) },
  notePitch2(Dir, Num, BaseLine, NextPitch, NextOctave, Note, NotePitch, NoteOctave).

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

notePitch2(Dir, _, BaseLine, NotePitch, NoteOctave, Note, NotePitch, NoteOctave) -->
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
notePitch2(Dir, Num, BaseLine, BasePitch, BaseOctave, Note, NotePitch, NoteOctave) -->
  selectchk(stafflines(Stafflines)),
  {
    % Baseline inside the stafflines
    nth1(Num, Stafflines, BaseLine),
    % Num +/- 1
    NumExpr =..[Dir, Num, 1],
    { NextNum == NumExpr },
    nth1(NextNum, Stafflines, NewBaseLine),
    nextPitch(Dir, BasePitch, BaseOctave, NextPitch, NextOctave)
  },
  notePitch1(Dir, NextNum, NewBaseLine, NextPitch, NextOctave, Note, NotePitch,
             NoteOctave).
notePitch2(Dir, Num, BaseLine, BasePitch, BaseOctave, Note, NotePitch, NoteOctave),
    [ledgerLine(LedgerLine)] -->
  selectchk(stafflines(Stafflines)),
  {
    ledgerlineCond(Dir, Stafflines, BaseLine, Note, Num, NextNum, LedgerLine),
    nextPitch(Dir, BasePitch, BaseOctave, NextPitch, NextOctave)
  },
  horizontalSeg(LedgerLine),
  notePitch1(Dir, NextNum, LedgerLine, NextPitch, NextOctave, Note, NotePitch,
             NoteOctave).
