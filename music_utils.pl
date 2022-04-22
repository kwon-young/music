:- module(music_utils, [interlineAtX/3]).

:- use_module(library(clpBNR)).
:- use_module(seg).
:- use_module(utils).

listSum([],0).
listSum([X | Xs],Sum) :-
  {Sum == X+S},
  listSum(Xs,S).

average(List, Average) :-
  listSum(List, Sum),
  length(List, Length),
  {Average == Sum / Length}.

interlineAtX_(X, Seg1, Seg2, Interline) :-
  segYAtX(Seg1, Y1, X),
  segYAtX(Seg2, Y2, X),
  { Interline == abs(Y2 - Y1) }.
interlineAtX(Stafflines, X, Interline) :-
  convlist2(interlineAtX_(X), Stafflines, Interlines),
  average(Interlines, Interline).
