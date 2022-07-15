:- module(music_utils, [
  interlineAtStart/2, interlineAtEnd/2, interlineAtX/3,
  interlineAt/3, interlineAt/4,
  listSum/2, average/2]).

:- use_module(library(clpBNR)).
:- use_module(seg).
:- use_module(geo).
:- use_module(utils).

listSum([],0).
listSum([X | Xs],Sum) :-
  {Sum == X+S},
  listSum(Xs,S).

average(List, Average) :-
  listSum(List, Sum),
  length(List, Length),
  {Average == Sum / Length}.

distance(X, Y, Z) :-
  { Z == abs(Y - X) }.

interlineAtStart(Stafflines, Interline) :-
  interlineAt(Stafflines, segStartY, Interline).
interlineAtEnd(Stafflines, Interline) :-
  interlineAt(Stafflines, segEndY, Interline).
interlineAtX(Stafflines, X, Interline) :-
  debug(music_utils, "interlineAtX Stafflines ~p~n", [Stafflines]),
  interlineAt(Stafflines, {X}/[Seg, Y]>>(segYAtX(Seg, Y, X)), Interline).
interlineAt(Stafflines, Getter, Interline) :-
  interlineAt(Stafflines, Getter, Interline, _).
interlineAt(Stafflines, Getter, Interline, Interlines) :-
  maplist(Getter, Stafflines, Ys),
  convlist2(distance, Ys, Interlines),
  average(Interlines, Interline).
