:- module(utils, [convlist2/3, maplist2/2, range/2, arange/3]).

:- meta_predicate convlist2(3, ?, ?).
:- meta_predicate maplist2(2, ?).

convlist2(Goal, List, Res) :-
  convlist2_(List, Res, Goal).
convlist2_([_], [], _).
convlist2_([A, B | List], [C | Res], Goal) :-
  call(Goal, A, B, C),
  convlist2_([B | List], Res, Goal).

maplist2(Goal, List) :-
  maplist2_(List, Goal).
maplist2_([_], _).
maplist2_([A, B | List], Goal) :-
  call(Goal, A, B),
  maplist2_([B | List], Goal).

arange(High, Range) :-
  arange(0, High, Range).
arange(Low, High, Range) :-
  bagof(X, between(Low, High, X), Range).
