:- module(utils, [convlist2/3, maplist2/2]).

:- meta_predicate convlist2(2, ?, ?).
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
