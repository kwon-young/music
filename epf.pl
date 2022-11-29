:- module(epf, [term//1, select//1, update//2,
                termchk//1, selectchk//1, selectchk//2, updatechk//2,
                add//1,
                sequence2//2, sequence2//3, sequence2//5]).

:- use_module(library(clpBNR)).

term_(nochk, El) -->
  [El].
term_(chk, El) -->
  [El], !.
term_(Mode, El), [CurEl] -->
  [CurEl],
  term_(Mode, El).
select_(Mode, El), [El] -->
  term_(Mode, El).

term(El), [State, StructOut] -->
  [State, StructIn],
  { phrase(term_(nochk, El), StructIn, StructOut) }.
termchk(El), [State, StructOut] -->
  [State, StructIn],
  { phrase(term_(chk, El), StructIn, StructOut) }.
select(El), [State, StructOut] -->
  [State, StructIn],
  { phrase(select_(nochk, El), StructIn, StructOut) }.
selectchk(El), [State, StructOut] -->
  [State, StructIn],
  { phrase(select_(chk, El), StructIn, StructOut) }.
selectchk(El1, El2) -->
  selectchk(El1),
  selectchk(El2).

update_(Goal, In, Out), [Out] -->
  call(Goal, In).
update(In, Out) -->
  update_(term, In, Out).
updatechk(In, Out) -->
  update_(termchk, In, Out).

add(El), [El] -->
  { true }.

:- meta_predicate sequence2(4, ?, ?, ?).

sequence2(Element, [Start1 | List1]) -->
  sequence2_(List1, Start1, Element).
sequence2_([B1 | List1], A1, P) -->
  call(P, A1, B1),
  !,
  sequence2_(List1, B1, P).
sequence2_([], _, _) --> {true}.

:- meta_predicate sequence2(6, ?, ?, ?, ?).

sequence2(Element, [Start1 | List1], [Start2 | List2]) -->
  sequence2_(List1, List2, Start1, Start2, Element).
sequence2_([B1 | List1], [B2 | List2], A1, A2, P) -->
  call(P, A1, B1, A2, B2),
  !,
  sequence2_(List1, List2, B1, B2, P).
sequence2_([], [], _, _, _) --> {true}.

:- meta_predicate sequence2(9, ?, ?, ?, ?, ?, ?).

sequence2(Element, [S1 | L1], [S2 | L2], [S3 | L3], [S4 | L4]) -->
  sequence2_(L1, L2, L3, L4, S1, S2, S3, S4, Element).
sequence2_([B1 | L1], [B2 | L2], [B3 | L3], [B4 | L4], A1, A2, A3, A4, P) -->
  call(P, A1, B1, A2, B2, A3, B3, A4, B4),
  !,
  sequence2_(L1, L2, L3, L4, B1, B2, B3, B4, P).
sequence2_([], [], [], [], _, _, _, _, _) --> {true}.

:- begin_tests(epf).

test('term') :-
  findall(X, phrase(term(X), [_, [a]], [_, []]), Xs),
  Xs == [a].

test('termchk') :-
  phrase(termchk(a), [_, [a]], [_, []]).

test('select') :-
  findall(X, phrase(select(X), [_, [a]], [_, [a]]), Xs),
  Xs == [a].

test('selectchk') :-
  phrase(selectchk(a), [_, [a]], [_, [a]]).

:- end_tests(epf).
