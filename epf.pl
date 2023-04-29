:- module(epf, [term//1, select//1, update//2,
                termchk//1, selectchk//1, selectchk//2, updatechk//2,
                add//1,
                sequence2//2, sequence2//3, sequence2//5,
                longuest_sequence//2, longuest_sequences//2,
                longuest_notempty_sequence//3]).

:- use_module(library(clpBNR)).
:- use_module(utils).

term_(Mode, X, [CurX | L], L) :-
   (  (Mode == chk ; (var(CurX), var(L)))
   -> !
   ;  true
   ),
   CurX = X.
term_(Mode, X, [CurX | L], [CurX | R]) :-
   term_(Mode, X, L, R).

select_(Mode, X, L, [X | R]) :-
   term_(Mode, X, L, R).

term(El), [State, StructOut] -->
  [State, StructIn],
  { term_(nochk, El, StructIn, StructOut) }.
termchk(El), [State, StructOut] -->
  [State, StructIn],
  { term_(chk, El, StructIn, StructOut) }.
select(El), [State, StructOut] -->
  [State, StructIn],
  { select_(nochk, El, StructIn, StructOut) }.
selectchk(El), [State, StructOut] -->
  [State, StructIn],
  { select_(chk, El, StructIn, StructOut) }.
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

:- meta_predicate head_(3, ?, ?, ?).

head_(Goal, [H | _]) -->
  call(Goal, H).

:- meta_predicate longuest_sequence(3, ?, ?, ?).

longuest_sequence(Goal, Sequence) -->
  reify(head_(Goal, Sequence), Result),
  longuest_sequence(Result, Goal, Sequence).
longuest_sequence(true, Goal, [_ | Sequence]) -->
  longuest_sequence(Goal, Sequence).
longuest_sequence(false, _, []) -->
  [].

:- meta_predicate longuest_sequences(4, ?, ?, ?).

longuest_sequences(Goal, SequenceIn) -->
  reify(call(Goal, SequenceIn, SequenceOut), Result),
  longuest_sequences(Result, Goal, SequenceIn, SequenceOut).
longuest_sequences(true, Goal, _, Sequence) -->
  longuest_sequences(Goal, Sequence).
longuest_sequences(false, _, [], _) -->
  [].

:- meta_predicate longuest_notempty_sequence(3, ?, ?, ?, ?).

longuest_notempty_sequence(Goal, SequenceIn, SequenceOut) -->
  longuest_notempty_sequence([], Goal, SequenceIn, SequenceOut).
longuest_notempty_sequence(Acc, Goal, SequenceIn, SequenceOut) -->
  reify(head_(Goal, SequenceIn), Result),
  longuest_notempty_sequence(Result, Acc, Goal, SequenceIn, SequenceOut).
longuest_notempty_sequence(true, Acc, Goal, [Element | SequenceIn], SequenceOut) -->
  longuest_notempty_sequence([Element | Acc], Goal, SequenceIn, SequenceOut).
longuest_notempty_sequence(false, [_ | _], _, Sequence, Sequence) -->
  [].

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

ab(a) --> [a].
ab(b) --> [b].

test('longuest_sequence') :-
  L = [a, b, a],
  phrase(longuest_sequence(ab, L), L),
  phrase(longuest_sequence(ab, L), L2),
  L == L2,
  phrase(longuest_sequence(ab, L3), L),
  L == L3.

:- end_tests(epf).
