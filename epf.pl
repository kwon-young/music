:- module(epf, [term//1, select//1, update//2,
                termchk//1, selectchk//1, selectchk//2, updatechk//2, add//1,
                apply//2, when//2,
                optional//3,
                sequence2//2, sequence2//3, sequence2//5,
                foldlg//4, foldlg//5, longuest_foldlg//5,
                longuest_sequence//2, longuest_sequences//2,
                longuest_notempty_sequence//4,
                longuest_notempty_sequences//3,
                longuest_notempty_sequence//2,
                longuest_notempty_sequence//1]).

:- use_module(library(clpBNR)).
:- use_module(state).
:- use_module(utils).
:- use_module(inkscape).

endMultiSegs(X, [Segs | _]) :-
  endMultiSeg(Segs, X).
endMultiSeg([H | T], X) :-
  ( H == X, var(T)
   -> T = []
  ; nonvar(T),
    endMultiSeg(T, X)
  ).
term_(Mode, X, [CurX | L], L) -->
   (  {Mode == chk ; (var(CurX), var(L))}
   -> !,
     (  valid_state, statep(endMultiSegs(X), [o(multiseg)])
     -> []
     ;  []
     )
   ;  []
   ),
   {CurX = X}.
   % {debug_highlight(term, X, 'green')}.
term_(Mode, X, [CurX | L], [CurX | R]) -->
   % {debug_highlight(term, CurX, 'red')},
   term_(Mode, X, L, R).

select_(Mode, X, L, [X | R]) -->
   term_(Mode, X, L, R).

add_(X, L, [X | L]) --> [].

pop_struct(Struct), [State] -->
  [State, Struct].
push_struct(Struct), [State, Struct] -->
  [State].

term(El) -->
  pop_struct(StructIn),
  term_(nochk, El, StructIn, StructOut),
  push_struct(StructOut).
termchk(El) -->
  pop_struct(StructIn),
  term_(chk, El, StructIn, StructOut),
  push_struct(StructOut).
select(El) -->
  pop_struct(StructIn),
  select_(nochk, El, StructIn, StructOut),
  push_struct(StructOut).
selectchk(El) -->
  pop_struct(StructIn),
  select_(chk, El, StructIn, StructOut),
  push_struct(StructOut).
selectchk(El1, El2) -->
  selectchk(El1),
  selectchk(El2).

update_(Goal, In, Out) -->
  call(Goal, In),
  add(Out).
update(In, Out) -->
  update_(term, In, Out).
updatechk(In, Out) -->
  update_(termchk, In, Out).

add(X) -->
  pop_struct(StructIn),
  add_(X, StructIn, StructOut),
  push_struct(StructOut).

:- meta_predicate sequence2(4, ?, ?, ?).

sequence2(Element, [Start1 | List1]) -->
  sequence2_(List1, Start1, Element).
sequence2_([B1 | List1], A1, P) -->
  call(P, A1, B1),
  !,
  sequence2_(List1, B1, P).
sequence2_([], _, _) --> {true}.

:- meta_predicate foldlg(5, ?, ?, ?, ?, ?).

foldlg(Goal, List, V0, V) -->
  foldlg_(List, Goal, V0, V).

foldlg_([H | T], Goal, V0, V) -->
  call(Goal, H, V0, V1),
  foldlg_(T, Goal, V1, V).
foldlg_([], _, V, V) --> [].

:- meta_predicate foldlg(5, ?, ?, ?, ?, ?, ?).

foldlg(Goal, List, V0, V, Rest) -->
  foldlg_(List, Goal, V0, V, Rest).

foldlg_([H | T], Goal, V0, V, Rest) -->
  call(Goal, H, V0, V1),
  foldlg_(T, Goal, V1, V, Rest).
foldlg_(Rest, _, V, V, Rest) --> [].

:- meta_predicate longuest_foldlg(5, ?, ?, ?, ?, ?, ?).

longuest_foldlg(Goal, List, V0, V, Rest) -->
  longuest_foldlg_(List, Goal, V0, V, Rest).

longuest_foldlg_(L, Goal, V0, V, Rest) -->
  reify(head_(Goal, L, V0, V1), Res),
  longuest_foldlg_(Res, L, Goal, V1, V, Rest).
longuest_foldlg_(true, [_ | T], G, V1, V, Rest) -->
  longuest_foldlg_(T, G, V1, V, Rest).
longuest_foldlg_(false, Rest, _, V, V, Rest) --> [].


:- meta_predicate sequence2(6, ?, ?, ?, ?).

sequence2(Element, [Start1 | List1], [Start2 | List2]) -->
  sequence2_(List1, List2, Start1, Start2, Element).
sequence2_([B1 | List1], [B2 | List2], A1, A2, P) -->
  call(P, A1, B1, A2, B2),
  !,
  sequence2_(List1, List2, B1, B2, P).
sequence2_([], [], _, _, _) --> {true}.

:- meta_predicate sequence2(:, ?, ?, ?, ?, ?, ?).

sequence2(Element, [S1 | L1], [S2 | L2], [S3 | L3], [S4 | L4]) -->
  sequence2_(L1, L2, L3, L4, S1, S2, S3, S4, Element).
sequence2_([B1 | L1], [B2 | L2], [B3 | L3], [B4 | L4], A1, A2, A3, A4, P) -->
  apply(P, [A1, B1, A2, B2, A3, B3, A4, B4]),
  !,
  sequence2_(L1, L2, L3, L4, B1, B2, B3, B4, P).
sequence2_([], [], [], [], _, _, _, _, _) --> {true}.

:- meta_predicate head_(3, ?, ?, ?).

head_(Goal, [H | _]) -->
  call(Goal, H).

:- meta_predicate head_(5, ?, ?, ?, ?, ?).

head_(Goal, [H | _], Arg1, Arg2) -->
  call(Goal, H, Arg1, Arg2).

:- meta_predicate apply(:, ?, ?, ?).

apply(Goal, Args, L, R) :-
  append(Args, [L, R], NewArgs),
  apply(Goal, NewArgs).

:- meta_predicate when(?, 2, ?, ?).

when(Cond, Mod:Goal, L, R) :-
  when(Cond, call(Mod:Goal, L, R)).

:- meta_predicate heads_(:, ?, ?, ?, ?).

heads_(Goal, SeqsIn, SeqsOut) -->
  { maplist(lists:selectchk, Args, SeqsIn, SeqsOut) },
  apply(Goal, Args).

:- meta_predicate optional(3, ?, ?, ?, ?).

optional(Goal, L, R) -->
  reify(head_(Goal, L), Result),
  optional_(Result, L, R).
optional_(true, [_ | L], L) --> [].
optional_(false, L, L) --> [].

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

:- meta_predicate longuest_notempty_sequence(?, 3, ?, ?, ?, ?).

longuest_notempty_sequence(PredN, Goal, SequenceIn, SequenceOut) -->
  state(+(PredN, 0)),
  longuest_notempty_sequence([], PredN, Goal, SequenceIn, SequenceOut).
longuest_notempty_sequence(Acc, PredN, Goal, SequenceIn, SequenceOut) -->
  nCond(PredN, _),
  reify(head_(Goal, SequenceIn), Result),
  longuest_notempty_sequence(Result, Acc, PredN, Goal, SequenceIn, SequenceOut).
longuest_notempty_sequence(true, Acc, PredN, Goal, [Element | SequenceIn], SequenceOut) -->
  longuest_notempty_sequence([Element | Acc], PredN, Goal, SequenceIn, SequenceOut).
longuest_notempty_sequence(false, [_ | _], _, _, Sequence, Sequence) -->
  [].

:- meta_predicate longuest_notempty_sequences(?, :, ?, ?, ?).

longuest_notempty_sequences(PredN, Goal, SeqsIn) -->
  state(+(PredN, 1)),
  heads_(Goal, SeqsIn, SeqsOut),
  longuest_notempty_sequences_(PredN, Goal, SeqsOut).
longuest_notempty_sequences_(PredN, Goal, SeqsIn) -->
  nCond(PredN, _),
  reify(heads_(Goal, SeqsIn, SeqsOut), Result),
  longuest_notempty_sequences_(Result, PredN, Goal, SeqsIn, SeqsOut).
longuest_notempty_sequences_(true, PredN, Goal, _, Seqs) -->
  longuest_notempty_sequences_(PredN, Goal, Seqs).
longuest_notempty_sequences_(false, _, _, Seqs, _) -->
  { maplist(=([]), Seqs) }.

:- meta_predicate longuest_notempty_sequence(?, 2, ?, ?).

longuest_notempty_sequence(PredN, Goal) -->
  state(+(PredN, 1)),
  Goal,
  longuest_notempty_sequence_(PredN, Goal).
longuest_notempty_sequence_(PredN, Goal) -->
  nCond(PredN, _),
  ( Goal
  *-> longuest_notempty_sequence_(PredN, Goal)
  ; []
  ).

:- meta_predicate longuest_notempty_sequence(2, ?, ?).

longuest_notempty_sequence(Goal) -->
  Goal,
  longuest_notempty_sequence_(Goal).
longuest_notempty_sequence_(Goal) -->
  ( Goal
  *-> longuest_notempty_sequence_(Goal)
  ; []
  ).

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
