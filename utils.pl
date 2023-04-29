:- module(utils, [convlist2/3, maplist2/2, maplist2/3, chain/2, chaing/2,
                  nth0u/3, nth1u/3, epsGround/3, lower_bound/2,
                  reify//2, reify/2,
                  add_id/1, add_id//1]).

:- use_module(library(delay)).
:- use_module(library(clpBNR)).

:- multifile delay:mode/1.

delay:mode(system:atom_number(ground, _)).
delay:mode(system:atom_number(_, ground)).

delay:mode(system:length(ground, _)).
delay:mode(system:length(_, ground)).

delay:mode(system:memberchk(_, list)).

delay:mode(system:compound_name_arity(nonvar, _, _)).
delay:mode(system:compound_name_arity(_, ground, nonvar)).

delay:mode(system:compound_name_arguments(nonvar, _, _)).
delay:mode(system:compound_name_arguments(_, ground, nonvar)).

:- meta_predicate convlist2(3, ?, ?).
:- meta_predicate maplist2(2, ?).
:- meta_predicate maplist2(4, ?, ?).

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
maplist2(Goal, [A1 | L1], [A2 | L2]) :-
  maplist2_(L1, L2, A1, A2, Goal).
maplist2_([], [], _, _, _).
maplist2_([B1 | L1], [B2 | L2], A1, A2, Goal) :-
  call(Goal, A1, B1, A2, B2),
  maplist2_(L1, L2, B1, B2, Goal).

product(L1, L2, L3) :-
  product(L2, L1, L1, L3).
product([], _, _, []).
product([B | L2], L1R, L1, L3) :-
  product_(L1R, [B | L2], L1, L3).
product_([], [_ | L2], L1, L3) :-
  product(L2, L1, L1, L3).
product_([A | L1R], [B | L2], L1, [A-B | L3]) :-
  product([B | L2], L1R, L1, L3).

chain(Rel, X, Prev, X) :-
  Expr =.. [Rel, Prev, X],
  {Expr}.

chain([A | List], Rel) :-
  foldl(chain(Rel), List, A, _).

:- meta_predicate chaing(?, 2).
:- meta_predicate chain_(2, ?, ?, ?).

chain_(Goal, B, A, B) :-
  call(Goal, A, B).
chaing([A | List], Goal) :-
  foldl(chain_(Goal), List, A, _).

:- use_module(library(clpBNR)).
:- use_module(library(reif)).

%if on the last element, do not search further
mycond([], 0, X, X, true).
%if the list contain at least one more element
%if either N or X and E are already assigned, do not backtrack since
% the list only contains unique elements
mycond([_ | _], N, X, E, T) :-
  (   once(N == 0 ; X == E)
  ->  N = 0, X = E, T = true
  ;   =(X, E, T), =(N, 0, T)
  ).

delay:mode(utils:nth1u(ground,ground,_)).
delay:mode(utils:nth1u(_,ground,ground)).

delay:mode(utils:nth0u(ground,ground,_)).
delay:mode(utils:nth0u(_,ground,ground)).

nth1u(N1, L, E) :-
  [N0, N1]::integer(0, _),
  {N1 == N0 + 1},
  nth0u(N0, L, E).

nth0u(N, L, E) :-
  N::integer(0, _),
  i_nth0u(L, N, E).

i_nth0u([X | Xs], N, E) :-
  if_(mycond(Xs, N, X, E), true, ({N == N1 + 1}, i_nth0u(Xs, N1, E))).

mult(X, Y, X*Y).
add(X, Y, X+Y).
joincoeff([], []).
joincoeff([_ | Lengths], [Coeff | Coeffs]) :-
  foldl(mult, Lengths, 1, Coeff),
  joincoeff(Lengths, Coeffs).

join(Vars, Z) :-
  maplist([Var, I, D]>>(get_attr(Var, dom, I-D)), Vars, Indexes, Domains),
  maplist(length, Domains, Lengths),
  joincoeff(Lengths, Coeffs),
  maplist(mult, Indexes, Coeffs, IndexesCoeffs),
  foldl(add, IndexesCoeffs, 0, Expr),
  get_attr(Z, dom, ZIndex-_),
  {ZIndex == Expr}.

epsGround(Eps, X, Y) :-
  ( delta(X, D), { D =< Eps }
  ->  Y is midpoint(X)
  ;   { Y == X }
  ).

lower_bound(N, X) :-
  ( interval(X)
  -> range(X, [V, _]),
     X is ceil(V * 10 * N) / (10.0 * N)
  ; true
  ).
delay:mode(pairs:pairs_keys_values(ground, _, _)).
delay:mode(pairs:pairs_keys_values(_, ground, _)).
delay:mode(pairs:pairs_keys_values(_, _, ground)).

:- meta_predicate reify(2, ?, ?, ?).
reify(Goal, Result, L, R) :-
   (  call(Goal, L, R)
   *-> Result = true
   ;  Result = false, L = R
   ).

:- meta_predicate reify(0, ?).
reify(Goal, Result) :-
   (  Goal
   *-> Result = true
   ;  Result = false
   ).

add_id(Id) :-
  ( var(Id)
  ->  gensym(id, Id)
  ; true
  ).

add_id(Id) -->
  {
    ( var(Id)
    ->  gensym(id, Id)
    ; true
    )
  }.
