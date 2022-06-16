:- module(constraint_math, [roundCons/2]).

:- use_module(library(clpBNR)).

sameSign(X, Y) :-
  { (X >= 0) == (Y >= 0) }.

sign(X, Sign) :-
  { Sign == X / abs(X) }.

ceilingCons(X, Y) :-
  Y::integer,
  sameSign(X, Y),
  {
    abs(Y) >= abs(X),
    abs(Y) - abs(X) =< 1
  }.

floorCons(X, Y) :-
  Y::integer,
  sameSign(X, Y),
  {
    abs(Y) =< abs(X),
    abs(X) - abs(Y) =< 1
  }.

roundCons(X, Y) :-
  Y::integer,
  sameSign(X, Y),
  { abs(X - Y) =< 0.5 }.

