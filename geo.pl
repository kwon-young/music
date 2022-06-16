:- module(
  geo, [
    diffEps/3, pointDiffEps/3, segYAtX/3, intersect/2
  ]).

:- use_module(library(clpBNR)).
:- use_module(seg).
:- use_module(ccx).

diffEps(Eps, A, B) :-
  debug(diffEps, "In Eps ~p, A ~p, B ~p~n", [Eps, A, B]),
  [A, B, Eps]::real,
  {
    Eps >= 0,
    abs(A - B) =< Eps
  },
  debug(diffEps, "Out Eps ~p, A ~p, B ~p~n", [Eps, A, B]).
pointDiffEps(Eps, point(X1, Y1), point(X2, Y2)) :-
  [Eps, X1, Y1, X2, Y2]::real,
  {
    Eps >= 0,
    sqrt((X2 - X1) ** 2 + (Y2 - Y1) ** 2) =< Eps
  }.

lineEq(Seg, point(X, Y), R) :-
  segStartEnd(Seg, point(X1, Y1), point(X2, Y2)),
  [X1, Y1, X2, Y2, X, Y, R]::real,
  { R == (Y2-Y1)*X + (X1-X2)*Y + (X2*Y1-X1*Y2) }.

boxArgs(box(LeftTop, RightBottom), [LeftTop, RightBottom]).

boxLeftTopRightBottom(Box, LeftTop, RightBottom) :-
  boxArgs(Box, [LeftTop, RightBottom]).

contour(Seg, Box) :-
  segStartEnd(Seg, Start, End),
  contour(Start, End, Box).
contour(Ccx, Box) :-
  ccxLeftTopRightBottom(Ccx, LeftTop, RightBottom),
  contour(LeftTop, RightBottom, Box).
contour(point(X1, Y1), point(X2, Y2), Box) :-
  [X1, Y1, X2, Y2, Xmin, Ymin, Xmax, Ymax]::real,
  {
    min(X1, X2) == Xmin,
    max(X1, X2) == Xmax,
    min(Y1, Y2) == Ymin,
    max(Y1, Y2) == Ymax
  },
  boxLeftTopRightBottom(Box, point(Xmin, Ymin), point(Xmax, Ymax)).

boxEq(box(point(X1, Y1), point(X2, Y2)), point(X, Y)) :-
  [X1, Y1, X2, Y2, X, Y]::real,
  {
    X >= X1,
    X =< X2,
    Y >= Y1,
    Y =< Y2
  }.

segEq(Seg, Point, R) :-
  lineEq(Seg, Point, R),
  contour(Seg, Box),
  boxEq(Box, Point).

segYAtX(Seg, Y, X) :-
  segEq(Seg, point(X, Y), 0).

intersect(Seg, Ccx) :-
  segEq(Seg, P, 0),
  contour(Ccx, Box),
  boxEq(Box, P).
