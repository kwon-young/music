:- module(
  geo, [
    eps/3, eps/4, above/4, leftof/2, horizontalSeg/3, slope/2, segYAtX/3, inside/2,
    intersect/2,
    box/1, boxArgs/2, boxLeftTopRightBottom/3, boxWidth/2,
    remove_scopes/2,
    ground_elem/2
  ]).

:- use_module(library(clpBNR)).
:- use_module(library(delay)).
:- use_module(seg).
:- use_module(ccx).
:- use_module(utils).

eps(Eps, A) :-
  { A =< Eps }.
eps(Eps, A, B) :-
  debug(eps, "A ~p~n", [A]),
  debug(eps, "B ~p~n", [B]),
  debug(eps, "Eps ~p~n", [Eps]),
  { abs(A - B) =< Eps },
  debug(eps, "abs(~p - ~p) =< ~p~n", [A, B, Eps]).
eps(p, Eps, point(X1, Y1), point(X2, Y2)) :-
  eps(Eps, X1, X2),
  eps(Eps, Y1, Y2).
eps(px, Eps, point(X1, _), point(X2, _)) :-
  eps(Eps, X1, X2).
eps(py, Eps, point(_, Y1), point(_, Y2)) :-
  eps(Eps, Y1, Y2).

above(Dist, Eps, box(_, P1), box(P2, _)) :-
  above(Dist, Eps, P1, P2).
above(Dist, Eps, point(_, Y1), point(_, Y2)) :-
  {
    Y == Y1 + Dist,
    Y1 =< Y2
  },
  eps(Eps, Y, Y2).

leftof(point(X1, _), point(X2, _)) :-
  { X1 =< X2 }.

horizontalSeg(Eps, Unit, Seg) :-
  segHeight(Seg, Height),
  segLength(Seg, Length),
  { N == Length / Unit },
  eps(Eps, Height/N).


lineEq(Seg, point(X, Y), R) :-
  segStartEnd(Seg, point(X1, Y1), point(X2, Y2)),
  [X1, Y1, X2, Y2, X, Y, R]::real,
  { R == (Y2-Y1)*X + (X1-X2)*Y + (X2*Y1-X1*Y2) }.

slope(Seg, Slope) :-
  segStartEnd(Seg, point(X1, Y1), point(X2, Y2)),
  { Slope == (Y2 - Y1) / (X2 - X1) }.

boxArgs(box(LeftTop, RightBottom), [LeftTop, RightBottom]).
box(Box) :-
  boxArgs(Box, [point(X1, Y1), point(X2, Y2)]),
  {
    X1 =< X2,
    Y1 =< Y2
  }.

boxLeftTopRightBottom(Box, LeftTop, RightBottom) :-
  boxArgs(Box, [LeftTop, RightBottom]).

boxWidth(box(point(Left, _), point(Right, _)), Width) :-
  { Width == Right - Left }.

delay:mode(geo:contour(nonvar, _)).
contour(El, Box) :-
  delay(compound_name_arity(El, Name, _)),
  contour_(Name, El, Box).
contour_(seg, Seg, Box) :-
  segStartEnd(Seg, Start, End),
  contour(Start, End, Box).
contour_(ccx, Ccx, Box) :-
  ccxLeftTopRightBottom(Ccx, LeftTop, RightBottom),
  boxLeftTopRightBottom(Box, LeftTop, RightBottom).
contour_(box, Box, Box).
contour(point(X1, Y1), point(X2, Y2), Box) :-
  [X1, Y1, X2, Y2, Xmin, Ymin, Xmax, Ymax]::real,
  debug(contour, "~s: ~p~n", ["X1", X1]),
  debug(contour, "~s: ~p~n", ["X2", X2]),
  debug(contour, "~s: ~p~n", ["Xmin", Xmin]),
  debug(contour, "~s: ~p~n", ["Xmax", Xmax]),
  {
    min(X1, X2) == Xmin,
    max(X1, X2) == Xmax,
    min(Y1, Y2) == Ymin,
    max(Y1, Y2) == Ymax
  },
  debug(contour, "~s: ~p~n", ["X1", X1]),
  debug(contour, "~s: ~p~n", ["X2", X2]),
  debug(contour, "~s: ~p~n", ["Xmin", Xmin]),
  debug(contour, "~s: ~p~n", ["Xmax", Xmax]),
  boxLeftTopRightBottom(Box, point(Xmin, Ymin), point(Xmax, Ymax)).

boxEq(box(point(X1, Y1), point(X2, Y2)), point(X, Y)) :-
  [X1, Y1, X2, Y2, X, Y]::real,
  debug(boxEq, "~p, ~p~n", [box(point(X1, Y1), point(X2, Y2)), point(X, Y)]),
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

vdistanceAtX(X, Seg1, Seg2, Distance) :-
  segYAtX(Seg1, Y1, X),
  segYAtX(Seg2, Y2, X),
  {Distance == Y2 - Y1}.

delay:mode(geo:inside(nonvar, nonvar)).
inside(Term, Container) :-
  contour(Term, box(P1, P2)),
  contour(Container, Box),
  boxEq(Box, P1),
  boxEq(Box, P2).

ground_elem(Elem, GroundElem) :-
  term_variables(Elem, InVars),
  copy_term_nat(InVars, Elem, OutVars, GroundElem),
  % include(interval, InVars, Intervals),
  % solve(Intervals),
  maplist([X, Y]>>((interval(X) -> domain(X, Y) ; X = Y)), InVars, OutVars),
  etiqs(Elem, Etiqs),
  length(Etiqs, _).
etiqs(ccx(_, _, Etiqs, _), Etiqs).
etiqs(seg(_, _, Etiqs, _), Etiqs).
etiqs(ccx(P1, P2, Etiqs, O), ccx(P1, P2, EtiqsNoScope, O), Etiqs, EtiqsNoScope).
etiqs(seg(P1, P2, Etiqs, T), seg(P1, P2, EtiqsNoScope, T), Etiqs, EtiqsNoScope).

remove_scopes(In, Out) :-
  open(In, read, SIn),
  read(SIn, Struct),
  close(SIn),
  maplist(etiqs, Struct, StructNoScope, Etiqs, EtiqsNoScope),
  maplist(maplist([InId-Label, OutId-Label]>>(Label == page -> InId=OutId ; true)),
          Etiqs, EtiqsNoScope),
  open(Out, write, SOut),
  print_term(StructNoScope, [output(SOut)]),
  write(SOut, "."),
  close(SOut).

:- begin_tests(ccx).

contour_gen(_, _).
contour_gen(Seg, _) :-
  seg(Seg).
contour_gen(Ccx, _) :-
  ccx(Ccx).
test('delayed_contour', [forall(contour_gen(Container, Box))]) :-
  delay(contour(Container, Box)).

test('boxEq', []) :-
  [X1, X2]::real(0, 2100),
  [Y1, Y2]::real(0, 2970),
  boxEq(box(point(X1, Y1), point(X2, Y2)), P1),
  P1 = point(50.0, 104.0),
  boxEq(box(point(X1, Y1), point(X2, Y2)), P2),
  P2 = point(301.8, 104.0).

% test('contour', []) :-
%   [X1, X2]::real(0, 2100),
%   [Y1, Y2]::real(0, 2970),
%   Cont = ccx(point(X1, Y1), point(X2, Y2), _, _),
%   contour(Cont, Box),
%   Term = seg(_, _, _, _),
%   contour(Term, box(P1, P2)),
%   boxEq(Box, P1),
%   Term = seg(point(50.0, 104.0), point(301.8, 104.0), _, _).
%
% test('inside', []) :-
%   [X1, X2]::real(0, 2100),
%   [Y1, Y2]::real(0, 2970),
%   Cont = ccx(point(X1, Y1), point(X2, Y2), _, _),
%   Term = seg(_, _, _, _),
%   inside(Term, Cont),
%   Term = seg(point(50.0, 104.0), point(301.8, 104.0), _, _),
%   {
%     X1 =< X2,
%     Y1 =< Y2
%   }.

:- end_tests(ccx).
