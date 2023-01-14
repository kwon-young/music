:- module(cond, [ccxEtiqsCond/2, ccxEtiqsCond/3, ccxOnSegCond/3,
                 horizontalCond/1, verticalCond/1]).

:- use_module(library(clpBNR)).
:- use_module(ccx).
:- use_module(seg).
:- use_module(geo).

ccxEtiqsCond(Ccx, Etiq) :-
  ccxEtiqs(Ccx, Etiqs),
  nth0(0, Etiqs, _-Etiq).
ccxEtiqsCond(Ccx, N, Etiq) :-
  ccxEtiqs(Ccx, Etiqs),
  nth0(N, Etiqs, _-Etiq).

ccxOnSegCond(Seg, Ccx, Eps) :-
  ccxOrigin(Ccx, point(OriginX, OriginY)),
  segYAtX(Seg, SegY, OriginX),
  diffEps(Eps, OriginY, SegY).

horizontalCond(Seg) :-
  segStartEnd(Seg, point(X1, Y1), point(X2, Y2)),
  [X1, Y1, X2, Y2]::real,
  {abs(X2 - X1) > abs(Y2 - Y1)}.

verticalCond(Seg) :-
  segStartEnd(Seg, point(X1, Y1), point(X2, Y2)),
  [X1, Y1, X2, Y2]::real,
  {abs(X2 - X1) < abs(Y2 - Y1)}.
