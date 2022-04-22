:- module(cond, [ccxEtiqsCond/2, ccxOnSegCond/3, horizontalCond/1, verticalCond/1]).

ccxEtiqsCond(Ccx, Etiq) :-
  ccxEtiqs(Ccx, Etiqs),
  memberchk(Etiq, Etiqs).

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
