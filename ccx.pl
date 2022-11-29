:- module(
  ccx, [
    ccx/1,
    ccxEtiqs/2, ccxOrigin/2, ccxLeftTop/2, ccxRightBottom/2,
    ccxLeftTopRightBottom/3, ccxTopBottom/3, ccxLeftRight/3,
    ccxRight/2, ccxLeft/2, ccxTop/2, ccxBottom/2, ccxWidth/2, ccxHeight/2
  ]).

:- use_module(library(clpBNR)).

ccx(Ccx) :-
  ccxArgs(Ccx, [point(X1, Y1), point(X2, Y2), _, point(X, Y)]),
  [X, Y, X1, Y1, X2, Y2]::real,
  {
    X1 =< X2,
    Y1 =< Y2
  }.
ccxArgs(ccx(LeftTop, RightBottom, Etiqs, Origin), [LeftTop, RightBottom, Etiqs, Origin]).

ccxEtiqs(Ccx, Etiqs) :-
  ccxArgs(Ccx, [_, _, Etiqs | _]).

ccxOrigin(Ccx, Origin) :-
  ccxArgs(Ccx, [_, _, _, Origin]).

ccxLeftTop(Ccx, LeftTop) :-
  ccxArgs(Ccx, [LeftTop | _]).

ccxRightBottom(Ccx, RightBottom) :-
  ccxArgs(Ccx, [_, RightBottom | _]).

ccxLeftTopRightBottom(Ccx, LeftTop, RightBottom) :-
  ccxLeftTop(Ccx, LeftTop),
  ccxRightBottom(Ccx, RightBottom).

ccxTopBottom(Ccx, Top, Bottom) :-
  ccxTop(Ccx, Top),
  ccxBottom(Ccx, Bottom).

ccxLeftRight(Ccx, Left, Right) :-
  ccxLeft(Ccx, Left),
  ccxRight(Ccx, Right).

ccxRight(Ccx, Right) :-
  ccxRightBottom(Ccx, point(Right, _)).

ccxBottom(Ccx, Bottom) :-
  ccxRightBottom(Ccx, point(_, Bottom)).

ccxLeft(Ccx, Left) :-
  ccxLeftTop(Ccx, point(Left, _)).

ccxTop(Ccx, Top) :-
  ccxLeftTop(Ccx, point(_, Top)).

ccxWidth(Ccx, Width) :-
  ccxRight(Ccx, Right),
  ccxLeft(Ccx, Left),
  { Width == Right - Left }.

ccxHeight(Ccx, Height) :-
  ccxBottom(Ccx, Bottom),
  ccxTop(Ccx, Top),
  { Height == Bottom - Top }.
