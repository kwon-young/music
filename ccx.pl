:- module(
  ccx, [
    ccxEtiqs/2, ccxOrigin/2
  ]).

ccxArgs(ccx(LeftTop, RightBottom, Etiqs, Origin), [LeftTop, RightBottom, Etiqs, Origin]).

ccxEtiqs(Ccx, Etiqs) :-
  ccxArgs(Ccx, [_, _, Etiqs | _]).

ccxOrigin(Ccx, Origin) :-
  ccxArgs(Ccx, [_, _, _, Origin]).

ccxTopBottom(Ccx, Top, Bottom) :-
  ccxArgs(Ccx, [point(_, Top), point(_, Bottom) | _]).
