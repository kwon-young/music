:- module(cond, [etiqsCond/2, etiqsCond/3, ccxOnSegCond/3, ccxWidthHeightCond/5]).

:- use_module(library(clpBNR)).
:- use_module(ccx).
:- use_module(seg).
:- use_module(geo).

etiqs(Ccx, Etiq) :-
  ccxEtiqs(Ccx, Etiq).
etiqs(Seg, Etiq) :-
  segEtiqs(Seg, Etiq).
etiqsCond(Ccx, Etiq) :-
  etiqs(Ccx, Etiqs),
  nth0(0, Etiqs, _-Etiq).
etiqsCond(Ccx, N, Etiq) :-
  etiqs(Ccx, Etiqs),
  nth0(N, Etiqs, _-Etiq).

ccxOnSegCond(Seg, Ccx, Eps) :-
  ccxOrigin(Ccx, point(OriginX, OriginY)),
  segYAtX(Seg, SegY, OriginX),
  eps(Eps, OriginY, SegY).

ccxWidthHeightCond(Ccx, Width, Height, Unit, Eps) :-
  ccxWidth(Ccx, CcxWidth),
  eps(Eps, CcxWidth, Width*Unit),
  ccxHeight(Ccx, CcxHeight),
  eps(Eps, CcxHeight, Height*Unit).
