:- module(cond, [ccxEtiqsCond/2, ccxEtiqsCond/3, ccxOnSegCond/3]).

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
  eps(Eps, OriginY, SegY).
