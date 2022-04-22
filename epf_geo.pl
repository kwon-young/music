:- module(epf_geo, [horizontalSeg//1, verticalSeg//1]).

:- use_module(epf).
:- use_module(cond).

horizontalSeg(Seg) -->
  { horizontalCond(Seg) },
  term(Seg).

verticalSeg(Seg) -->
  { verticalCond(Seg) },
  term(Seg).
