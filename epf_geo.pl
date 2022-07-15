:- module(epf_geo, [termp//1, selectp//1, horizontalSeg//1, verticalSeg//1]).

:- use_module(epf).
:- use_module(cond).
:- use_module(geo).

termp(Term) -->
  state(page, inside(Term)),
  term(Term),
  {debug(epf_geo, "termp Term ~p~n", [Term])}.
selectp(Term) -->
  state(page, inside(Term)),
  select(Term),
  {debug(epf_geo, "selectp Term ~p~n", [Term])}.

horizontalSeg(Seg) -->
  { horizontalCond(Seg) },
  termp(Seg).

verticalSeg(Seg) -->
  { verticalCond(Seg) },
  termp(Seg).
