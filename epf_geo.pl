:- module(epf_geo, [termp//1, selectp//1, find//2, horizontalSeg//1, verticalSeg//1]).

:- use_module(epf).
:- use_module(cond).
:- use_module(geo).

termp(Term) -->
  state(page, inside(Term)),
  cursor(term, Term),
  {debug(epf_geo, "termp Term ~p~n", [Term])}.

selectp(Term) -->
  state(page, inside(Term)),
  cursor(select, Term),
  {debug(epf_geo, "selectp Term ~p~n", [Term])}.

:- meta_predicate cursor(1, ?, ?, ?).

cursor(_Goal, Term) -->
  updatechk(cursor(Cursor), cursor(noEl)),
  {
    dif(Cursor, noEl),
    Term = Cursor
  }.
cursor(Goal, Term) -->
  selectchk(cursor(noEl)),
  call(Goal, Term).

:- meta_predicate find(1, ?, ?, ?).

find(Goal, Arg) -->
  termp(Term),
  updatechk(cursor(noEl), cursor(Term)),
  call(Goal, Arg).

horizontalSeg(Seg) -->
  { horizontalCond(Seg) },
  termp(Seg).

verticalSeg(Seg) -->
  { verticalCond(Seg) },
  termp(Seg).
