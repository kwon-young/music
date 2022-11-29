:- module(epf_geo, [termp//1, selectp//1, find//2, horizontalSeg//1, verticalSeg//1]).

:- use_module(library(delay)).
:- use_module(epf).
:- use_module(state).
:- use_module(cond).
:- use_module(geo).

termp(Term) -->
  statep(delay:delay(geo:inside(Term)), [o(page)]),
  cursor(term, Term),
  {debug(epf_geo, "termp Term ~p~n", [Term])}.

selectp(Term) -->
  statep(delay:delay(geo:inside(Term)), [o(page)]),
  cursor(select, Term),
  {debug(epf_geo, "selectp Term ~p~n", [Term])}.

:- meta_predicate cursor(3, ?, ?, ?).

cursor(_Mod:term, Term) -->
  updatechk(cursor(Cursor), cursor(noEl)),
  {
    dif(Cursor, noEl),
    Term = Cursor
  }.
cursor(_Mod:select, Term) -->
  updatechk(cursor(Cursor), cursor(Cursor)),
  {
    dif(Cursor, noEl),
    Term = Cursor
  }.
cursor(Goal, Term) -->
  selectchk(cursor(noEl)),
  call(Goal, Term).

:- meta_predicate find(1, ?, ?, ?).

find(Goal, Arg) -->
  selectchk(cursor(El)),
  { dif(El, noEl) },
  call(Goal, Arg).
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
