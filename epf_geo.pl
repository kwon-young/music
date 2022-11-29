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

cursor_state(term, Cursor) -->
  state(-(cursor, Cursor, noEl)).
cursor_state(select, Cursor) -->
  state(o(cursor, Cursor)).
cursor(Mod:Goal, Term) -->
  cursor_state(Goal, Cursor),
  cursor_(Cursor, Mod:Goal, Term).
cursor_(cursor(Term), _, Term) -->
  {true}.
cursor_(noEl, Goal, Term) -->
  call(Goal, Term).

:- meta_predicate find(1, ?, ?, ?).

find(Goal, Arg) -->
  state(o(cursor, Cursor)),
  find_(Cursor, Goal, Arg).
find_(cursor(_), Goal, Arg) -->
  call(Goal, Arg).
find_(noEl, Goal, Arg) -->
  term(Term),
  state(+(cursor, cursor(Term))),
  call(Goal, Arg).

horizontalSeg(Seg) -->
  { horizontalCond(Seg) },
  termp(Seg).

verticalSeg(Seg) -->
  { verticalCond(Seg) },
  termp(Seg).
