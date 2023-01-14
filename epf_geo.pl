:- module(epf_geo, [termp//1, selectp//1, find//2, horizontalSeg//1, verticalSeg//1]).

:- use_module(library(delay)).
:- use_module(ccx).
:- use_module(seg).
:- use_module(epf).
:- use_module(state).
:- use_module(cond).
:- use_module(geo).
:- use_module(utils).

delay:mode(epf_geo:in_scope(nonvar, _)).
in_scope(Term, Scopes) :-
  delay(compound_name_arity(Term, Name, _)),
  in_scope(Name, Term, Scopes).
in_scope(seg, Seg, Scopes) :-
  segEtiqs(Seg, Etiqs),
  maplist(in_scope_end(Etiqs), Scopes).
in_scope(ccx, Ccx, Scopes) :-
  ccxEtiqs(Ccx, Etiqs),
  maplist(in_scope_end(Etiqs), Scopes).
in_scope_end(Etiqs, Scope) :-
  delay(memberchk(Scope, Etiqs)).



termp(Term) -->
  statep(delay:delay(geo:inside(Term)), [o(page)]),
  statep(delay:delay(epf_geo:in_scope(Term)), [o(scope)]),
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
