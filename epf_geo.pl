:- module(epf_geo, [termp//1, terms//1, selectp//1, find//2, vertical_layout//3]).

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

in_bounding_box(Term, BBoxes) :-
  in_bounding_box_(BBoxes, Term).
in_bounding_box_([], _).
in_bounding_box_([BBox | _], Term) :-
  debug(in_bounding_box, "BBox ~p~n", [BBox]),
  debug(in_bounding_box, "Term ~p~n", [Term]),
  delay(inside(Term, BBox)).

termp(Term) -->
  statep(delay:delay(epf_geo:in_scope(Term)), [o(scope)]),
  statep(in_bounding_box(Term), [o(bbox)]),
  cursor(term, Term).
terms(Term) -->
  statep(delay:delay(epf_geo:in_scope(Term)), [o(scope)]),
  cursor(term, Term).

selectp(Term) -->
  statep(delay:delay(geo:inside(Term)), [o(page)]),
  statep(in_bounding_box(Term), [o(bbox)]),
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

:- meta_predicate vertical_layout(5, ?, ?, ?, ?).

vertical_layout(Goal, Margin, SequenceIn) -->
  state(o(bbox, [box(P, _) | _])),
  {Box = box(P, _)},
  reify(call(Goal, Box, SequenceIn, SequenceOut), Result),
  vertical_layout(Result, Goal, Margin, Box, SequenceIn, SequenceOut).

vertical_layoutCond(Margin, PrevBox, Box, Unit, Eps) :-
  PrevBox = box(point(X1, _), point(X2, Y2)),
  Box = box(point(X1, Y1), point(X2, _)),
  eps(Eps, Y2 + Margin*Unit, Y1).

vertical_layout(Goal, Margin, PrevBox, SequenceIn) -->
  statep(vertical_layoutCond(Margin, PrevBox, Box), [o(unit), o(eps)]),
  reify(call(Goal, Box, SequenceIn, SequenceOut), Result),
  vertical_layout(Result, Goal, Margin, Box, SequenceIn, SequenceOut).
vertical_layout(true, Goal, Margin, Box, _, Sequence) -->
  vertical_layout(Goal, Margin, Box, Sequence).
vertical_layout(false, _, _, _, [], _) -->
  [].
