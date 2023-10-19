:- module(epf_geo, [termp//1, terms//1, selectp//1, find//2,
                    % vertical_layout_seq//3,
                    vertical_layout_seq//4, vertical_layout_args//3]).

:- use_module(library(delay)).
:- use_module(library(clpBNR)).
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
  debug(in_bounding_box, "Term pre ~p~n", [Term]),
  delay(inside(Term, BBox)),
  debug(in_bounding_box, "Term post ~p~n", [Term]).

contourCond(Term, OldContours, NewContours) :-
  when(nonvar(Term), contour(Term, Contour)),
  maplist(union(Contour), OldContours, NewContours).

termp(Term) -->
  statep(delay:delay(epf_geo:in_scope(Term)), [o(scope)]),
  statep(in_bounding_box(Term), [o(bbox)]),
  statep(contourCond(Term), [-(contour)]),
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

vertical_layoutCond(Margin, PrevBox, Box, Unit, Eps) :-
  debug(vertical_layoutCond, "Margin ~p~n", [Margin]),
  PrevBox = box(point(X1, _), point(X2, Y2)),
  Box = box(point(X1, Y1), point(X2, _)),
  eps(Eps, Y2 + Margin*Unit, Y1),
  debug(vertical_layoutCond, "Y1 ~p~n", [Y1]).

:- meta_predicate vertical_layout_seq(4, ?, ?, ?, ?, ?).

vertical_layout_seq(Goal, Margin, SequenceIn, SequenceOut) -->
  reify(bbox(call(Goal, SequenceIn, SequenceTmp), Box), Result),
  vertical_layout_seq(Result, Goal, Margin, Box, SequenceIn, SequenceTmp, SequenceOut).

vertical_layout_seq(Goal, Margin, PrevBox, SequenceIn, SequenceOut) -->
  statep(vertical_layoutCond(Margin, PrevBox, Box), [o(unit), o(eps)]),
  reify(bbox(call(Goal, SequenceIn, SequenceTmp), Box), Result),
  vertical_layout_seq(Result, Goal, Margin, Box, SequenceIn, SequenceTmp, SequenceOut).

vertical_layout_seq(true, Goal, Margin, Box, _, SequenceIn, SequenceOut) -->
  vertical_layout_seq(Goal, Margin, Box, SequenceIn, SequenceOut).
vertical_layout_seq(false, _, _, _, Sequence, _, Sequence) -->
  [].

head_tail([H | T], H, T).
heads_tails(ArgsN, Heads, Tails) :-
  maplist(head_tail, ArgsN, Heads, Tails).

:- meta_predicate vertical_layout_args(:, ?, ?, ?, ?).

vertical_layout_args(Goal, Margin, ArgsN) -->
  reify(bbox(and({ heads_tails(ArgsN, Args, RemainingArgs) }, epf:apply(Goal, Args)), Box), Result),
  vertical_layout_args(Result, Goal, Margin, Box, ArgsN, Args, RemainingArgs).

and(A, B) -->
  A, B.

vertical_layout_args(Goal, Margin, PrevBox, ArgsN) -->
  statep(vertical_layoutCond(Margin, PrevBox, Box), [o(unit), o(eps)]),
  reify(bbox(epf_geo:and({ heads_tails(ArgsN, Args, RemainingArgs) }, epf:apply(Goal, Args)), Box), Result),
  vertical_layout_args(Result, Goal, Margin, Box, ArgsN, Args, RemainingArgs).
vertical_layout_args(true, Goal, Margin, Box, _, _, RemainingArgs) -->
  vertical_layout_args(Goal, Margin, Box, RemainingArgs).
vertical_layout_args(false, _, _, _, ArgsN, _, _) -->
  { maplist(=([]), ArgsN) }.

with(Name, Goal, New) -->
  state(-(Name, Old, [New | Old])),
  Goal,
  state(-(Name, [New | Old], Old)).
