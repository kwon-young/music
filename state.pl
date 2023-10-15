:- module(state, [makeState/2, state//1, statep//2,
                  scope//1, scope//2, scope//3, scope//4, pop_scope//1, push_scope//2,
                  bbox//2, nCond/2, nCond/3, nCond//2,
                  add_id//1, ground_all_ids/1]).

:- use_module(library(rbtrees)).
:- use_module(library(clpBNR)).
:- use_module(library(dcg/high_order)).
:- use_module(geo).
:- use_module(utils).

makeState(state(Tree), List) :-
  list_to_rbtree([cursor-noEl, scope-[], bbox-[], ids-[] | List], Tree).

state(Term) -->
  stateValues(Term, _).
stateValues(Term, Values), [state(StateOut)] -->
  [state(StateIn)],
  { phrase(state_(Term, Values), [StateIn], [StateOut]) }.

state_(o(Key), Values) -->
  state_(o(Key, _), Values).
state_(o(Key, Value), [Value]), [State] -->
  [State],
  {
    ( rb_lookup(Key, Value, State)
    -> true
    ;  existence_error(key, Key, State)
    )
  }.

state_(+(Key), Values) -->
  state_(+(Key, _), Values).
state_(+(Key, Value), [Value]), [StateOut] -->
  [StateIn],
  { rb_insert(StateIn, Key, Value, StateOut) }.

state_(-(Key), Values) -->
  state_(-(Key, _, _), Values).
state_(-(Key, OldValue, NewValue), [OldValue, NewValue]), [StateOut] -->
  [StateIn],
  { rb_update(StateIn, Key, OldValue, NewValue, StateOut) }.

state_([], []) --> [].
state_([Term | Terms], [Values]) -->
  sequence3(state_, [Term | Terms], ListValues),
  { append(ListValues, Values) }.

sequence3(Goal, L1, L2) -->
  sequence3_(L1, L2, Goal).
sequence3_([A | L1], [B | L2], Goal) -->
  call(Goal, A, B),
  sequence3_(L1, L2, Goal).
sequence3_([], [], _Goal) -->
  [].

nCond(PrevN, N) :-
  N::integer(1, _),
  { N == PrevN + 1 }.

nCond(NAtom, PrevN, N) :-
  nCond(PrevN, N),
  atom_number(NAtom, N).

nCond(State, NAtom) -->
  statep(nCond(NAtom), [-(State)]).

:- meta_predicate add_args(:, ?, ?).

add_args(delay:delay(Goal), Args, delay:delay(NewGoal)) :-
  !,
  add_args(Goal, Args, NewGoal).
add_args(Mod:Goal, Args, Mod:NewGoal) :-
  Goal =.. GoalList,
  append(GoalList, Args, NewGoalList),
  NewGoal =.. NewGoalList.

:- meta_predicate statep(:, ?, ?, ?).

statep(Goal, KeyValues) -->
  stateValues(KeyValues, ListValues),
  {
    append(ListValues, Values),
    add_args(Goal, Values, NewGoal),
    call(NewGoal)
  }.

:- meta_predicate scope(3, ?, ?).

scope(Mod:Goal) -->
  state(-(scope, Scopes, [Scope-Name | Scopes])),
  { Goal =.. [Name | _] },
  call(Mod:Goal, Scope),
  state(-(scope, [Scope-Name | Scopes], Scopes)).

:- meta_predicate scope(4, ?, ?, ?).

scope(Goal, Arg) -->
  { add_args(Goal, [Arg], NewGoal) },
  scope(NewGoal).

:- meta_predicate scope(5, ?, ?, ?, ?).

scope(Goal, Arg1, Arg2) -->
  { add_args(Goal, [Arg1, Arg2], NewGoal) },
  scope(NewGoal).

:- meta_predicate scope(6, ?, ?, ?, ?, ?).

scope(Goal, Arg1, Arg2, Arg3) -->
  { add_args(Goal, [Arg1, Arg2, Arg3], NewGoal) },
  scope(NewGoal).

:- meta_predicate pop_scope(2, ?, ?).

pop_scope(Goal) -->
  state(-(scope, [Scope | Scopes], Scopes)),
  Goal,
  state(-(scope, Scopes, [Scope | Scopes])).

:- meta_predicate push_scope(2, ?, ?, ?).

push_scope(Goal, Scope) -->
  state(-(scope, Scopes, [Scope-_ | Scopes])),
  Goal,
  state(-(scope, [Scope-_ | Scopes], Scopes)).


:- meta_predicate bbox(2, ?, ?, ?).

bbox(Goal, BBox) -->
  state(-(bbox, [Parent | BBoxes], [BBox, Parent | BBoxes])),
  {
    box(BBox),
    inside(BBox, Parent),
    debug(bbox, "bbox ~p~n", [BBox])
  },
  Goal,
  state(-(bbox, [BBox, Parent | BBoxes], [Parent | BBoxes])).

add_id_(Id, Ids, FinalIds) :-
  reify(ord_memberchk(Id, Ids), Res),
  add_id_(Res, Id, Ids, FinalIds).
add_id_(false, Id, Ids, NewIds) :-
  maplist(dif(Id), Ids),
  ord_add_element(Ids, Id, NewIds).
add_id_(true, _, Ids, Ids).

add_id(Id) -->
  statep(add_id_(Id), [-(ids)]).

ground_all_ids(state(State)) :-
  rb_lookup(ids, Ids, State),
  include(var, Ids, VarIds),
  maplist(gensym(id), VarIds).

:- begin_tests(state).

test('state(o(key))') :-
  rb_new(TreeIn),
  rb_insert_new(TreeIn, key, value, TreeOut),
  phrase(state(o(key)), [state(TreeOut)], [state(TreeOut)]).
test('state(o(key, value))') :-
  rb_new(TreeIn),
  rb_insert_new(TreeIn, key, value, TreeOut),
  phrase(state(o(key, value)), [state(TreeOut)], [state(TreeOut)]).
test('state(o(key, Value))') :-
  rb_new(TreeIn),
  rb_insert_new(TreeIn, key, value, TreeOut),
  phrase(state(o(key, Value)), [state(TreeOut)], [state(TreeOut)]),
  Value == value.
test('state(o(newkey, Value))', [error(existence_error(key, newkey, T0))]) :-
  rb_new(T0),
  phrase(state(o(newkey, Value)), [state(T0)], [state(T1)]),
  rb_lookup(newkey, Value, T1).
test('state(+(key, value))') :-
  rb_new(TreeIn),
  phrase(state(+(key, value)), [state(TreeIn)], [state(TreeOut)]),
  rb_lookup(key, value, TreeOut).
test('state(+(existentkey, value))') :-
  rb_new(T0),
  rb_insert_new(T0, existentkey, previousvalue, T1),
  phrase(state(+(existentkey, value)), [state(T1)], [state(T2)]),
  rb_lookup(existentkey, value, T2).
test('state(-(key, oldvalue, newvalue))') :-
  rb_new(EmptyTree),
  rb_insert_new(EmptyTree, key, oldvalue, TreeIn),
  phrase(state(-(key, oldvalue, newvalue)), [state(TreeIn)], [state(TreeOut)]),
  rb_lookup(key, newvalue, TreeOut).

test('states') :-
  rb_new(T0),
  rb_insert_new(T0, key1, value1, T1),
  rb_insert_new(T1, key2, value2, T2),
  phrase(state([[o(key1, value1), -(key2, value2, newvalue2), +(key3, value3)]]),
         [state(T2)], [state(_T3)]).
test('statep(Goal, KeyValues)') :-
  rb_new(T0),
  rb_insert_new(T0, key1, value1, T1),
  rb_insert_new(T1, key2, value2, T2),
  phrase(statep([_Value1, _OldValue2, _NewValue2, _Value3]>>(true),
                [o(key1, value1), -(key2, value2, newvalue2), +(key3, value3)]),
         [state(T2)], [state(_T3)]).
test('statep(Goal, KeyValues)') :-
  list_to_rbtree([key1-value1, key2-value2], T2),
  phrase(statep([value1,
                 [value1, value2, newvalue2],
                 value3]>>(true),
                [o(key1, value1),
                 [o(key1, value1), -(key2, value2, newvalue2)],
                 +(key3, value3)]),
         [state(T2)], [state(_T3)]).
test('statep(Goal, [o(key1)])') :-
  rb_new(T0),
  rb_insert_new(T0, key1, value1, T1),
  phrase(statep([_Value1]>>(true), [o(key1)]),
         [state(T1)], [state(_T2)]).
test('statep(Goal, [-(key1)])') :-
  rb_new(T0),
  rb_insert_new(T0, key1, value1, T1),
  phrase(statep([_OldValue, _NewValue]>>(true), [-(key1)]),
         [state(T1)], [state(_T2)]).

:- use_module(library(delay)).

test('statep_delay') :-
  list_to_rbtree([key1-value1], T0),
  phrase(statep(delay:delay(atom_codes), [-key1]), [state(T0)], [state(_)]).

:- end_tests(state).
