:- module(state, [makeState/2, state//1, states//1, statep//2, scope//1, scope//2,
                  bbox//2, nCond/3, nCond//2]).

:- use_module(library(rbtrees)).
:- use_module(library(clpBNR)).
:- use_module(library(dcg/high_order)).
:- use_module(geo).

makeState(state(Tree), List) :-
  list_to_rbtree([cursor-noEl, scope-[], bbox-[] | List], Tree).

state(Term) -->
  stateValues(Term, _).
stateValues(Term, Values), [state(StateOut)] -->
  [state(StateIn)],
  { state_(Term, Values, StateIn, StateOut) }.

state_(o(Key), Values, StateIn, StateOut) :-
  state_(o(Key, _), Values, StateIn, StateOut).
state_(o(Key, Value), [Value], StateIn, StateOut) :-
  ( rb_lookup(Key, Value, StateIn)
  ->  StateIn = StateOut
  ; rb_insert_new(StateIn, Key, Value, StateOut)
  ).
state_(+(Key), Values, StateIn, StateOut) :-
  state_(+(Key, _), Values, StateIn, StateOut).
state_(+(Key, Value), [Value], StateIn, StateOut) :-
  rb_insert(StateIn, Key, Value, StateOut).

state_(-(Key), Values, StateIn, StateOut) :-
  state_(-(Key, _, _), Values, StateIn, StateOut).
state_(-(Key, OldValue, NewValue), [OldValue, NewValue], StateIn, StateOut) :-
  rb_update(StateIn, Key, OldValue, NewValue, StateOut).

sequence3(Goal, L1, L2) -->
  sequence3_(L1, L2, Goal).
sequence3_([A | L1], [B | L2], Goal) -->
  call(Goal, A, B),
  sequence3_(L1, L2, Goal).
sequence3_([], [], _Goal) -->
  [].

states(KeyValues) -->
  sequence(state, KeyValues).

nCond(NAtom, PrevN, N) :-
  N::integer(1, _),
  { N == PrevN + 1 },
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
  sequence3(stateValues, KeyValues, ListValues),
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

:- meta_predicate scope(4, ?, ?).

scope(Mod:Goal, Arg) -->
  {
    Goal =.. L,
    append(L, [Arg], NewL),
    NewGoal =.. NewL
  },
  scope(Mod:NewGoal).

:- meta_predicate bbox(2, ?, ?, ?).

bbox(Mod:Goal, BBox) -->
  state(-(bbox, [Parent | BBoxes], [BBox, Parent | BBoxes])),
  {
    box(BBox),
    inside(BBox, Parent)
  },
  call(Mod:Goal),
  state(-(bbox, [BBox, Parent | BBoxes], [Parent | BBoxes])).

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
test('state(o(newkey, Value))') :-
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
  phrase(states([o(key1, value1), -(key2, value2, newvalue2), +(key3, value3)]),
         [state(T2)], [state(_T3)]).
test('statep(Goal, KeyValues)') :-
  rb_new(T0),
  rb_insert_new(T0, key1, value1, T1),
  rb_insert_new(T1, key2, value2, T2),
  phrase(statep([_Value1, _OldValue2, _NewValue2, _Value3]>>(true),
                [o(key1, value1), -(key2, value2, newvalue2), +(key3, value3)]),
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
