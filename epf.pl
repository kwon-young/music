:- module(epf, [term//1, select//1, update//2,
                termchk//1, selectchk//1, selectchk//2, updatechk//2,
                state//2, state//3, state//4, state//5, state//6, state//7, state//8,
                state_selectchk//1, state_selectchk//2,
                add//1]).

term(El) -->
  [El].
term(El), [CurEl] -->
  [CurEl],
  term(El).
termchk(El) -->
  [El], !.
termchk(El), [CurEl] -->
  [CurEl],
  termchk(El).

select_(Goal, El), [El] -->
  call(Goal, El).
select(El) -->
  select_(term, El).
selectchk(El) -->
  select_(termchk, El).
selectchk(El1, El2) -->
  selectchk(El1),
  selectchk(El2).

update_(Goal, In, Out), [Out] -->
  call(Goal, In).
update(In, Out) -->
  update_(term, In, Out).
updatechk(In, Out) -->
  update_(termchk, In, Out).

add(El), [El] -->
  { true }.

parseOp(-, updatechk, 2).
parseOp(+, add, 1).
parseCompound(Name, StateGoal, Args) :-
  atom(Name),
  !,
  parseCompound_(Name, 1, selectchk, 1, StateGoal, Args).
parseCompound(Compound, StateGoal, Args) :-
  compound_name_arguments(Compound, '/', [Name, Arity]),
  atom(Name),
  !,
  parseCompound_(Name, Arity, selectchk, 1, StateGoal, Args).
parseCompound(Compound, StateGoal, Args) :-
  compound_name_arguments(Compound, Op, [Name]),
  parseOp(Op, StateFunc, StateArity),
  atom(Name),
  !,
  parseCompound_(Name, 1, StateFunc, StateArity, StateGoal, Args).
parseCompound(Compound, StateGoal, Args) :-
  compound_name_arguments(Compound, '/', [SubCompound, Arity]),
  compound_name_arguments(SubCompound, Op, [Name]),
  parseOp(Op, StateFunc, StateArity),
  atom(Name),
  !,
  parseCompound_(Name, Arity, StateFunc, StateArity, StateGoal, Args).
parseCompound(Compound, StateGoal, AllArgs) :-
  compound_name_arguments(Compound, Op, [SubCompound]),
  parseOp(Op, StateFunc, StateArity),
  compound_name_arguments(SubCompound, Name, Args),
  atom(Name),
  !,
  length(Args, Arity),
  parseCompound_(Name, Arity, StateFunc, StateArity, StateGoal, AllArgs),
  append(_, Args, AllArgs).
parseCompound(Compound, StateGoal, Args) :-
  compound_name_arguments(Compound, Name, Args),
  atom(Name),
  !,
  length(Args, Arity),
  parseCompound_(Name, Arity, selectchk, 1, StateGoal, Args).
parseCompound_(Name, Arity, StateFunctor, StateArity, StateGoal, Args) :-
  length(Compounds, StateArity),
  maplist({Name, Arity}/[Compound, Arg]>>(
    length(Arg, Arity),
    compound_name_arguments(Compound, Name, Arg)), Compounds, ArgsList),
  append(ArgsList, Args),
  compound_name_arguments(StateGoal, StateFunctor, Compounds).

:- meta_predicate state_(?, 0, ?, ?, ?).

state_(Compound, Mod:Goal, Mod:NewGoal), [state(StateOut)] -->
  [state(StateIn)],
  {
    parseCompound(Compound, StateGoal, Args),
    Goal =.. GoalList,
    append(GoalList, Args, NewGoalList),
    NewGoal =.. NewGoalList,
    phrase(StateGoal, StateIn, StateOut)
  }.

:- meta_predicate state(0, ?, ?).
:- meta_predicate state(?, 1, ?, ?).
:- meta_predicate state(?, ?, 2, ?, ?).
:- meta_predicate state(?, ?, ?, 3, ?, ?).
:- meta_predicate state(?, ?, ?, ?, 4, ?, ?).
:- meta_predicate state(?, ?, ?, ?, ?, 5, ?, ?).
:- meta_predicate state(?, ?, ?, ?, ?, ?, 6, ?, ?).
:- meta_predicate state(?, ?, ?, ?, ?, ?, ?, 7, ?, ?).

state(Goal) -->
  { call(Goal) }.
state(Name, Goal) -->
  state_(Name, Goal, NewGoal),
  state(NewGoal).
state(Name1, Name2, Goal) -->
  state_(Name1, Goal, NewGoal),
  state(Name2, NewGoal).
state(Name1, Name2, Name3, Goal) -->
  state_(Name1, Goal, NewGoal),
  state(Name2, Name3, NewGoal).
state(Name1, Name2, Name3, Name4, Goal) -->
  state_(Name1, Goal, NewGoal),
  state(Name2, Name3, Name4, NewGoal).
state(Name1, Name2, Name3, Name4, Name5, Goal) -->
  state_(Name1, Goal, NewGoal),
  state(Name2, Name3, Name4, Name5, NewGoal).
state(Name1, Name2, Name3, Name4, Name5, Name6, Goal) -->
  state_(Name1, Goal, NewGoal),
  state(Name2, Name3, Name4, Name5, Name6, NewGoal).
state(Name1, Name2, Name3, Name4, Name5, Name6, Name7, Goal) -->
  state_(Name1, Goal, NewGoal),
  state(Name2, Name3, Name4, Name5, Name6, Name7, NewGoal).

identity(_).

state_selectchk(Term) , [state(StateOut)]-->
  [state(StateIn)],
  {phrase(selectchk(Term), StateIn, StateOut)}.
state_selectchk(Term1, Term2) -->
  state_selectchk(Term1),
  state_selectchk(Term2).
