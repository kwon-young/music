:- module(epf, [term//1, select//1, update//2,
                termchk//1, selectchk//1, selectchk//2, updatechk//2,
                state//2, state//3, state//4, state//5, state//6, state//7, state//8,
                state//9,
                state_selectchk//1, state_selectchk//2, state_selectchk//3,
                add//1,
                sequence2//3, sequence2//5]).

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

state_(Compound, Mod:Goal, Mod:NewGoal) -->
  updatechk(state(StateIn), state(StateOut)),
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
:- meta_predicate state(?, ?, ?, ?, ?, ?, ?, ?, 8, ?, ?).

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
state(Name1, Name2, Name3, Name4, Name5, Name6, Name7, Name8, Goal) -->
  state_(Name1, Goal, NewGoal),
  state(Name2, Name3, Name4, Name5, Name6, Name7, Name8, NewGoal).

identity(_).

state_selectchk(Term) , [state(StateOut)]-->
  [state(StateIn)],
  {phrase(selectchk(Term), StateIn, StateOut)}.
state_selectchk(Term1, Term2) -->
  state_selectchk(Term1),
  state_selectchk(Term2).
state_selectchk(Term1, Term2, Term3) -->
  state_selectchk(Term1),
  state_selectchk(Term2),
  state_selectchk(Term3).

:- meta_predicate sequence2(6, ?, ?, ?, ?).

sequence2(Element, [Start1 | List1], [Start2 | List2]) -->
  sequence2_(List1, List2, Start1, Start2, Element).
sequence2_([B1 | List1], [B2 | List2], A1, A2, P) -->
  call(P, A1, B1, A2, B2),
  !,
  sequence2_(List1, List2, B1, B2, P).
sequence2_([], [], _, _, _) --> {true}.

:- meta_predicate sequence2(9, ?, ?, ?, ?, ?, ?).

sequence2(Element, [S1 | L1], [S2 | L2], [S3 | L3], [S4 | L4]) -->
  sequence2_(L1, L2, L3, L4, S1, S2, S3, S4, Element).
sequence2_([B1 | L1], [B2 | L2], [B3 | L3], [B4 | L4], A1, A2, A3, A4, P) -->
  call(P, A1, B1, A2, B2, A3, B3, A4, B4),
  !,
  sequence2_(L1, L2, L3, L4, B1, B2, B3, B4, P).
sequence2_([], [], [], [], _, _, _, _, _) --> {true}.
