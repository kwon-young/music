:- module(epf, [term//1, select//1, termchk//1, selectchk//1]).

term(El) -->
  [El].
term(El), [CurEl] -->
  [CurEl],
  term(El).
select(El), [El] -->
  term(El).

termchk(El) -->
  [El], !.
termchk(El), [CurEl] -->
  [CurEl],
  termchk(El).
selectchk(El), [El] -->
  termchk(El).
