:- module(mei, [mei_transform/2]).

:- use_module(library(delay)).
:- use_module(library(apply)).

:- multifile delay:mode/1.

delay:mode(mei:vu(ground, _)).
delay:mode(mei:vu(_, ground)).
vu(Atom, Number) :-
  delay(atom_number(AtomNumber, Number)),
  delay(atom_concat(AtomNumber, 'vu', Atom)).

mei_transform(meiversion=Atom, meiversion=N) :-
  delay(atom_number(Atom, N)).

mei_transform(n=Atom, n=N) :-
  delay(atom_number(Atom, N)).

mei_transform(lines=Atom, lines=N) :-
  delay(atom_number(Atom, N)).

mei_transform(line=Atom, line=N) :-
  delay(atom_number(Atom, N)).

mei_transform(count=Atom, count=N) :-
  delay(atom_number(Atom, N)).

mei_transform(unit=Atom, unit=N) :-
  delay(atom_number(Atom, N)).

mei_transform(ho=Atom, ho=N) :-
  delay(atom_number(Atom, N)).

mei_transform(staff=StaffAtom, staff=StaffList) :-
  delay((
      atomic_list_concat(Atoms, ' ', StaffAtom),
      maplist(atom_number, Atoms, StaffList)
  )).

mei_transform(dur=Atom, dur=N) :-
  delay(atom_number(Atom, N)).

mei_transform(oct=Atom, oct=N) :-
  delay(atom_number(Atom, N)).

mei_transform(len=Atom, len=N) :-
  delay(vu(Atom, N)).

mei_transform(Term, Term).
