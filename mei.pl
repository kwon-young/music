:- module(mei, [mei_transform/2]).

:- use_module(library(delay)).
:- use_module(library(apply)).

:- multifile delay:mode/1.

delay:mode(mei:vu(ground, _)).
delay:mode(mei:vu(_, ground)).
vu(Atom, Number) :-
  delay(atom_number(AtomNumber, Number)),
  delay(atom_concat(AtomNumber, 'vu', Atom)).

transform(meiversion=Atom, meiversion=N) :-
  delay(atom_number(Atom, N)).
transform(n=Atom, n=N) :-
  delay(atom_number(Atom, N)).
transform(lines=Atom, lines=N) :-
  delay(atom_number(Atom, N)).
transform(line=Atom, line=N) :-
  delay(atom_number(Atom, N)).
transform(count=Atom, count=N) :-
  delay(atom_number(Atom, N)).
transform(unit=Atom, unit=N) :-
  delay(atom_number(Atom, N)).
transform(ho=Atom, ho=N) :-
  delay(atom_number(Atom, N)).
transform(staff=StaffAtom, staff=StaffList) :-
  delay((
      atomic_list_concat(Atoms, ' ', StaffAtom),
      maplist(atom_number, Atoms, StaffList)
  )).
transform(dur=Atom, dur=N) :-
  delay(atom_number(Atom, N)).
transform(oct=Atom, oct=N) :-
  delay(atom_number(Atom, N)).
transform(len=Atom, len=N) :-
  delay(vu(Atom, N)).
