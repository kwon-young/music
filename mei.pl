:- module(mei, []).

:- use_module(library(delay)).
:- use_module(library(apply)).
:- use_module(library(lists)).

load_mei(Filename, Mei) :-
  ( file_name_extension(_, mei, Filename)
  -> load_xml(Filename, Xml, [space(remove), number(integer)]),
    mapsubterms(parse, Xml, Mei)
  ; file_name_extension(_, pl, Filename),
    open(Filename, read, S),
    read(S, Mei),
    close(S)
  ).

mei_write(Filename, Mei) :-
  open(XmlFile, write, XmlS),
  ( ground(Xml)
  -> xml_write(XmlS, Xml, [])
  ; print_term(Xml, [output(XmlS)])
  ),

:- multifile delay:mode/1.

delay:mode(mei:vu(ground, _)).
delay:mode(mei:vu(_, ground)).
vu(Atom, Number) :-
  delay(atom_number(AtomNumber, Number)),
  delay(atom_concat(AtomNumber, 'vu', Atom)).

gen(A, B) :- parse(B, A).

parse(meiversion=Atom, meiversion=N) :-
  delay(atom_number(Atom, N)).
parse(lines=Atom, lines=N) :-
  delay(atom_number(Atom, N)).
parse(line=Atom, line=N) :-
  delay(atom_number(Atom, N)).
parse(count=Atom, count=N) :-
  delay(atom_number(Atom, N)).
parse(ho=Atom, ho=N) :-
  delay(atom_number(Atom, N)).
parse(staff=StaffAtom, staff=StaffList) :-
  delay((
      atomic_list_concat(Atoms, ' ', StaffAtom),
      maplist(atom_number, Atoms, StaffList)
  )).
parse(dur=Atom, dur=N) :-
  delay(atom_number(Atom, N)).
parse(oct=Atom, oct=N) :-
  delay(atom_number(Atom, N)).
parse(len=Atom, len=N) :-
  delay(vu(Atom, N)).
parse(element(measure, AttrIn, ChildsIn), element(measure, AttrOut, ChildsOut)) :-
  mapsubterms(parse, ChildsIn, ChildsOut),
  selectchk(n=Atom, AttrIn, n=N, AttrOut),
  delay(atom_number(Atom, N)).
parse(element(staff, AttrIn, ChildsIn), element(staff, AttrOut, ChildsOut)) :-
  mapsubterms(parse, ChildsIn, ChildsOut),
  selectchk(n=Atom, AttrIn, n=N, AttrOut),
  delay(atom_number(Atom, N)).
parse(element(staffDef, AttrIn, ChildsIn), element(staffDef, AttrOut, ChildsOut)) :-
  mapsubterms(parse, ChildsIn, ChildsOut),
  selectchk(n=Atom, AttrIn, n=N, AttrOut),
  delay(atom_number(Atom, N)).
parse(element(meterSig, AttrIn, ChildsIn), element(meterSig, AttrOut, ChildsOut)) :-
  mapsubterms(parse, ChildsIn, ChildsOut),
  selectchk(unit=Atom, AttrIn, unit=N, AttrOut),
  delay(atom_number(Atom, N)).
