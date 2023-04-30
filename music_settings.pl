:- module(music_settings, [get_settings/2, update_settings/1]).

:- use_module(library(clpBNR)).
:- use_module(library(settings)).

:- setting(pageWidth, pair, real(0, inf)-2100, 'page width').
:- setting(pageHeight, pair, real(0, inf)-2970, 'page height').
:- setting(topMargin, pair, real(0, inf)-104, 'page top margin').
:- setting(leftMargin, pair, real(0, inf)-50, 'page left margin').
:- setting(bottomMargin, pair, real(0, inf)-50, 'page bottom margin').
:- setting(rightMargin, pair, real(0, inf)-50, 'page right margin').
:- setting(eps, pair, real(0, 100)-0, 'Global Eps in pixels').
:- setting(unit, pair, real(0, 1000)-9, 'The MEI unit (1⁄2 of the distance between the staff lines)').
:- setting(thickness, pair, real(0, 1)-0.15, 'line thickness in unit').
:- setting(barLineThickness, pair, real(0, 1)-0.30, 'barline thickness in unit').
:- setting(measureMinWidth, pair, real(0, 100)-15, 'The minimal measure width in MEI units').
:- setting(leftMarginClef, pair, real(0, 2)-1, 'The left margin for clef in MEI units').
:- setting(spacingStaff, pair, real(0, 48)-12, 'The staff minimal spacing in MEI units').
:- setting(gClefWidth, pair, real(3, 9)-5.16, 'The G clef width in MEI units').
:- setting(gClefHeight, pair, real(10, 20)-13.9, 'The G clef height in MEI units').
:- setting(gClefYOffset, pair, real(4, 16)-8.7, 'The G clef origin vertical offset from the top in MEI units').
% :- setting(noteStem, number, 0.16, 'stem note vertical jonction').

label_setting(eps, Value) :-
  global_minimize(Value, Value, 3).
label_setting(_, _).

get_settings(Type, Settings) :-
  findall(Setting, get_setting(Type, Setting), Settings).
get_setting(domain, Name-Value) :-
  setting(_Mod:Name, Range-_),
  Value::Range.
get_setting(value, Name-Value) :-
  setting(_Mod:Name, _-Value).

update_settings(Settings) :-
  maplist(update_setting, Settings).
update_setting(Name-Value) :-
  setting(Mod:Name, Domain-Default),
  once(label_setting(Name, Value)),
  ( interval(Value)
  ->  domain(Value, NewDomain), set_setting(Mod:Name, NewDomain-Default)
  ; set_setting(Mod:Name, Domain-Value)
  ).
