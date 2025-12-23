:- module(inkscape, [debug_highlight/3]).

:- use_module(library(janus)).

:- table import/1.

import(Module) :-
  atom_concat('gi.repository.', Module, FullModule),
  py_import(FullModule, []).

gio(Method, Result) :-
  import('Gio'),
  py_call('Gio':Method, Result).

glib(Method, Result, Options) :-
  import('GLib'),
  py_call('GLib':Method, Result, Options).

:- table bus/1.

bus(Bus) :-
  gio('BusType':'SESSION', Session),
  gio(bus_get_sync(Session, @(none)), Bus).

:- table action_group/3.

action_group(BusName, ObjectPath, ActionGroup) :-
  bus(Bus),
  gio('DBusActionGroup':get(Bus, BusName, ObjectPath), ActionGroup).

app(App) :-
  action_group("org.inkscape.Inkscape", "/org/inkscape/Inkscape", App).

win(Win) :-
  action_group("org.inkscape.Inkscape", "/org/inkscape/Inkscape/window/1", Win).

doc(Doc) :-
  action_group("org.inkscape.Inkscape", "/org/inkscape/Inkscape/document/1", Doc).

new_string(String, NewString) :-
  glib('Variant':new_string(String), NewString, [py_object(true)]).

action(GroupGoal, Action) :-
  action(GroupGoal, Action, @(none)).
action(GroupGoal, Action, Arg) :-
  ( (string(Arg) ; atom(Arg))
  -> new_string(Arg, Variant)
  ;  integer(Arg)
  -> number_string(Arg, Arg1),
     new_string(Arg1, Variant)
  ;  Variant = Arg
  ),
  call(GroupGoal, Group),
  py_call(Group:activate_action(Action, Variant)).

highlight(Id, Color) :-
  action(app, 'select-clear'),
  action(app, 'select-by-id', Id),
  action(win, 'canvas-zoom-selection'),
  action(win, 'canvas-zoom-out'),
  string_concat('fill,', Color, Fill),
  action(app, 'object-set-attribute', Fill),
  string_concat('color,', Color, CurrentColor),
  action(app, 'object-set-attribute', CurrentColor).

debug_highlight(_Topic, X, Color) :-
  compound(X),
  arg(3, X, Ids),
  Ids = [Id-_ | _],
  ground(Id),
  !,
  highlight(Id, Color).
debug_highlight(_, _, _).
