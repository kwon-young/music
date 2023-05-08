:- module(music, [main/0, main/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpBNR)).
:- use_module(library(pprint)).
:- use_module(library(delay)).
:- use_module(seg).
:- use_module(ccx).
:- use_module(geo).
:- use_module(utils).
:- use_module(epf).
:- use_module(state).
:- use_module(cond).
:- use_module(epf_geo).
:- use_module(music_utils).
:- use_module(music_settings).
:- use_module(pitch_cond).

main :-
  current_prolog_flag(argv, [Goal | Args]),
  main(Goal, Args).
main(Goal, Args) :-
  apply(Goal, Args).

mainGen(XmlFile, StructFile) :-
  load_xml(XmlFile, Xml, [space(remove), number(integer)]),
  get_settings(value, Settings),
  makeState(State, Settings),
  once(phrase(mei(Xml), [State, Struct], [_, []])),
  maplist(ground_elem, Struct, GroundStruct),
  open(StructFile, write, S),
  print_term(GroundStruct, [output(S)]),
  write(S, "."),
  close(S).
mainReco(StructFile, TestSettingsFile, XmlFile, RecoSettingsFile) :-
  open(StructFile, read, S),
  read(S, Struct),
  close(S),
  load_settings(TestSettingsFile),
  get_settings(domain, Settings),
  makeState(State, Settings),
  once(phrase(mei(Xml), [State, Struct], [_, []])),
  % print_term(Rest, []),
  % Rest == [],
  open(XmlFile, write, XmlS),
  xml_write(XmlS, Xml, []),
  close(XmlS),
  update_settings(Settings),
  save_settings(RecoSettingsFile).
mainTest(XmlFile, StructFile, SettingsFile) :-
  load_xml(XmlFile, Xml, [space(remove), number(integer)]),
  open(StructFile, read, S),
  read(S, Struct),
  close(S),
  forall(setting(Mod:Name, _), restore_setting(Mod:Name)),
  get_settings(domain, Settings),
  makeState(State, Settings),
  once(phrase(mei(Xml), [State, Struct], [_, _Rest])),
  % print_term(Rest, []),
  % Rest == [],
  update_settings(Settings),
  save_settings(SettingsFile).

mei([pi('xml-model href="https://music-encoding.org/schema/dev/mei-all.rng" type="application/xml" schematypens="http://relaxng.org/ns/structure/1.0"'),
     pi('xml-model href="https://music-encoding.org/schema/dev/mei-all.rng" type="application/xml" schematypens="http://purl.oclc.org/dsdl/schematron"'),
     element(mei, [xmlns='http://www.music-encoding.org/ns/mei', meiversion='5.0.0-dev'], [MeiHead, Music])]) -->
  states([
    +(pageId, 0),
    +(measureN, 0),
    +(staffN, 0),
    +(staffLines, noEl)
  ]),
  {
    MeiHead = element(meiHead, [], [element(fileDesc, [], [element(titleStmt, [], [element(title, [], [])])])])
  },
  music(Music).

music(element(music, [], [Body])) -->
  body(Body).

body(element(body, [], [element(mdiv, ['xml:id'=Id], [Score])])) -->
  add_id(Id),
  score(Score).

score(element(score, ['xml:id'=Id], [ScoreDef, Section])) -->
  add_id(Id),
  scoreDef(ScoreDef),
  section(Section).

scoreDef(element(scoreDef, ['xml:id'=Id], [StaffGrp])) -->
  add_id(Id),
  staffGrp(StaffGrp).

% staffGrp(element(staffGrp, ['xml:id'=Id, 'bar.thru'='true'], [StaffDef1, StaffDef2])) -->
%   add_id(Id),
%   staffDef(StaffDef1),
%   staffDef(StaffDef2).
staffGrp(element(staffGrp, ['xml:id'=Id], [StaffDef])) -->
  add_id(Id),
  staffDef(StaffDef).

staffDef(element(staffDef, ['xml:id'=Id, n=NAtom, lines=Lines], Childs)) -->
  add_id(Id),
  nCond(staffN, NAtom),
  statep(atom_number(Lines), [+(NAtom-staffNumLines, 5)]),
  state(+(NAtom-staffDef, Childs)).

section(element(section, ['xml:id'=Id], Measures)) -->
  add_id(Id),
  scope(page(Measures)).

pageCond(Page, PrevId, Id, [PageMargin], W, H, TopM, LeftM, BotM, RightM) :-
  Id::integer(1, _),
  { Id == PrevId + 1 },
  ccxEtiqsCond(Page, 'page'),
  ccxLeftTop(Page, point(0, 0)),
  ccxOrigin(Page, point(0, 0)),
  ccxWidth(Page, W),
  ccxHeight(Page, H),
  ccxLeftTopRightBottom(Page, point(Left, Top), point(Right, Bottom)),
  [TopWM, LeftWM, BottomWM, RightWM]::real(0, inf),
  {
    TopWM == Top + TopM,
    LeftWM == Left + LeftM,
    BottomWM == Bottom - BotM,
    RightWM == Right - RightM
  },
  boxArgs(PageMargin, [point(LeftWM, TopWM), point(RightWM, BottomWM)]),
  box(PageMargin).
page(Measures, PageId) -->
  statep(pageCond(Page), [-(pageId, _, PageId), +(bbox), o(pageWidth), o(pageHeight),
                          o(topMargin), o(leftMargin), o(bottomMargin), o(rightMargin)]),
  terms(Page),
  measures(Measures).

measures(Measures) -->
  state(o(spacingStaff, Spacing)),
  vertical_layout(measureLine, Spacing, Measures).

measureLine(MeasuresIn, MeasuresOut) -->
  state(+(staffLines, noEl)),
  longuest_notempty_sequence(state:scope(music:measure), MeasuresIn, MeasuresOut).

measureCond(StaffBox, StaffWidth, MeasureMinWidth, Unit, Eps) :-
  { StaffWidth >= MeasureMinWidth * Unit },
  boxWidth(StaffBox, StaffBoxWidth),
  eps(Eps, StaffWidth, StaffBoxWidth).

measure(element(measure, ['xml:id'=Id, n=NAtom], [Staff]), Id) -->
  add_id(Id),
  statep(nCond(NAtom), [-(measureN)]),
  states([+(staffN, 0), +(staffWidth)]),
  bbox(scope(staff(Staff)), Box),
  statep(measureCond(Box), [o(staffWidth), o(measureMinWidth), o(unit), o(eps)]).

staff(element(staff, ['xml:id'=Id, n=NAtom], [Layer]), Id) -->
  debug(staff, "start ~p~n", [NAtom]),
  add_id(Id),
  statep(nCond(NAtom), [-(staffN)]),
  stafflines(NAtom),
  clef(NAtom),
  scope(layer(Layer)),
  pop_scope(barline).

stafflinesCond([L | Lines], StaffLines, _, NumLines, Unit, Width, Thickness, Eps) :-
  maplist(segEnd, [L | Lines], Ends),
  length(StaffLines, NumLines),
  maplist(segStart, StaffLines, Starts),
  maplist(eps(p, Eps), Ends, Starts),
  Ends = [End | _],
  Starts = [Start | _],
  debug(stafflinesCond, "End ~p~n", [End]),
  debug(stafflinesCond, "Start ~p~n", [Start]),
  stafflinesCond(StaffLines, NumLines, Unit, Width, Thickness, Eps).
stafflinesCond(noEl, StaffLines, [Box | _], NumLines, Unit, Width, Thickness, Eps) :-
  stafflinesCond(StaffLines, NumLines, Unit, Width, Thickness, Eps),
  StaffLines = [TopLine | _],
  segStart(TopLine, Start),
  boxArgs(Box, [LeftTop, _]),
  debug(stafflinesCond, "LeftTop ~p~n", [LeftTop]),
  debug(stafflinesCond, "Start ~p~n", [Start]),
  eps(px, Eps, LeftTop, Start).
stafflinesCond(StaffLines, NumLines, Unit, Width, Thickness, Eps) :-
  length(StaffLines, NumLines),
  maplist(segStartEndThickness, StaffLines, Starts, Ends, Thicknesses),
  maplist(leftof, Starts, Ends),
  chaing(Starts, above(2*Unit, Eps)),
  chaing(Ends, above(2*Unit, Eps)),
  chaing(Starts, eps(px, Eps)),
  chaing(Ends, eps(px, Eps)),
  maplist(eps(Eps, Thickness*Unit), Thicknesses),
  maplist(horizontalSeg(Eps, Unit), StaffLines),
  maplist(segWidth, StaffLines, Widths),
  maplist(eps(Eps, Width), Widths).

stafflines(N) -->
  debug(stafflines, "start~n", []),
  statep(stafflinesCond, [-(staffLines, _, StaffLines), o(bbox), o(N-staffNumLines),
                          o(unit), o(staffWidth), o(thickness), o(eps)]),
  sequence(termp, StaffLines),
  debug(stafflines, "end ~p~n", [StaffLines]).

layer(element(layer, ['xml:id'=Id, n='1'], []), Id) -->
  add_id(Id),
  [].

barlineCond(BarLine, StaffLines, Thickness, Unit, Eps) :-
  nth1(1, StaffLines, TopLine),
  last(StaffLines, BottomLine),
  maplist(segEnd, [TopLine, BottomLine], StaffLinesPoints),
  segHV(v, right, top, BarLine, BarLineTopRight),
  segHV(v, right, bottom, BarLine, BarLineBottomRight),
  maplist(eps(p, Eps), [BarLineTopRight, BarLineBottomRight], StaffLinesPoints),
  segThickness(BarLine, BarLineThickness),
  eps(Eps, Unit*Thickness, BarLineThickness).
barline -->
  debug(barline, "barline start ~n", []),
  statep(barlineCond(BarLine), [o(staffLines), o(barLineThickness), o(unit), o(eps)]),
  debug(barline, "barline mid ~p~n", [BarLine]),
  termp(BarLine),
  debug(barline, "barline end ~p~n", [BarLine]).

debug(Topic, Fmt, Args) -->
  state(o(scope, Scope)),
  {
    string_concat("~p ~p ", Fmt, NewFmt),
    append([Topic, Scope], Args, NewArgs),
    debug(Topic, NewFmt, NewArgs)
  }.

:- multifile delay:mode/1.

delay:mode(music:clef_shape_etiq(ground, _)).
delay:mode(music:clef_shape_etiq(_, ground)).
clef_shape_etiq('G', gClef).
clef_shape_etiq('F', fClef).

clefCond(Clef,
         [element(clef, ['xml:id'=Id, shape=Shape, line=NAtom], [])],
         StaffLines, LeftMargin, Width, Height, YOffset, Unit, Eps) :-
  add_id(Id),
  delay(atom_number(NAtom, N)),
  ccxEtiqsCond(Clef, 1, 'clef'),
  ccxEtiqsCond(Clef, Etiq),
  delay(clef_shape_etiq(Shape, Etiq)),
  ccxOrigin(Clef, point(X, Y)),
  ccxLeft(Clef, Left),
  eps(Eps, X, Left),
  length(StaffLines, NumLines),
  { Index == NumLines - N + 1 },
  nth1(Index, StaffLines, Line),
  segStart(Line, point(SegX, SegY)),
  eps(Eps, SegY, Y),
  eps(Eps, SegX + Unit * LeftMargin, X),
  ccxWidth(Clef, ClefWidth),
  eps(Eps, ClefWidth, Width*Unit),
  ccxHeight(Clef, ClefHeight),
  eps(Eps, ClefHeight, Height*Unit),
  ccxTop(Clef, Top),
  eps(Eps, Top + YOffset*Unit, Y).

clef(NAtom) -->
  statep(clefCond(Clef), [o(NAtom-staffDef), o(staffLines), o(leftMarginClef),
                          o(gClefWidth), o(gClefHeight), o(gClefYOffset),
                          o(unit), o(eps)]),
  termp(Clef).
clef(NAtom) -->
  state(o(NAtom-staffDef, [])).
