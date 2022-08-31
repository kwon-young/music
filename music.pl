:- module(music, [mainGen/3, mainGen/4, mainReco/3, mainReco/4]).

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
:- use_module(cond).
:- use_module(epf_geo).
:- use_module(note).
:- use_module(music_utils).
:- use_module(pitch_cond).

:- use_module(library(settings)).

:- setting(page_width, number, 2100.0, 'page width').
:- setting(page_height, number, 2970.0, 'page height').
:- setting(staff_aligned_eps, number, 0, 'staffline alignement threshold').
:- setting(staff_interline, number, 18, 'stafflines interline height').
:- setting(staff_interline_eps, number, 0.05, 'stafflines interline threshold').
:- setting(staff_barline_eps, number, 0.05, 'stafflines and barline threshold').
:- setting(clef_eps, number, 0, 'Clef position threshold').
:- setting(beats_eps, number, 0, 'Clef position threshold').

ccxOnStaffline(Stafflines, Ccx, Etiq, NumStaffline, Eps) :-
  ccxEtiqsCond(Ccx, Etiq),
  nth1(NumStaffline, Stafflines, Staffline),
  ccxOnSegCond(Staffline, Ccx, Eps).

mainGen(XmlFile, SettingsFile, StructFile) :-
  load_settings(SettingsFile),
  mainGen(XmlFile, State, Struct, Rest),
  save_settings,
  open(StructFile, write, S),
  print_term(Struct, [output(S)]),
  close(S),
  format("State~n", []),
  print_term(State, _),
  format("Struct~n", []),
  print_term(Struct, _),
  format("~nRest~n", []),
  print_term(Rest, _).
mainGen(XmlFile, State, Struct, Rest) :-
  load_xml(XmlFile, Xml, [space(remove), number(integer)]),
  phrase(music(Xml), [state(State) | Struct], Rest).

mainReco(StructFile, SettingsFile, XmlFile) :-
  load_settings(SettingsFile),
  mainReco(StructFile, Struct, Rest, Xml),
  save_settings,
  format("Xml~n", []),
  print_term(Xml, _),
  open(XmlFile, write, XmlS),
  xml_write(XmlS, Xml, [
    doctype('score-partwise'),
    public("-//Recordare//DTD MusicXML 4.0 Partwise//EN"),
    system("http://www.musicxml.org/dtds/partwise.dtd"),
    net(false)]),
  close(XmlS),
  format("~nStruct~n", []),
  print_term(Struct, _),
  format("~nRest~n", []),
  print_term(Rest, _).
mainReco(StructFile, Struct, Rest, Xml) :-
  open(StructFile, read, S),
  read(S, Struct),
  close(S),
  phrase(music(Xml), [state(_) | Struct], Rest).

music([element('score-partwise', [version='4.0'], [PartList, Part])]) -->
  state2(keySteps([]), keyAlter(0), accidStepsOctaves([])),
  page,
  partList(PartId, PartList),
  part(PartId, Part).

pageCond(Page) :-
  setting(page_width, Width),
  setting(page_height, Height),
  ccxLeftTopRightBottom(Page, point(0, 0), point(Width, Height)),
  ccxEtiqsCond(Page, 'page').

page -->
  state(page(Page), pageCond),
  term(Page).

partList(PartId, element('part-list', [], [ScorePart])) -->
  scorePart(PartId, ScorePart).

scorePart(PartId, element('score-part', [id=PartId], [element('part-name', [], [])])) -->
  {true}.

part(PartId, element(part, [id=PartId], [Measure])) -->
  { PartId = 'P1' },
  measure(Measure),
  {debug(music, "part: Measure ~p~n", [Measure])},
  state2(division(Div), duration(Dur), dots(Dots)),
  {debug(music, "part: Div ~p~n", [Div])},
  {debug(music, "part: Dur ~p~n", [Dur])},
  {debug(music, "part: Dots ~p~n", [Dots])},
  state(division, lower_bound).

measure(element(measure, [number='1'], [Attributes | Notes])) -->
  {debug(music, "measure: In ~p, ~p~n", [Attributes, Notes])},
  staff(),
  barline(),
  attributes(Attributes),
  state2(beam(noBeam)),
  sequence(note, Notes).

aligned(Getter, Eps, Elements) :-
  convlist(Getter, Elements, [Coord | Coords]),
  maplist(diffEps(Eps, Coord), Coords).

interlineCond(Stafflines, Getter, Interline, Eps) :-
  music_utils:interlineAt(Stafflines, Getter, InterlineAverage, Interlines),
  diffEps(Eps, Interline, InterlineAverage),
  maplist(diffEps(Eps, Interline), Interlines),
  debug(music, "interlineCond Interlines: ~p~n", [Interlines]).

staffCond(Stafflines) :-
  length(Stafflines, 5),
  setting(staff_aligned_eps, AlignedEps),
  % left aligned
  aligned(segStartX, AlignedEps, Stafflines),
  % right aligned
  aligned(segEndX, AlignedEps, Stafflines),
  convlist(segStartY, Stafflines, Ys),
  % order lines along Y coord starting from the bottom
  maplist2([Y1, Y2]>>([Y1, Y2]::real, {Y2 < Y1}), Ys),
  setting(staff_interline, Interline),
  setting(staff_interline_eps, InterlineEps),
  % all interlines are similar
  interlineCond(Stafflines, segStartY, Interline, InterlineEps),
  interlineCond(Stafflines, segEndY, Interline, InterlineEps).

staff() -->
  state(stafflines(Stafflines), staffCond),
  sequence(horizontalSeg, Stafflines).

barlineCond(Barline, Stafflines) :-
  setting(staff_barline_eps, BarlineEps),
  % bottom right corner
  Stafflines = [StafflineBottom | _],
  segEnd(StafflineBottom, StafflineBottomEnd),
  segCorner(v, right-bottom, Barline, BarlineBottomRight),
  pointDiffEps(BarlineEps, StafflineBottomEnd, BarlineBottomRight),
  % top right corner
  last(Stafflines, StafflineTop),
  segEnd(StafflineTop, StafflineTopEnd),
  segCorner(v, right-top, Barline, BarlineTopRight),
  pointDiffEps(BarlineEps, StafflineTopEnd, BarlineTopRight).

barline() -->
  state(barline(Barline), stafflines, barlineCond),
  verticalSeg(Barline).

attributes(element(attributes, [], [Div, Key, Time, Clef])) -->
  {debug(music, "attributes: ~p, ~p, ~p, ~p~n", [Div, Key, Time, Clef])},
  division(Div),
  clef(Clef),
  time(Time),
  key(Key).

divisionCond(DivAtom, Div) :-
  Div::integer(1, _),
  delay(atom_number(DivAtom, Div)).

division(element(divisions, [], [DivAtom])) -->
  state(division, divisionCond(DivAtom)).

clefCond([element(sign, [], ['G']), element(line, [], ['2'])],
         Clef, 2, BaseLine, 'G', 4, Stafflines) :-
  setting(clef_eps, ClefEps),
  ccxOnStaffline(Stafflines, Clef, 'gClef', 2, ClefEps),
  nth1(2, Stafflines, BaseLine).


clef(element(clef, [], SignLine)) -->
  state(clef(Clef), num, baseLine, baseStep, baseOctave, stafflines, clefCond(SignLine)),
  termp(Clef).

time(element(time, [], [Beats, BeatType])) -->
  beats(Beats),
  beatType(BeatType).

beatsCond('4', Beats, Stafflines) :-
  setting(beats_eps, BeatsEps),
  ccxOnStaffline(Stafflines, Beats, 'timeSig4', 4, BeatsEps).

beats(element(beats, [], [BeatsNumber])) -->
  state(beats(Beats), stafflines, beatsCond(BeatsNumber)),
  termp(Beats).

beatTypeCond('4', BeatType, Stafflines) :-
  setting(beats_eps, BeatsEps),
  ccxOnStaffline(Stafflines, BeatType, 'timeSig4', 2, BeatsEps).

beatType(element('beat-type', [], [BeatTypeNumber])) -->
  state(beatType(BeatType), stafflines, beatTypeCond(BeatTypeNumber)),
  termp(BeatType).

delay:mode(music:fifthsAlter(ground, _)).
delay:mode(music:fifthsAlter(_, ground)).
fifthsAlter(0, 0).
fifthsAlter(Fifths, Alter) :-
  Fifths::integer(-7, 7),
  Alter::integer(-1, 1),
  {
    Fifths <> 0,
    Alter <> 0,
    Alter == Fifths / abs(Fifths)
  }.
fifthsCond(Accidentals, FifthsAtom, FifthsList, Fifths, Steps, FifthsAlter) :-
  Fifths::integer(-7, 7),
  delay(atom_number(FifthsAtom, Fifths)),
  {Length == abs(Fifths)},
  delay(length(Accidentals, Length)),
  fifthsList(Fifths, FifthsList),
  delay(length(Steps, Length)),
  delay(fifthsAlter(Fifths, FifthsAlter)).

fifthsStep(1, 'F').
fifthsStep(2, 'C').
fifthsStep(3, 'G').
fifthsStep(4, 'D').
fifthsStep(5, 'A').
fifthsStep(6, 'E').
fifthsStep(7, 'B').

fifthsList(Fifths, FifthsList) :-
  Fifths::integer(-7, 7),
  Incr::integer(-1, 1),
  {
    Fifths <> 0,
    Incr <> 0,
    Incr == Fifths / abs(Fifths),
    Fifths1 == Fifths + Incr
  },
  when(
    (nonvar(FifthsList) ; ground(Fifths)),
    fifthsList_(FifthsList, 0, 0, Fifths1, Incr)).
fifthsList(0, [1]).
fifthsList_([], _, Fifths, Fifths, _).
fifthsList_([NextFifth | FifthsList], Cpt, Fifth, Fifths, Incr) :-
  debug(fifthsList, "~p~n", [Fifth]),
  NextFifth::integer(-8, 8),
  {
    Cpt < 8,
    NextCpt == Cpt + 1,
    abs(Fifth) < abs(Fifths),
    NextFifth == Fifth + Incr
  },
  when(
    (nonvar(FifthsList) ; ground(Fifths)),
    fifthsList_(FifthsList, NextCpt, NextFifth, Fifths, Incr)).

  
key(element(key, [], [element(fifths, [], [FifthsAtom])])) -->
  state(fifths, +keySteps(Steps), +keyAlter, fifthsCond(Accidentals, FifthsAtom, FifthsList)),
  state2(clef(Clef), baseStep(BaseStep), baseOctave(BaseOctave)),
  sequence2(fifths, FifthsList, [Clef | Accidentals], [BaseStep | Steps], [BaseOctave | _]).

fifthCondEtiq(Fifth, Accidental) :-
  {Fifth >= 1},
  ccxEtiqsCond(Accidental, 'accidentalSharp').
fifthCondEtiq(Fifth, Accidental) :-
  {Fifth =< -1},
  ccxEtiqsCond(Accidental, 'accidentalFlat').

fifthPattern('G', 2, Fifth, Interval) :-
  fifthGClef(Fifth, Interval).
fifthPattern('F', 4, Fifth, Interval) :-
  fifthFClef(Fifth, Interval).
fifthPattern('C', Num, Fifth, Interval) :-
  fifthCClef(Num, Fifth, Interval).

fifthGClef(1, 6).
fifthGClef(-1, 2).
fifthGClef(Fifth, Interval) :-
  ( fifthsSharp(Fifth, Interval)
  ; fifthsFlat(Fifth, Interval)
  ).

fifthFClef(1, 0).
fifthFClef(-1, -4).
fifthFClef(Fifth, Interval) :-
  ( fifthsSharp(Fifth, Interval)
  ; fifthsFlat(Fifth, Interval)
  ).

fifthCClef(3, Fifth, Interval) :-
  fifthC3(Fifth, Interval).
fifthCClef(4, Fifth, Interval) :-
  fifthC4(Fifth, Interval).

fifthC3(1, 3).
fifthC3(-1, -1).
fifthC3(Fifth, Interval) :-
  ( fifthsSharp(Fifth, Interval)
  ; fifthsFlat(Fifth, Interval)
  ).

fifthC4(1, -4).
fifthC4(-1, -1).
fifthC4(Fifth, Interval) :-
  ( fifthsSharpTenor(Fifth, Interval)
  ; fifthsFlat(Fifth, Interval)
  ).

fifthsSharp(2, -3).
fifthsSharp(3,  4).
fifthsSharp(4, -3).
fifthsSharp(5, -3).
fifthsSharp(6,  4).
fifthsSharp(7, -3).

fifthsSharpTenor(2,  4).
fifthsSharpTenor(3, -3).
fifthsSharpTenor(4,  4).
fifthsSharpTenor(5, -3).
fifthsSharpTenor(6,  4).
fifthsSharpTenor(7, -3).

fifthsFlat(-2,  3).
fifthsFlat(-3, -4).
fifthsFlat(-4,  3).
fifthsFlat(-5,  3).
fifthsFlat(-6, -4).
fifthsFlat(-7, -4).

fifthCond(Fifth, Ref, Accidental, BaseStep, Step, BaseOctave, Octave, Clef, Num, Stafflines) :-
  fifthCondEtiq(Fifth, Accidental),
  fifthPattern(Clef, Num, Fifth, Interval),
  pitchCondCcx(Ref, BaseStep, BaseOctave, Accidental, Step, Octave, Interval, Stafflines).

fifthsIncr(Fifth, NextFifth) :-
  [Fifth, NextFifth]::integer(1, 7),
  { NextFifth == Fifth + 1 }.
fifthsIncr(Fifth, NextFifth) :-
  [Fifth, NextFifth]::integer(-7, -1),
  { NextFifth == Fifth - 1 }.

fifths(Fifth, NextFifth, Ref, Accidental, BaseStep, Step, BaseOctave, Octave) -->
  { fifthsIncr(Fifth, NextFifth) },
  state(baseStep, num, stafflines,
        fifthCond(Fifth, Ref, Accidental, BaseStep, Step, BaseOctave, Octave)),
  termp(Accidental).
