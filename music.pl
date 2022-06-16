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

:- use_module(library(settings)).
:- setting(staff_aligned_eps, number, 0, 'staffline alignement threshold').
:- setting(staff_interline_eps, number, 0.05, 'stafflines interline threshold').
:- setting(staff_barline_eps, number, 0.05, 'stafflines and barline threshold').
:- setting(clef_eps, number, 0, 'Clef position threshold').
:- setting(beats_eps, number, 0, 'Clef position threshold').
:- setting(stemEW_eps, number, 0.05, 'stem up/down horizontal note jonction threshold').
:- setting(stemUp, list(pair), [
  'noteheadHalf'-0.16,
  'noteheadBlack'-0.16
  ], 'stem up vertical note jonction').
:- setting(stemDown, list(pair), [
  'noteheadHalf'-(-0.14),
  'noteheadBlack'-(-0.16)
  ], 'stem down vertical note jonction').
:- setting(stemNS_eps, number, 0.01, 'stem up/down vertical note jonction threshold').

ccxOnStaffline(Stafflines, Ccx, Etiq, NumStaffline, Eps) :-
  ccxEtiqsCond(Ccx, Etiq),
  nth1(NumStaffline, Stafflines, Staffline),
  ccxOnSegCond(Staffline, Ccx, Eps).

mainGen(XmlFile, StructFile) :-
  load_xml(XmlFile, Xml, [space(remove), number(integer)]),
  phrase(music(Xml), [state(State) | Struct], Rest),
  open(StructFile, write, S),
  print_term(Struct, [output(S)]),
  close(S),
  format("State~n", []),
  print_term(State, _),
  format("Struct~n", []),
  print_term(Struct, _),
  format("~nRest~n", []),
  print_term(Rest, _).

mainReco(StructFile, XmlFile) :-
  mainReco(StructFile, Struct, Rest, Xml),
  open(XmlFile, write, XmlS),
  xml_write(XmlS, Xml, [
    doctype('score-partwise'),
    public("-//Recordare//DTD MusicXML 4.0 Partwise//EN"),
    system("http://www.musicxml.org/dtds/partwise.dtd"),
    net(false)]),
  close(XmlS),
  format("Xml~n", []),
  print_term(Xml, _),
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
  partList(PartId, PartList),
  part(PartId, Part).

partList(PartId, element('part-list', [], [ScorePart])) -->
  scorePart(PartId, ScorePart).

scorePart(PartId, element('score-part', [id=PartId], [element('part-name', [], [])])) -->
  {true}.

part(PartId, element(part, [id=PartId], [Measure])) -->
  { PartId = 'P1' },
  measure(Measure),
  state(division, lower_bound).

measure(element(measure, [number='1'], [Attributes, Note])) -->
  {debug(music, "measure: ~p, ~p~n", [Attributes, Note])},
  staff(),
  barline(),
  attributes(Attributes),
  note(Note).

aligned(Getter, Eps, Elements) :-
  convlist(Getter, Elements, [Coord | Coords]),
  maplist(diffEps(Eps, Coord), Coords).

interlineCond(Stafflines, Getter, Eps) :-
  music_utils:interlineAt(Stafflines, Getter, Interline, Interlines),
  maplist(diffEps(Eps, Interline), Interlines).

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
  setting(staff_interline_eps, InterlineEps),
  % all interlines are similar
  interlineCond(Stafflines, segStartY, InterlineEps),
  interlineCond(Stafflines, segEndY, InterlineEps).

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
  term(Clef).

time(element(time, [], [Beats, BeatType])) -->
  beats(Beats),
  beatType(BeatType).

beatsCond('4', Beats, Stafflines) :-
  setting(beats_eps, BeatsEps),
  ccxOnStaffline(Stafflines, Beats, 'timeSig4', 4, BeatsEps).

beats(element(beats, [], [BeatsNumber])) -->
  state(beats(Beats), stafflines, beatsCond(BeatsNumber)),
  term(Beats).

beatTypeCond('4', BeatType, Stafflines) :-
  setting(beats_eps, BeatsEps),
  ccxOnStaffline(Stafflines, BeatType, 'timeSig4', 2, BeatsEps).

beatType(element('beat-type', [], [BeatTypeNumber])) -->
  state(beatType(BeatType), stafflines, beatTypeCond(BeatTypeNumber)),
  term(BeatType).

key(element(key, [], [element(fifths, [], ['0'])])) --> {true}.
