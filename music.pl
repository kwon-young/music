:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpBNR)).
:- use_module(library(pprint)).
:- use_module(seg).
:- use_module(ccx).
:- use_module(geo).
:- use_module(utils).
:- use_module(epf).
:- use_module(cond).
:- use_module(epf_geo).
:- use_module(pitch).

:- use_module(library(settings)).
:- setting(staff_aligned_eps, number, 0, 'staffline alignement threshold').
:- setting(staff_interline_eps, number, 0.05, 'stafflines interline threshold').
:- setting(staff_barline_eps, number, 0.05, 'stafflines and barline threshold').
:- setting(clef_eps, number, 0, 'Clef position threshold').
:- setting(beats_eps, number, 0, 'Clef position threshold').

ccxOnStaffline(Stafflines, Ccx, Etiq, NumStaffline, Eps) :-
  ccxEtiqsCond(Ccx, Etiq),
  nth1(NumStaffline, Stafflines, Staffline),
  ccxOnSegCond(Staffline, Ccx, Eps).

mainGen(XmlFile, StructFile) :-
  load_xml(XmlFile, Xml, [space(remove), number(integer)]),
  phrase(music(Xml), Struct, Rest),
  open(StructFile, write, S),
  print_term(Struct, [output(S)]),
  close(S),
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
  format("Struct~n", []),
  print_term(Struct, _),
  format("~nRest~n", []),
  print_term(Rest, _).
mainReco(StructFile, Struct, Rest, Xml) :-
  open(StructFile, read, S),
  read(S, Struct),
  close(S),
  phrase(music(Xml), Struct, Rest).

music([element('score-partwise', [version='4.0'], [PartList, Part])]) -->
  partList(PartId, PartList),
  part(PartId, Part).

partList(PartId, element('part-list', [], [ScorePart])) -->
  scorePart(PartId, ScorePart).

scorePart(PartId, element('score-part', [id=PartId], [element('part-name', [], [])])) -->
  {true}.

part(PartId, element(part, [id=PartId], [Measure])) -->
  { PartId = 'P1' },
  measure(Measure).

measure(element(measure, [number='1'], [Attributes, Note])) -->
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

staff(), [stafflines(Stafflines)] -->
  { staffCond(Stafflines) },
  sequence(horizontalSeg, Stafflines).

barlineCond(Stafflines, Barline) :-
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

barline(), [barline(Barline)] -->
  selectchk(stafflines(Stafflines)),
  {
    barlineCond(Stafflines, Barline)
  },
  verticalSeg(Barline).

attributes(element(attributes, [], [element(divisions, [], ['1']), Key, Time, Clef])) -->
  clef(Clef),
  time(Time),
  key(Key).

clefCond(Stafflines, Clef, [element(sign, [], ['G']), element(line, [], ['2'])], baseLine(2, BaseLine), basePitch('G'-'4')) :-
  setting(clef_eps, ClefEps),
  ccxOnStaffline(Stafflines, Clef, 'gClef', 2, ClefEps),
  nth1(2, Stafflines, BaseLine).

clef(element(clef, [], SignLine)), [BaseLine, BasePitch, clef(Clef)] -->
  selectchk(stafflines(Stafflines)),
  {
    clefCond(Stafflines, Clef, SignLine, BaseLine, BasePitch)
  },
  term(Clef).

time(element(time, [], [Beats, BeatType])) -->
  beats(Beats),
  beatType(BeatType).

beatsCond(Stafflines, Beats, '4') :-
  setting(beats_eps, BeatsEps),
  ccxOnStaffline(Stafflines, Beats, 'timeSig4', 4, BeatsEps).

beats(element(beats, [], [BeatsNumber])), [beats(Beats)] -->
  selectchk(stafflines(Stafflines)),
  {
    beatsCond(Stafflines, Beats, BeatsNumber)
  },
  term(Beats).

beatTypeCond(Stafflines, BeatType, '4') :-
  setting(beats_eps, BeatsEps),
  ccxOnStaffline(Stafflines, BeatType, 'timeSig4', 2, BeatsEps).

beatType(element('beat-type', [], [BeatTypeNumber])), [beatType(BeatType)] -->
  selectchk(stafflines(Stafflines)),
  {
    beatTypeCond(Stafflines, BeatType, BeatTypeNumber)
  },
  term(BeatType).

key(element(key, [], [element(fifths, [], ['0'])])) --> {true}.

noteCond(Note, element(duration, [], ['4']), element(type, [], ['whole'])) :-
  ccxEtiqsCond(Note, 'noteheadWhole').
noteCond(Note, element(duration, [], ['2']), element(type, [], ['half'])) :-
  ccxEtiqsCond(Note, 'noteheadHalf').

note(element(note, [], [Pitch, Duration, Type])), [note(Notehead)] -->
  { ccxEtiqsCond(Notehead, 1, 'notehead') },
  term(Notehead),
  { noteCond(Notehead, Duration, Type) },
  notePitch(Notehead, Pitch).
