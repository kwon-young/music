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

:- multifile delay:mode/1.

main :-
  current_prolog_flag(argv, [Goal | Args]),
  main(Goal, Args).
main(Goal, Args) :-
  apply(Goal, Args).

mainGen(Stem) :-
  atomic_list_concat(['data/', Stem, '-ids.mei'], XmlFile),
  atomic_list_concat(['data/', Stem, '-music.pl'], StructFile),
  mainGen(XmlFile, StructFile).
mainGen(XmlFile, StructFile) :-
  load_xml(XmlFile, Xml, [space(remove), number(integer)]),
  get_settings(value, _Settings, AllSettings),
  makeState(State, AllSettings),
  once(phrase(mei(Xml), [State, Struct], [_, []])),
  maplist(ground_elem, Struct, GroundStruct),
  open(StructFile, write, S),
  print_term(GroundStruct, [output(S)]),
  write(S, "."),
  close(S).
mainReco(Stem) :-
  atomic_list_concat(['data/', Stem, '-music.mei'], XmlFile),
  atomic_list_concat(['data/', Stem, '-verovio-noscope.pl'], StructFile),
  atomic_list_concat(['settings/', Stem, '-test.txt'], TestSettingsFile),
  atomic_list_concat(['settings/', Stem, '-reco.txt'], RecoSettingsFile),
  mainReco(StructFile, TestSettingsFile, XmlFile, RecoSettingsFile).
mainReco(StructFile, TestSettingsFile, XmlFile, RecoSettingsFile) :-
  open(StructFile, read, S),
  read(S, Struct),
  close(S),
  load_settings(TestSettingsFile),
  get_settings(domain, Settings, AllSettings),
  makeState(StateIn, AllSettings),
  once(phrase(mei(Xml), [StateIn, Struct], [StateOut, Rest])),
  ground_all_ids(StateOut),
  print_term(Rest, []),
  % Rest == [],
  update_settings(Settings),
  term_attvars(Xml, AttVars),
  include(interval, AttVars, Intervals),
  splitsolve(Intervals, 3),
  maplist(midpoint, Intervals, Intervals),
  % print_term(Xml, []),
  open(XmlFile, write, XmlS),
  ( ground(Xml)
  -> xml_write(XmlS, Xml, [])
  ; print_term(Xml, [output(XmlS)])
  ),
  close(XmlS),
  save_settings(RecoSettingsFile).
mainTest(Stem) :-
  atomic_list_concat(['data/', Stem, '-ids.mei'], XmlFile),
  atomic_list_concat(['data/', Stem, '-verovio.pl'], StructFile),
  atomic_list_concat(['settings/', Stem, '-test.txt'], TestSettingsFile),
  mainTest(XmlFile, StructFile, TestSettingsFile).
mainTest(XmlFile, StructFile, SettingsFile) :-
  load_xml(XmlFile, Xml, [space(remove), number(integer)]),
  open(StructFile, read, S),
  read(S, Struct),
  close(S),
  forall(setting(Mod:Name, _), restore_setting(Mod:Name)),
  get_settings(domain, Settings, AllSettings),
  makeState(State, AllSettings),
  once(phrase(mei(Xml), [State, Struct], [_, Rest])),
  print_term(Rest, []),
  % Rest == [],
  update_settings(Settings),
  save_settings(SettingsFile).

mei([pi('xml-model href="https://music-encoding.org/schema/dev/mei-all.rng" type="application/xml" schematypens="http://relaxng.org/ns/structure/1.0"'),
     pi('xml-model href="https://music-encoding.org/schema/dev/mei-all.rng" type="application/xml" schematypens="http://purl.oclc.org/dsdl/schematron"'),
     element(mei, [xmlns='http://www.music-encoding.org/ns/mei', meiversion='5.0.0-dev'], [MeiHead, Music])]) -->
  state([
    +(pageId, 0),
    +(measureN, 0),
    +(staffs, _),
    +(pitchAnchor, no)
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
  state(+(scoreDef, ScoreDef)),
  scoreDef(ScoreDef),
  section(Section).

gatherStaffDefs_([]) -->
  [].
gatherStaffDefs_([El | Childs]) -->
  when(nonvar(El), gatherStaffDefs(El)),
  when(nonvar(Childs), gatherStaffDefs_(Childs)).

gatherStaffDefs(element(grpSym, _, _)) -->
  [].
gatherStaffDefs(element(staffDef, Attr, Childs)) -->
  [element(staffDef, Attr, Childs)].
gatherStaffDefs(element(staffGrp, _, Childs)) -->
  when(nonvar(Childs), gatherStaffDefs_(Childs)).

scoreDef(element(scoreDef, ['xml:id'=Id], [StaffGrp])) -->
  { StaffGrp = element(staffGrp, _, _) },
  add_id(Id),
  statep(phrase(gatherStaffDefs(StaffGrp)), [+(staffDefs)]).

section(element(section, ['xml:id'=Id], Measures)) -->
  add_id(Id),
  state_phrase(longuest_notempty_sequence(state:scope(music:page)), Measures:measures).

pageCond(Page, PrevId, Id, [PageMargin], W, H, TopM, LeftM, BotM, RightM) :-
  nCond(PrevId, Id),
  etiqsCond(Page, 'page'),
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
  box(PageMargin),
  debug(pageCond, "~p~n", [PageMargin]).

page(PageId) -->
  statep(pageCond(Page),
         [-(pageId, _, PageId), +(bbox), o(pageWidth), o(pageHeight),
          o(topMargin), o(leftMargin), o(bottomMargin), o(rightMargin)]),
  terms(Page),
  longuest_notempty_sequence(systemN, state:scope(music:system)).

system(_Id) -->
  longuest_notempty_sequence(measureLineN, state:scope(music:measure)).

lineCond(PrevSystemStaffLines, SystemStaffLines, MinSpacing, Unit) :-
  last(PrevSystemStaffLines, PrevStaffLines),
  SystemStaffLines = [StaffLines | _],
  systemCond(PrevStaffLines, StaffLines, MinSpacing, Unit).

measure(Id) -->
  state([element(measure, ['xml:id'=Id, n=NAtom], Staffs)]:measures),
  add_id(Id),
  nCond(measureN, NAtom),
  state([+(staffWidth), o(staffDefs, StaffDefs)]),
  ( state(o(systemN, 1))
  -> state(+(systemStaffLines, SystemStaffLines))
  ; statep(lineCond, [-(systemStaffLines, _, SystemStaffLines), o(spacingSystem),
                      o(unit)])
  ),
  longuest_notempty_sequences(staffN, state:scope(music:staff),
                              [Staffs, StaffDefs, SystemStaffLines]),
  state(o(measureLineN, MeasureLineN)),
  pop_scope(measureLineN(MeasureLineN)),
  state(o(scoreDef, ScoreDef)),
  scope(barLine(ScoreDef)).

staff(element(staff, ['xml:id'=Id, n=NAtom], [Layer]),
      element(staffDef, ['xml:id'=DefId, n=NAtom, lines='5'], StaffDefChilds),
      StaffLines, Id) -->
  add_id(Id),
  add_id(DefId),
  statep(atom_number(NAtom), [o(staffN)]),
  stafflines(5, StaffLines),
  state(o(measureLineN, MeasureLineN)),
  staffDefChilds(MeasureLineN, StaffDefChilds),
  state_phrase(scope(layer(Layer)), LedgerLines:ledgerlines),
  sequence(termp, LedgerLines).

measureLineCond(PrevStaffLines, StaffLines, Eps) :-
  maplist(segEnd, PrevStaffLines, Ends),
  maplist(eps(p, Eps), Ends, Starts),
  maplist(segStart, StaffLines, Starts).

systemCond(PrevStaffLines, StaffLines, MinSpacing, Unit) :-
  last(PrevStaffLines, TopStaffLine),
  segStartY(TopStaffLine, TopY),
  StaffLines = [BottomStaffLine | _],
  segStartY(BottomStaffLine, BottomY),
  { TopY + MinSpacing * Unit =< BottomY }.

stafflinesCond(NumLines, StaffLines, Unit, Width, MinWidth, Thickness, Eps) :-
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
  maplist(eps(Eps, Width), Widths),
  { Width >= MinWidth * Unit }.

stafflines(NumLines, StaffLines) -->
  statep(stafflinesCond(NumLines, StaffLines),
         [o(unit), o(staffWidth), o(measureMinWidth), o(thickness), o(eps)]),
  state(o(staffN, StaffN)),
  ( state(o(measureLineN, 1))
  -> state(+(StaffN-stafflines, StaffLines))
  ; statep(measureLineCond, [-(StaffN-stafflines, _, StaffLines), o(eps)])
  ),
  ( { StaffN = 1 }
  -> state(+(stafflines, StaffLines))
  ; statep(systemCond, [-(stafflines, _, StaffLines), o(spacingStaff), o(unit)])
  ),
  sequence(termp, StaffLines).

staffDefChilds(N, StaffDefChilds) -->
  state([o(stafflines, [Seg | _]), +(anchor, Anchor)]),
  { segStartX(Seg, Anchor) },
  staffDefChilds_(N, StaffDefChilds).
staffDefChilds_(N, _) -->
  { dif(N, 1) }.
staffDefChilds_(1, StaffDefChilds) -->
  staffDefChilds(StaffDefChilds).

staffDefChilds(L) -->
  foldlg(optional,
         [scope(music:clef), scope(music:keySig), scope(music:meterSig)], L, []).

delay:mode(music:clefCond(ground, _, _, _)).
delay:mode(music:clefCond(_, ground, ground, _)).
clefCond(gClef, 'G', 2, '4').
clefCond(fClef, 'F', 4, '3').

clefCond(Shape, N, Clef, StaffLines, Anchor, NewAnchor, Pitch-N,
         AllSettings, LeftMargin, RightMargin, Unit, Eps) :-
  etiqsCond(Clef, Etiq),
  freeze(Etiq, memberchk(Etiq-[Width, Height, XOffset, YOffset], AllSettings)),
  delay(clefCond(Etiq, Shape, N, Octave)),
  delay(downcase_atom(Shape, PName)),
  ccxOrigin(Clef, point(X, Y)),
  eps(Eps, Anchor + Unit * LeftMargin, X),
  length(StaffLines, NumLines),
  { Index == NumLines - N + 1 },
  freeze(Index, nth1(Index, StaffLines, Line)),
  segYAtX(Line, SegY, X),
  eps(Eps, SegY, Y),
  ccxWidthHeightCond(Clef, Width, Height, Unit, Eps),
  ccxLeft(Clef, Left),
  eps(Eps, Left + XOffset*Unit, X),
  ccxTop(Clef, Top),
  eps(Eps, Top + YOffset*Unit, Y),
  ccxRight(Clef, ClefRight),
  eps(Eps, ClefRight + RightMargin*Unit, NewAnchor),
  pitch_octave_pname(Pitch, Octave, PName).

clef(element(clef, ['xml:id'=IdDef, shape=Shape, line=LineAtom], []), _Id) -->
  add_id(IdDef),
  { delay(atom_number(LineAtom, Line)) },
  statep(clefCond(Shape, Line, Clef),
         [o(stafflines), -(anchor), +(pitchAnchor), o(clefSettings),
          o(clefLeftMargin), o(clefRightMargin),
          o(unit), o(eps)]),
  termp(Clef).

keySig(element(keySig, ['xml:id'=IdDef, sig='0'], []), _Id) -->
  add_id(IdDef).

meterSigMarginCond(box(point(MeterSigLeft, _), point(MeterSigRight, _)),
                   Anchor, NewAnchor, LeftMargin, RightMargin, Unit) :-
  eps(Eps, Anchor + Unit * LeftMargin, MeterSigLeft),
  eps(Eps, MeterSigRight + RightMargin*Unit, NewAnchor).

meterSig(element(meterSig, ['xml:id'=IdDef, count=Count, unit=Unit], []), _Id) -->
  add_id(IdDef),
  statep(meterSigMarginCond(Box),
         [-(anchor), o(timeSigLeftMargin), o(timeSigRightMargin), o(unit)]),
  contour(meterSig_(Count, Unit), Box).

delay:mode(music:meterSigCond(ground, _, _)).
delay:mode(music:meterSigCond(_, ground, _)).
meterSigCond(Etiq, DigitAtom, N) :-
  atom_concat(timeSig, DigitAtom, Etiq),
  meterSigDown(N, DigitAtom).

meterSigDown(2, _).
meterSigDown(4, DigitAtom) :-
  atom_number(DigitAtom, Digit),
  Power::integer(1, 4),
  { Digit == 2 ** Power }.

meterSigCond(N, Count, MeterSig, Center, StaffLines, AllSettings, Unit, Eps) :-
  etiqsCond(MeterSig, Etiq),
  freeze(Etiq, member(Etiq-[Width, Height, XOffset, YOffset], AllSettings)),
  delay(meterSigCond(Etiq, Count, N)),
  ccxOrigin(MeterSig, point(X, Y)),
  nth1(N, StaffLines, Line),
  segYAtX(Line, SegY, X),
  eps(Eps, SegY, Y),
  ccxWidthHeightCond(MeterSig, Width, Height, Unit, Eps),
  ccxLeft(MeterSig, Left),
  eps(Eps, Left + XOffset*Unit, X),
  ccxTop(MeterSig, Top),
  eps(Eps, Top + YOffset*Unit, Y),
  ccxRight(MeterSig, MeterSigRight),
  eps(4*Eps, (Left + MeterSigRight) / 2, Center).

meterSig_(Count, Unit) -->
  statep(meterSigCond(2, Count, MeterSigUp, Center),
         [o(stafflines), o(timeSigSettings), o(unit), o(eps)]),
  termp(MeterSigUp),
  statep(meterSigCond(4, Unit, MeterSigDown, Center),
         [o(stafflines), o(timeSigSettings), o(unit), o(eps)]),
  termp(MeterSigDown).

layer(element(layer, ['xml:id'=Id, n='1'], Childs), Id) -->
  add_id(Id),
  sequence(find(music:layerChild), Childs).

layerChild(Child) -->
  scope(note(Child)).
layerChild(Child) -->
  scope(rest(Child)).

delay:mode(music:restCond(ground, _)).
delay:mode(music:restCond(_, ground)).
restCond(rest8th, '8').

restCond(Rest, Dur, StaffLines,
         LeftAnchor, RightAnchor, RestSettings, LeftMargin, RightMargin,
         Unit, Eps) :-
  etiqsCond(Rest, Etiq),
  freeze(Etiq, memberchk(Etiq-[Width, Height, XOffset, YOffset], RestSettings)),
  delay(restCond(Etiq, Dur)),
  ccxWidthHeightCond(Rest, Width, Height, Unit, Eps),
  ccxOrigin(Rest, point(X, Y)),
  ccxRight(Rest, Right),
  {
    LeftAnchor + LeftMargin*Unit =< X,
    Right + RightMargin*Unit =< RightAnchor
  },
  ccxTop(Rest, Top),
  eps(Eps, Top + Unit * YOffset, Y),
  ccxLeft(Rest, Left),
  eps(Eps, Left + Unit * XOffset, X),
  nth1(3, StaffLines, Line),
  segYAtX(Line, LineY, X),
  eps(Eps, LineY, Y).

rest(element(rest, ['xml:id'=Id, dur=Dur], []), Id) -->
  add_id(Id),
  statep(restCond(Rest, Dur),
         [o(stafflines), -(anchor), o(restSettings), o(restLeftMargin),
          o(restRightMargin), o(unit), o(eps)]),
  term(Rest).

marginCond(box(point(Left, _), point(Right, _)),
               LeftAnchor, RightAnchor,
               LeftMargin, RightMargin, Unit) :-
  {
    LeftAnchor + LeftMargin*Unit =< Left,
    Right + RightMargin*Unit =< RightAnchor
  }.

note(element(note, ['xml:id'=Id, dur=Dur, oct=Oct, pname=PName], NoteChilds),
     Id) -->
  add_id(Id),
  statep(marginCond(NoteContour),
         [-(anchor), o(noteLeftMargin), o(noteRightMargin), o(unit)]),
  contour(note(Dur, Oct, PName, NoteChilds), NoteContour).

note(Dur, Oct, PName, NoteChilds) -->
  contour(scope(music:notehead(Dur, Oct, PName)), NoteHeadContour),
  foldlg(optional, [scope(stem), scope(accid(NoteHeadContour))], NoteChilds, []).

delay:mode(music:number_pname(ground, _)).
delay:mode(music:number_pname(_, ground)).
number_pname(0, c).
number_pname(1, d).
number_pname(2, e).
number_pname(3, f).
number_pname(4, g).
number_pname(5, a).
number_pname(6, b).

pitch_octave_pname(Pitch, Octave, PName) :-
  delay(number_pname(PNameNumber, PName)),
  delay(atom_number(Octave, OctaveNumber)),
  PNameNumber::integer(0, 6),
  OctaveNumber::integer(0, 9),
  { Pitch == OctaveNumber * 7 + PNameNumber }.

delay:mode(music:noteHeadCond(ground, _)).
delay:mode(music:noteHeadCond(_, ground)).
noteHeadCond(noteheadWhole, 1).
noteHeadCond(noteheadWhite, 2).
noteHeadCond(noteheadBlack, Dur) :-
  X::integer(2, 8),
  { Dur == 2**X }.

noteHeadCond(DurAtom, Oct, PName, NoteHead, Dur, Pitch, BasePitch-BaseN, StaffLines,
             NoteHeadSettings, Unit, Eps) :-
  etiqsCond(NoteHead, Etiq),
  freeze(Etiq, memberchk(Etiq-[Width, Height], NoteHeadSettings)),
  delay(atom_number(DurAtom, Dur)),
  delay(noteHeadCond(Etiq, Dur)),
  ccxWidthHeightCond(NoteHead, Width, Height, Unit, Eps),
  ccxOrigin(NoteHead, Origin),
  ccxLeftTopRightBottom(NoteHead, point(Left, Top), point(_, Bottom)),
  { Middle == (Top + Bottom) / 2 },
  eps(p, Eps, Origin, point(Left, Middle)),
  pitch_octave_pname(Pitch, Oct, PName),
  { RelativePitch == BasePitch - Pitch },
  length(StaffLines, NumLines),
  { Index == NumLines - BaseN + 1 },
  nth1(Index, StaffLines, BaseSeg),
  segYAtX(BaseSeg, BaseY, Left),
  eps(Eps, BaseY + Unit * RelativePitch, Middle).

notehead(Dur, Oct, PName, Id) -->
  add_id(Id),
  statep(noteHeadCond(Dur, Oct, PName),
         [+(notehead, NoteHead), +(duration), +(pitch), o(pitchAnchor), o(stafflines),
          o(noteheadSettings), o(unit), o(eps)]),
  termp(NoteHead),
  pop_scope(pop_scope(pop_scope(scope(ledgerLines)))).

ledgerlinesCond(LedgerLines, NoteHead, StaffLines, Pitch, BasePitch-BaseN,
                Extension, Thickness, Unit, Eps) :-
  { Above == (Pitch > BasePitch) },
  N::integer(0, _),
  ledgerlinesCond(Above, N, Goal, StaffLines, Pitch, BasePitch-BaseN),
  enumerate(N),
  stafflinesCond(N, LedgerLines, Unit, Width, _, Thickness, Eps),
  ( call(Goal, LedgerLines, LedgerLine)
  -> ccxWidth(NoteHead, NoteHeadWidth),
    eps(Eps, Width,  NoteHeadWidth + 2*Extension*Unit),
    centerCond(NoteHead, LedgerLine, Eps)
  ; true).

first([X | _], X).

ledgerlinesCond(1, N, first, StaffLines, Pitch, BasePitch-BaseN) :-
  length(StaffLines, NumStaffLines),
  Offset::integer(0, 1),
  N1::integer(0, _),
  {
    Pitch == BasePitch + N1 * 2 + Offset,
    N == max(0, N1 - (NumStaffLines - BaseN))
  }.
ledgerlinesCond(0, N, last, _StaffLines, Pitch, BasePitch-BaseN) :-
  Offset::integer(0, 1),
  N1::integer(0, _),
  {
    Pitch == BasePitch - N1 * 2 - Offset,
    N == max(0, N1 - (BaseN - 1))
  }.

ledgerLines(Id) -->
  add_id(Id),
  statep(ledgerlinesCond(LedgerLines),
         [o(notehead), o(stafflines), o(pitch), o(pitchAnchor),
          o(ledgerlineExtension), o(ledgerlineThickness), o(unit), o(eps)]),
  sequence(selectp, LedgerLines),
  state(LedgerLines:ledgerlines).

stemCond(Stem, down, StemLengthAtom, NoteHead,
         AllSettings, StemWidth, Unit, Eps) :-
  segThickness(Stem, Thickness),
  eps(Eps, StemWidth * Unit, Thickness),
  etiqsCond(NoteHead, Etiq),
  freeze(Etiq, memberchk(Etiq-[Offset], AllSettings)),
  ccxOrigin(NoteHead, point(NoteHeadX, NoteHeadY)),
  segHV(v, left, top, Stem, StemRightTop),
  eps(p, Eps, point(NoteHeadX, NoteHeadY + Unit * Offset), StemRightTop),
  segEndY(Stem, StemBottom),
  delay(atom_number(StemLengthAtom, StemLength)),
  { StemLength * Unit == StemBottom - NoteHeadY }.
  
stem(element(stem, ['xml:id'=Id, len=Len, dir=Dir], []), Id) -->
  add_id(Id),
  statep(stemCond(Stem, Dir, Len),
         [o(notehead), o(stemSettings), o(stemWidth), o(unit), o(eps)]),
  termp(Stem),
  ( scope(flag(Stem, Dir))
  *-> []
  ; []
  ).

delay:mode(music:flagCond(ground, _)).
delay:mode(music:flagCond(_, ground)).
flagCond('flag8thDown', 8).

flagCond(Flag, Stem, down, Dur, Settings, Unit, Eps) :-
  segHV(v, left, bottom, Stem, StemLeftBottom),
  ccxLeftBottom(Flag, FlagLeftBottom),
  eps(p, Eps, StemLeftBottom, FlagLeftBottom),
  etiqsCond(Flag, Etiq),
  delay(flagCond(Etiq, Dur)),
  freeze(Etiq, memberchk(Etiq-[Width, Height], Settings)),
  ccxWidthHeightCond(Flag, Width, Height, Unit, Eps).

flag(Stem, Dir, Id) -->
  add_id(Id),
  statep(flagCond(Flag, Stem, Dir), [o(duration), o(flagSettings), o(unit), o(eps)]),
  termp(Flag).

delay:mode(music:accidCond(ground, _)).
delay:mode(music:accidCond(_, ground)).
accidCond(accidentalSharp, s).
accidCond(accidentalFlat, f).
accidCond(accidentalNatural, n).

accidCond(Accidental, Shape, box(point(BoxLeft, _), point(_, _)),
          NoteHead, Settings, _LeftMargin, RightMargin, Unit, Eps) :-
  etiqsCond(Accidental, Etiq),
  delay(accidCond(Etiq, Shape)),
  freeze(Etiq, memberchk(Etiq-[Width, Height, XOffset, YOffset], Settings)),
  ccxWidthHeightCond(Accidental, Width, Height, Unit, Eps),
  ccxOrigin(Accidental, point(X, Y)),
  ccxLeft(Accidental, Left),
  eps(Eps, Left + XOffset*Unit, X),
  ccxTop(Accidental, Top),
  eps(Eps, Top + YOffset*Unit, Y),
  ccxOrigin(NoteHead, point(NoteHeadLeft, NoteheadY)),
  eps(Eps, Y, NoteheadY),
  ccxRight(Accidental, Right),
  { Right + RightMargin*Unit =< NoteHeadLeft },
  { Right =< BoxLeft }.

accid(NoteHeadContour, element(accid, ['xml:id'=Id, accid=Shape], []), Id) -->
  add_id(Id),
  statep(accidCond(Accidental, Shape, NoteHeadContour),
         [o(notehead), o(accidentalSettings), o(accidentalLeftMargin),
          o(accidentalRightMargin), o(unit), o(eps)]),
  termp(Accidental).

measureLineN(N) -->
  { dif(N, 1) }.
measureLineN(1) -->
  reify(systemLine, Result),
  state(+(systemLine, Result)),
  stateg(gather_grpSym, [o(scoreDef), o(staffDefs), o(systemStaffLines)]).

systemLineCond(SystemLine, Anchor, Staffs, Thickness, Unit, Eps) :-
  segHV(v, left, top, SystemLine, SystemLineLeftTop),
  Staffs = [[TopStaffLine | _], _ | _],
  segStart(TopStaffLine, TopStaffLineLeft),
  eps(p, Eps, SystemLineLeftTop, TopStaffLineLeft),
  last(Staffs, LastStaff),
  last(LastStaff, LastStaffLine),
  segHV(v, left, bottom, SystemLine, SystemLineLeftBottom),
  segStart(LastStaffLine, LastStaffLineLeft),
  eps(p, Eps, SystemLineLeftBottom, LastStaffLineLeft),
  segThickness(SystemLine, SystemLineThickness),
  eps(Eps, Unit*Thickness, SystemLineThickness),
  etiqsCond(SystemLine, system),
  SystemLineLeftTop = point(LeftTop, _),
  SystemLineLeftBottom = point(LeftBottom, _),
  { Anchor == min(LeftTop, LeftBottom) }.

systemLine -->
  statep(systemLineCond(SystemLine),
         [+(anchor-grpSym), o(systemStaffLines), o(barLineThickness),
          o(unit), o(eps)]),
  termp(SystemLine).

gather_grpSym(element(scoreDef, _, Childs), StaffDefs, SystemStaffLines) -->
  gather_grpSym_(Childs, StaffDefs, SystemStaffLines).

gather_split([El1 | L1], [El2 | L2],
             [El1 | Grouped1], [El2 | Grouped2],
             Rest1, Rest2) :-
  gather_split(L1, L2, Grouped1, Grouped2, Rest1, Rest2).
gather_split(Rest1, Rest2, [], [], Rest1, Rest2).

gather_grpSym_([], [], []) --> [].
gather_grpSym_([element(staffGrp, ['xml:id'=Id | _], [GrpSym | Childs]) | Rest],
               StaffDefs,
               SystemStaffLines) -->
  add_id(Id),
  state(o(systemLine, true)),
  scope(grpSym(GrpSym,
               StaffDefs, SystemStaffLines,
               GroupStaffDefs, GroupStaffLines,
               RestStaffDefs, RestStaffLines)),
  gather_grpSym_(Childs, GroupStaffDefs, GroupStaffLines),
  gather_grpSym_(Rest, RestStaffDefs, RestStaffLines).
gather_grpSym_([StaffDef | Childs],
               [StaffDef | StaffDefs],
               [_ | SystemStaffLines]) -->
  { StaffDef = element(staffDef, _, _) },
  gather_grpSym_(Childs, StaffDefs, SystemStaffLines).
gather_grpSym_([element(staffGrp, ['xml:id'=Id | _], Childs) | Rest],
              StaffDefs,
              SystemStaffLines) -->
  add_id(Id),
  { gather_split(StaffDefs, SystemStaffLines,
                 GroupStaffDefs, GroupStaffLines,
                 RestChilds, RestStaffLines) },
  gather_grpSym_(Childs, GroupStaffDefs, GroupStaffLines),
  gather_grpSym_(Rest, RestChilds, RestStaffLines).

braceCond(Brace,
          [StaffDef | StaffDefs],
          [[FirstStaffLine | OtherStaffLines] | SystemStaffLines],
          [StaffDef | BracedStaffDefs],
          [[FirstStaffLine | OtherStaffLines] | BracedStaffLines],
          OtherStaffDefs, OtherSystemStaffLines,
          Anchor, NewAnchor,
          BraceWidth, BraceVerticalMargin, GrpSymMargin, Unit, Eps) :-
  ccxLeft(Brace, NewAnchor),
  ccxRight(Brace, BraceRight),
  eps(Eps, Anchor - GrpSymMargin * Unit, BraceRight),
  segStartY(FirstStaffLine, FirstStaffLineY),
  ccxTop(Brace, BraceTop),
  eps(Eps, FirstStaffLineY + BraceVerticalMargin * Unit, BraceTop),
  ccxWidth(Brace, Width),
  eps(Eps, BraceWidth * Unit, Width),
  etiqsCond(Brace, brace),
  when(nonvar(StaffDefs),
    braceCondPost(StaffDefs, SystemStaffLines,
                  BracedStaffDefs, BracedStaffLines,
                  OtherStaffDefs, OtherSystemStaffLines,
                  Brace, BraceVerticalMargin, Unit, Eps)).
braceCondPost([StaffDef | StaffDefs], [StaffLines | SystemStaffLines],
              [StaffDef | BracedStaffDefs], [StaffLines | BracedStaffLines],
              OtherStaffDefs, OtherSystemStaffLines,
              Brace, BraceVerticalMargin, Unit, Eps) :-
  when(nonvar(StaffDefs),
    braceCondPost(StaffDefs, SystemStaffLines,
                  BracedStaffDefs, BracedStaffLines,
                  OtherStaffDefs, OtherSystemStaffLines,
                  Brace, BraceVerticalMargin, Unit, Eps)).
braceCondPost([StaffDef | OtherStaffDefs], [StaffLines | OtherSystemStaffLines],
              [StaffDef], [StaffLines],
              OtherStaffDefs, OtherSystemStaffLines,
              Brace, BraceVerticalMargin, Unit, Eps) :-
  delay(last(StaffLines, BottomStaffLine)),
  segStartY(BottomStaffLine, BottomStaffLineY),
  ccxBottom(Brace, BraceBottom),
  eps(Eps, BottomStaffLineY - BraceVerticalMargin * Unit, BraceBottom).

bracketCond(BracketSeg, BracketTop, BracketBottom,
            [StaffDef | StaffDefs],
            [[TopStaffLine | OtherStaffLines] | SystemStaffLines],
            [StaffDef | GroupedStaffDefs],
            [[TopStaffLine | OtherStaffLines] | GroupedStaffLines],
            OtherStaffDefs, OtherSystemStaffLines,
            Anchor, NewAnchor,
            BracketThickness, BracketVerticalOffset, BracketOverlap, GrpSymMargin,
            Unit, Eps) :-
  segThickness(BracketSeg, Thickness),
  eps(Eps, BracketThickness * Unit, Thickness),
  segHV(v, right, top, BracketSeg, point(BStartX, BStartY)),
  eps(Eps, Anchor - GrpSymMargin * Unit, BStartX),
  segHV(v, right, bottom, BracketSeg, point(BEndX, _)),
  eps(Eps, Anchor - GrpSymMargin * Unit, BEndX),
  segStartY(TopStaffLine, TopStaffLineY),
  eps(Eps, TopStaffLineY - BracketVerticalOffset * Unit, BStartY),
  ccxLeft(BracketTop, BracketTopLeft),
  ccxBottom(BracketTop, BracketTopBottom),
  etiqsCond(BracketTop, bracketTop),
  segHV(v, left, top, BracketSeg, BracketSegLeftTop),
  eps(p, Eps, BracketSegLeftTop,
      point(BracketTopLeft, BracketTopBottom-BracketOverlap)),
  ccxLeft(BracketBottom, BracketBottomLeft),
  ccxTop(BracketBottom, BracketBottomBottom),
  etiqsCond(BracketBottom, bracketBottom),
  segHV(v, left, bottom, BracketSeg, BracketSegLeftBottom),
  eps(p, Eps, BracketSegLeftBottom,
      point(BracketBottomLeft, BracketBottomBottom+BracketOverlap)),
  BracketSegLeftTop = point(LeftTop, _),
  BracketSegLeftBottom = point(LeftBottom, _),
  { NewAnchor == min(LeftTop, LeftBottom) },
  when(nonvar(StaffDefs),
    bracketCondPost(StaffDefs, SystemStaffLines,
                    GroupedStaffDefs, GroupedStaffLines,
                    OtherStaffDefs, OtherSystemStaffLines,
                    BracketSeg, BracketVerticalOffset, Unit, Eps)).
bracketCondPost([StaffDef | StaffDefs], [StaffLines | SystemStaffLines],
                [StaffDef | GroupedStaffDefs], [StaffLines | GroupedStaffLines],
                OtherStaffDefs, OtherSystemStaffLines,
                BracketSeg, BracketVerticalOffset, Unit, Eps) :-
  when(nonvar(StaffDefs),
    bracketCondPost(StaffDefs, SystemStaffLines,
                    GroupedStaffDefs, GroupedStaffLines,
                    OtherStaffDefs, OtherSystemStaffLines,
                    BracketSeg, BracketVerticalOffset, Unit, Eps)).
bracketCondPost([StaffDef | OtherStaffDefs], [StaffLines | OtherSystemStaffLines],
                [StaffDef], [StaffLines],
                OtherStaffDefs, OtherSystemStaffLines,
                BracketSeg, BracketVerticalOffset, Unit, Eps) :-
  delay(last(StaffLines, BottomStaffLine)),
  segStartY(BottomStaffLine, BottomStaffLineY),
  segEndY(BracketSeg, BEndY),
  eps(Eps, BottomStaffLineY + BracketVerticalOffset * Unit, BEndY).

grpSym(element(grpSym, ['xml:id'=DefId, symbol=brace], []),
       StaffDefs,
       SystemStaffLines,
       BracedStaffDefs,
       BracedStaffLines,
       OtherStaffDefs, OtherStaffLines, _RealId) -->
  add_id(DefId),
  termp(Brace),
  statep(braceCond(Brace, StaffDefs, SystemStaffLines,
                   BracedStaffDefs, BracedStaffLines,
                   OtherStaffDefs, OtherStaffLines),
         [-(anchor-grpSym), o(braceWidth), o(braceVerticalMargin), o(braceMargin),
          o(unit), o(eps)]).
grpSym(element(grpSym, ['xml:id'=DefId, symbol=bracket], []),
       StaffDefs, SystemStaffLines,
       GroupedStaffDefs, GroupedSystemStaffLines,
       OtherStaffDefs, OtherSystemStaffLines,
       _RealId) -->
  add_id(DefId),
  statep(bracketCond(BracketSeg, BracketTop, BracketBottom,
                     StaffDefs, SystemStaffLines,
                     GroupedStaffDefs, GroupedSystemStaffLines,
                     OtherStaffDefs, OtherSystemStaffLines),
         [-(anchor-grpSym), o(bracketThickness), o(bracketVerticalOffset),
          o(bracketOverlap), o(bracketMargin), o(unit), o(eps)]),
  termp(BracketTop),
  termp(BracketBottom),
  termp(BracketSeg).

barlineCond(BarLine, StaffLines, Thickness, Unit, Eps) :-
  nth1(1, StaffLines, TopLine),
  last(StaffLines, BottomLine),
  barlineCond(BarLine, TopLine, BottomLine, Thickness, Unit, Eps).
barlineCond(BarLine, TopLine, BottomLine, Thickness, Unit, Eps) :-
  maplist(segEnd, [TopLine, BottomLine], StaffLinesPoints),
  segHV(v, right, top, BarLine, BarLineTopRight),
  segHV(v, right, bottom, BarLine, BarLineBottomRight),
  maplist(eps(p, Eps), [BarLineTopRight, BarLineBottomRight], StaffLinesPoints),
  segThickness(BarLine, BarLineThickness),
  eps(Eps, Unit*Thickness, BarLineThickness).

barLine(element(scoreDef, _, [StaffGrp]), _Id) -->
  state([o(systemStaffLines, SystemStaffLines)]),
  state_phrase(barline(StaffGrp, SystemStaffLines, []), SystemStaffLines:dcg).
barline(element(staffGrp, ['xml:id'=_Id | StaffGrpAttr], Childs), L, R) -->
  barline(Childs, L1, []),
  { append(L1, R, L) },
  staffGrpAttr(StaffGrpAttr, L1).
barline(element(staffDef, _, _), [StaffLines | R], R) -->
  state([StaffLines]:dcg),
  barline_(StaffLines).
barline(element(grpSym, _, _), L, L) --> [].
barline([Child | Childs], L, R) -->
  barline(Child, L, L1),
  barline(Childs, L1, R).
barline([], L, L) --> [].

barline_(StaffLines) -->
  statep(barlineCond(BarLine, StaffLines), [o(barLineThickness), o(unit), o(eps)]),
  termp(BarLine).

staffGrpAttr(['bar.thru'='true'], [H | T]) -->
  foldlg(systemBarLine, T, H, _).
staffGrpAttr([], _) --> [].

systemBarLineCond(BarLine, TopStaffLines, BottomStaffLines, Thickness, Unit, Eps) :-
  last(TopStaffLines, TopLine),
  BottomStaffLines = [BottomLine | _],
  barlineCond(BarLine, TopLine, BottomLine, Thickness, Unit, Eps).

systemBarLine(BottomGroup, TopGroup, BottomGroup) -->
  statep(systemBarLineCond(BarLine, TopGroup, BottomGroup),
         [o(barLineThickness), o(unit), o(eps)]),
  termp(BarLine).

debug(Topic, Fmt, Args) -->
  state(o(scope, Scope)),
  {
    string_concat("~p ~p ", Fmt, NewFmt),
    append([Topic, Scope], Args, NewArgs),
    debug(Topic, NewFmt, NewArgs)
  }.
