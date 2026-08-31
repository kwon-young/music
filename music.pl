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
  print_term(Rest, []), nl,
  update_settings(Settings),
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
  load_mei(XmlFile, Xml),
  open(StructFile, read, S),
  read(S, Struct),
  close(S),
  forall(setting(Mod:Name, _), restore_setting(Mod:Name)),
  get_settings(domain, Settings, AllSettings),
  makeState(State, AllSettings),
  once(phrase(mei(Xml), [State, Struct], [StateOut, Rest])),
  ground_all_ids(StateOut),
  % memberchk(unit-Unit, Settings),
  % Unit = 18,
  % memberchk(eps-Eps, Settings),
  % global_minimize(Eps, Eps),
  % upper_bound(Eps),
  % term_attvars(Xml, AttVars),
  % include(interval, AttVars, Intervals),
  % map_list_to_pairs(delta, Intervals, DeltaIntervals),
  % keysort(DeltaIntervals, SortedDeltaIntervals),
  % pairs_values(SortedDeltaIntervals, SortedIntervals),
  % % partition(small, Intervals, SmallIntervals, LargeIntervals),
  % splitsolve(SortedIntervals),
  % maplist(midpoint, SortedIntervals, SortedIntervals),
  % print_term(Eps-Unit, []), nl,
  % midpoint(Unit, Unit),
  % print_term(Xml, []), nl,
  print_term(Rest, []), nl,
  % Rest == [],
  % open(XmlFile, write, XmlS),
  % ( ground(Xml)
  % -> xml_write(XmlS, Xml, [])
  % ; print_term(Xml, [output(XmlS)])
  % ),
  % close(XmlS),
  update_settings(Settings),
  save_settings(SettingsFile).

load_mei(Filename, Mei) :-
  ( file_name_extension(_, mei, Filename)
  -> load_xml(Filename, Mei, [space(remove), number(integer)])
  ; file_name_extension(_, pl, Filename),
    open(Filename, read, S),
    read(S, Mei),
    close(S)
  ).

mei([pi('xml-model href="https://music-encoding.org/schema/dev/mei-all.rng" type="application/xml" schematypens="http://relaxng.org/ns/structure/1.0"'),
     pi('xml-model href="https://music-encoding.org/schema/dev/mei-all.rng" type="application/xml" schematypens="http://purl.oclc.org/dsdl/schematron"'),
     element(mei, [xmlns='http://www.music-encoding.org/ns/mei', meiversion='6.0-dev'], [MeiHead, Music])]) -->
  state([
    +(pageId, 0),
    +(measureN, 0),
    +(staffs, _),
    +(pitchAnchor, no),
    +(beam-0, end-no),
    +(beam-1, end-no),
    +(beam-2, end-no),
    +(beam-3, end-no),
    +(beam-4, end-no),
    +(timestamp, 0)
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
  staffsPre,
  longuest_notempty_sequence(systemN, state:scope(music:system)).

reco -->
  select(X), {ground(X)}.

:- det(staffsPre/2).
staffsPre -->
  stafflinesPre(5, AllStaffLines),
  statep(staffsPre(Staffs, AllStaffLines, RestStaffLines), [o(eps)]),
  { predsort(sortStaffs, Staffs, SortedStaffs),
    flatten(SortedStaffs, Lines),
    append(RestStaffLines, RestLines),
    append(Lines, RestLines, AllLines),
    reverse(AllLines, RLines)
  },
  sequence(add, RLines).

stafflinesPre(NumLines, [StaffLines | Rest]) -->
  reco,
  statep(stafflinesCond(NumLines, StaffLines),
         [o(unit), +(staffWidth), o(measureMinWidth), o(thickness), o(eps)]),
  sequence(termp, StaffLines),
  !,
  stafflinesPre(NumLines, Rest).
stafflinesPre(_, []) --> [].

staffsPre([[StaffLines | Staff] | Staffs], [StaffLines | AllStaffLines], RRest, Eps) :-
  staffPre(Staff, StaffLines, AllStaffLines, Rest, Eps),
  !,
  staffsPre(Staffs, Rest, RRest, Eps).
staffsPre([], AllStaffLines, AllStaffLines, _).

staffPre([StaffLines | Staff], PrevStaffLines, AllStaffLines, Rest, Eps) :-
  lists:select(StaffLines, AllStaffLines, RestStaffLines),
  measureLineCond(PrevStaffLines, StaffLines, Eps),
  !,
  staffPre(Staff, StaffLines, RestStaffLines, Rest, Eps).
staffPre([], _, AllStaffLines, AllStaffLines, _).

sortStaffs(<, Staff1, Staff2) :-
  Staff1 = [StaffLines1 | _],
  last(StaffLines1, Seg1),
  Staff2 = [[Seg2 | _] | _],
  segStartY(Seg1, Y1),
  segStartY(Seg2, Y2),
  Y1 =< Y2,
  !.
sortStaffs(>, Staff1, Staff2) :-
  Staff1 = [StaffLines1 | _],
  last(StaffLines1, Seg1),
  Staff2 = [[Seg2 | _] | _],
  segStartY(Seg1, Y1),
  segStartY(Seg2, Y2),
  Y1 > Y2.

system(_Id) -->
  longuest_notempty_sequence(measureLineN, state:scope(music:measure)).

lineCond(PrevSystemStaffLines, SystemStaffLines, MinSpacing, Unit) :-
  last(PrevSystemStaffLines, PrevStaffLines),
  SystemStaffLines = [StaffLines | _],
  systemCond(PrevStaffLines, StaffLines, MinSpacing, Unit).

measureChilds(Childs, Staffs, Dynams, BeamSpans) :-
  ( var(Childs)
  -> measureChildsReco(Childs, Staffs, Dynams, BeamSpans)
  ; measureChildsGen(Childs, Staffs, Dynams, BeamSpans)
  ).
measureChildsGen([], [], [], []).
measureChildsGen([Staff | Childs], [Staff | Staffs], Dynams, BeamSpans) :-
  Staff = element(staff, _, _),
  measureChildsGen(Childs, Staffs, Dynams, BeamSpans).
measureChildsGen([Child | Childs], Staffs, [Dynam | Dynams], BeamSpans) :-
  Child = element(dynam, _, _),
  Dynam = element(dynam, _, _),
  dynam_(Child, Dynam),
  measureChildsGen(Childs, Staffs, Dynams, BeamSpans).
measureChildsGen([Child | Childs], Staffs, Dynams, [BeamSpan | BeamSpans]) :-
  Child = element(beamSpan, _, _),
  BeamSpan = element(beamSpan, _, _),
  beamSpan_(Child, BeamSpan),
  measureChildsGen(Childs, Staffs, Dynams, BeamSpans).

measureChildsReco(Childs, [Staff | Staffs], Dynams, BeamSpans) =>
  Childs = [Staff | Rest],
  measureChildsReco(Rest, Staffs, Dynams, BeamSpans).
measureChildsReco(Childs, Staffs, [Dynam | Dynams], BeamSpans) =>
  Childs = [Child | Rest],
  dynam_(Child, Dynam),
  measureChildsReco(Rest, Staffs, Dynams, BeamSpans).
measureChildsReco(Childs, Staffs, Dynams, [BeamSpan | BeamSpans]) =>
  Childs = [Child | Rest],
  beamSpan_(Child, BeamSpan),
  measureChildsReco(Rest, Staffs, Dynams, BeamSpans).
measureChildsReco(Childs, Staffs, Dynams, BeamSpans) =>
  ( include(var, [Staffs, Dynams, BeamSpans], [H | T])
  -> foldl([Var, NonVar, (nonvar(Var) ; NonVar)]>>true,
           T, nonvar(H), Expr),
     when(Expr, measureChildsReco(Childs, Staffs, Dynams, BeamSpans))
  ; Childs = []
  ).

:- begin_tests(measureChilds).

test(lazyness, [nondet]) :-
  measureChilds(Childs, Staffs, Dynams, _BeamSpans),
  S1 = element(staff, _, _),
  S2 = element(staff, _, _),
  Staffs = [S1, S2],
  D = element(dynam, _, _),
  Dynams = [D],
  Childs = [S1, S2, D | _].

test(empty) :-
  measureChilds([], [], [], []).

test(forward_simple, [nondet]) :-
  S = element(staff, _, _),
  D_in = element(dynam, _, _),
  B_in = element(beamSpan, _, _),
  Childs = [S, D_in, B_in],
  measureChilds(Childs, Staffs, Dynams, BeamSpans),
  D_out = element(dynam, _, _),
  B_out = element(beamSpan, _, _),
  Staffs = [S],
  Dynams = [D_out],
  BeamSpans = [B_out].

test(backward_simple, [nondet]) :-
  S = element(staff, _, _),
  D_in = element(dynam, _, _),
  B_in = element(beamSpan, _, _),
  Staffs = [S],
  Dynams = [D_in],
  BeamSpans = [B_in],
  measureChilds(Childs, Staffs, Dynams, BeamSpans),
  D_out = element(dynam, _, _),
  B_out = element(beamSpan, _, _),
  Childs = [S, D_out, B_out].

test(forward_complex, [nondet]) :-
  S1 = element(staff, _, _),
  S2 = element(staff, _, _),
  D1_in = element(dynam, _, _),
  D2_in = element(dynam, _, _),
  B1_in = element(beamSpan, _, _),
  Childs = [S1, D1_in, S2, B1_in, D2_in],
  measureChilds(Childs, Staffs, Dynams, BeamSpans),
  Staffs = [S1, S2],
  D1_out = element(dynam, _, _),
  D2_out = element(dynam, _, _),
  Dynams = [D1_out, D2_out],
  B1_out = element(beamSpan, _, _),
  BeamSpans = [B1_out].

test(backward_complex, [nondet]) :-
  S1 = element(staff, _, _),
  S2 = element(staff, _, _),
  Staffs = [S1, S2],
  D1_in = element(dynam, _, _),
  D2_in = element(dynam, _, _),
  Dynams = [D1_in, D2_in],
  B1_in = element(beamSpan, _, _),
  BeamSpans = [B1_in],
  measureChilds(Childs, Staffs, Dynams, BeamSpans),
  D1_out = element(dynam, _, _),
  D2_out = element(dynam, _, _),
  B1_out = element(beamSpan, _, _),
  Childs = [S1, S2, D1_out, D2_out, B1_out].

test(forward_no_dynams, [nondet]) :-
  S = element(staff, _, _),
  B_in = element(beamSpan, _, _),
  Childs = [S, B_in],
  measureChilds(Childs, Staffs, Dynams, BeamSpans),
  Staffs = [S],
  Dynams = [],
  B_out = element(beamSpan, _, _),
  BeamSpans = [B_out].

test(backward_no_dynams, [nondet]) :-
  S = element(staff, _, _),
  B_in = element(beamSpan, _, _),
  Staffs = [S],
  Dynams = [],
  BeamSpans = [B_in],
  measureChilds(Childs, Staffs, Dynams, BeamSpans),
  B_out = element(beamSpan, _, _),
  Childs = [S, B_out].

:- end_tests(measureChilds).

beamSpan_(
    element(beamSpan, ['xml:id'=Id, plist=PListIn, startid=Start, endid=End], []),
    element(beamSpan, ['xml:id'=Id, plist=PListOut, startid=Start, endid=End], [])
  ) :-
  PListOut = [Start | _],
  when((ground(PListIn) ; ground(PListOut)),
       atomic_list_concat(PListOut, ' ', PListIn)),
  when(ground(End), once(last(PListOut, End))).
dynam_(
    element(dynam, ['xml:id'=Id, ho=HOffsetAtom, place=Place, staff=StaffIn, startid=StartId], [Etiq]),
    element(dynam, ['xml:id'=Id, ho=HOffset, place=Place, staff=StaffOut, startid=StartId], [Etiq])
  ) :-
  when((ground(StaffIn) ; ground(Atoms)), atomic_list_concat(Atoms, ' ', StaffIn)),
  when((ground(StaffOut) ; ground(Atoms)),
       (same_length(StaffOut, Atoms), maplist(atom_number, Atoms, StaffOut))),
  delay(vu(HOffsetAtom, HOffset)).

measure(Id) -->
  state([element(measure, ['xml:id'=Id, n=NAtom], Childs)]:measures),
  add_id(Id),
  nCond(measureN, NAtom),
  state([+(staffWidth), o(staffDefs, StaffDefs)]),
  ( state(o(systemN, 1))
  -> state(+(systemStaffLines, SystemStaffLines))
  ; statep(lineCond, [-(systemStaffLines, _, SystemStaffLines), o(spacingSystem),
                      o(unit)])
  ),
  statep(measureChilds(Childs, Staffs), [+(dynams, Dynams), +(beamSpans)]),
  state(+(timestampAnchors, TimestampAnchors)),
  longuest_notempty_sequences(staffN, state:scope(music:staff),
                              [Staffs, StaffDefs, SystemStaffLines]),
  state(o(timestampAnchors, [])),
  state(+(timestampAnchors, TimestampAnchors)),
  longuest_sequence(state:scope(music:dynam), Dynams),
  state(o(measureLineN, MeasureLineN)),
  pop_scope(measureLineN(MeasureLineN)),
  state(o(scoreDef, ScoreDef)),
  scope(barLine(ScoreDef)).

delay:mode(music:dynamCond(ground, _)).
delay:mode(music:dynamCond(_, ground)).
dynamCond(dynamicPP, pp).

dynamCond(Dynam, Place, StaffNs, HPlace, HOffset, Atom, Staffs, DynamSettings,
          TimestampAnchors, Unit, Eps) :-
  etiqsCond(Dynam, Etiq),
  freeze(Etiq, memberchk(Etiq-[Width, Height, XOffset, YOffset, Advance], DynamSettings)),
  delay(dynamCond(Etiq, Atom)),
  ccxWidthHeightCond(Dynam, Width, Height, Unit, Eps),
  ccxOrigin(Dynam, point(X, Y)),
  ccxLeft(Dynam, Left),
  eps(Eps, Left + XOffset*Unit, X),
  ccxTop(Dynam, Top),
  eps(Eps, Top + YOffset*Unit, Y),
  dynamPlaceCond(Place, Etiq, Dynam, StaffNs, Staffs, Unit, Eps),
  dynamHPlace(HPlace, HOffset, Dynam, TimestampAnchors, Advance, Unit, Eps).
dynamPlaceCond(between, dynamicPP, Dynam, [TopN, BottomN], Staffs, Unit, Eps) :-
  length(Staffs, MaxN),
  [TopN, BottomN]::integer(1, MaxN),
  { BottomN == TopN + 1 },
  % nth1 is non-det
  nth1(TopN, Staffs, TopStaff),
  nth1(BottomN, Staffs, [BottomStaffLine | _]),
  last(TopStaff, TopStaffLine),
  ccxOrigin(Dynam, point(OriginX, OriginY)),
  segYAtX(TopStaffLine, TopY, OriginX),
  segYAtX(BottomStaffLine, BottomY, OriginX),
  eps(Eps, (BottomY + TopY)/2, OriginY - Unit).
dynamHPlace(startid=Id, HOffset, Dynam, TimestampAnchors, Advance, Unit, Eps) :-
  member(_Timestamp-Id-Notehead, TimestampAnchors),
  ccxOrigin(Dynam, point(DynamOrigin, _)),
  ccxLeft(Notehead, NoteheadLeft),
  ccxRight(Notehead, NoteheadRight),
  ccxCenterX(Notehead, NoteheadCenter),
  { NoteheadLeft =< DynamCenter, DynamCenter =< NoteheadRight },
  eps(Eps, DynamOrigin + Advance*Unit/2 + HOffset*Unit, NoteheadCenter).

dynam(element(dynam, ['xml:id'=Id, ho=HOffset, place=Place, staff=StaffNs, HPlace], [Atom]), Id) -->
  add_id(Id),
  statep(dynamCond(Dynam, Place, StaffNs, HPlace, HOffset, Atom),
         [o(systemStaffLines), o(dynamSettings), o(timestampAnchors), o(unit), o(eps)]),
  termp(Dynam).

staff(element(staff, ['xml:id'=Id, n=NAtom], [Layer]),
      element(staffDef, ['xml:id'=DefId, n=NAtom, lines='5'], StaffDefChilds),
      StaffLines, Id) -->
  add_id(Id),
  add_id(DefId),
  state(+(timestamp, 1)),
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
  state([o(staffN, StaffN), +(StaffN-meterSig)]),
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

delay:mode(music:meterSigCond(ground, _, _, _)).
delay:mode(music:meterSigCond(_, ground, _, _)).
meterSigCond(Etiq, DigitAtom, N, MeterSig) :-
  atom_concat(timeSig, DigitAtom, Etiq),
  meterSigDown(N, DigitAtom, MeterSig).

meterSigDown(2, DigitAtom, Count-_) :-
  atom_number(DigitAtom, Count).
meterSigDown(4, DigitAtom, _-Unit) :-
  atom_number(DigitAtom, Unit),
  Power::integer(1, 4),
  { Unit == 2 ** Power }.

meterSigCond(N, Count, Ccx, Center, StaffLines, AllSettings, MeterSig, Unit, Eps) :-
  etiqsCond(Ccx, Etiq),
  freeze(Etiq, member(Etiq-[Width, Height, XOffset, YOffset], AllSettings)),
  delay(meterSigCond(Etiq, Count, N, MeterSig)),
  ccxOrigin(Ccx, point(X, Y)),
  nth1(N, StaffLines, Line),
  segYAtX(Line, SegY, X),
  eps(Eps, SegY, Y),
  ccxWidthHeightCond(Ccx, Width, Height, Unit, Eps),
  ccxLeft(Ccx, Left),
  eps(Eps, Left + XOffset*Unit, X),
  ccxTop(Ccx, Top),
  eps(Eps, Top + YOffset*Unit, Y),
  ccxRight(Ccx, CcxRight),
  eps(4*Eps, (Left + CcxRight) / 2, Center).

meterSig_(Count, Unit) -->
  state(o(staffN, StaffN)),
  statep(meterSigCond(2, Count, MeterSigUp, Center),
         [o(stafflines), o(timeSigSettings), o(StaffN-meterSig), o(unit), o(eps)]),
  termp(MeterSigUp),
  statep(meterSigCond(4, Unit, MeterSigDown, Center),
         [o(stafflines), o(timeSigSettings), o(StaffN-meterSig), o(unit), o(eps)]),
  termp(MeterSigDown).

layer(element(layer, ['xml:id'=Id, n='1'], Childs), Id) -->
  add_id(Id),
  sequence(find(music:layerChild), Childs).

layerChild(Child) -->
  ( scope(note(Child))
  ; scope(rest(Child))
  ).

delay:mode(music:restCond(ground, _)).
delay:mode(music:restCond(_, ground)).
restCond(rest8th, '8').

restCond(Rest, Dur, StaffLines,
         LeftAnchor, RightAnchor, RestSettings, LeftMargin, RightMargin,
         MeterSig, TstampIn, TstampOut, TstampIn-Rest, Unit, Eps) :-
  etiqsCond(Rest, Etiq),
  freeze(Etiq, memberchk(Etiq-[Width, Height, XOffset, YOffset], RestSettings)),
  delay(restCond(Etiq, Dur)),
  timestampCond(Dur, TstampIn, TstampOut, MeterSig),
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

timestampCond(Dur, TstampIn, TstampOut, _Count-Unit) :-
  { TstampOut == TstampIn + (1 / (Dur / Unit)) }.

rest(element(rest, ['xml:id'=Id, dur=Dur], []), Id) -->
  add_id(Id),
  state(o(staffN, StaffN)),
  statep(restCond(Rest, Dur),
         [o(stafflines), -(anchor), o(restSettings), o(restLeftMargin),
          o(restRightMargin), o(StaffN-meterSig), -(timestamp),
          [_]:timestampAnchors, o(unit), o(eps)]),
  termp(Rest).

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
  state(+(noteId, Id)),
  statep(marginCond(NoteContour),
         [-(anchor), o(noteLeftMargin), o(noteRightMargin), o(unit)]),
  contour(note(Dur, Oct, PName, NoteChilds), NoteContour).

note(Dur, Oct, PName, NoteChilds) -->
  contour(scope(music:notehead(Dur, Oct, PName)), NoteHeadContour),
  foldlg(call, [scope(stem), scope(accid(NoteHeadContour))], NoteChilds, []).

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
noteHeadCond(noteheadBlack, _).

noteHeadCond(DurAtom, Oct, PName, NoteHead, Dur, Pitch, BasePitch-BaseN, StaffLines,
             NoteHeadSettings, MeterSig, TstampIn, TstampOut, TstampIn-NoteId-NoteHead,
             NoteId, Unit, Eps) :-
  etiqsCond(NoteHead, Etiq),
  freeze(Etiq, memberchk(Etiq-[Width, Height], NoteHeadSettings)),
  delay(atom_number(DurAtom, Dur)),
  delay(noteHeadCond(Etiq, Dur)),
  timestampCond(Dur, TstampIn, TstampOut, MeterSig),
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
  state(o(staffN, StaffN)),
  statep(noteHeadCond(Dur, Oct, PName),
         [+(notehead, NoteHead), +(duration), +(pitch), o(pitchAnchor), o(stafflines),
          o(noteheadSettings), o(StaffN-meterSig), -(timestamp), [_]:timestampAnchors,
          o(noteId), o(unit), o(eps)]),
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

%TODO: can't detect the same ledgerlines multiple times as they are appended
%      as a list instead of as set
ledgerLines(Id) -->
  add_id(Id),
  statep(ledgerlinesCond(LedgerLines),
         [o(notehead), o(stafflines), o(pitch), o(pitchAnchor),
          o(ledgerlineExtension), o(ledgerlineThickness), o(unit), o(eps)]),
  sequence(selectp, LedgerLines),
  state(LedgerLines:ledgerlines).

delay:mode(music:vu(ground, _)).
delay:mode(music:vu(_, ground)).
vu(Atom, Number) :-
  delay(atom_number(AtomNumber, Number)),
  delay(atom_concat(AtomNumber, 'vu', Atom)).

stemCond(down, StemLengthAtom, Stem, NoteHead,
         AllSettings, StemWidth, Unit, Eps) :-
  segThickness(Stem, Thickness),
  eps(Eps, StemWidth * Unit, Thickness),
  etiqsCond(NoteHead, Etiq),
  freeze(Etiq, memberchk(Etiq-[Offset], AllSettings)),
  ccxOrigin(NoteHead, point(NoteHeadX, NoteHeadY)),
  segHV(v, left, top, Stem, StemRightTop),
  eps(p, Eps, point(NoteHeadX, NoteHeadY + Unit * Offset), StemRightTop),
  segEndY(Stem, StemBottom),
  delay(vu(StemLengthAtom, StemLength)),
  eps(Eps, StemLength * Unit, StemBottom - NoteHeadY).

noStemCond(noStem, noDirection, NoteHead) :-
  etiqsCond(NoteHead, noteheadWhole).
  
stem([element(stem, ['xml:id'=Id, len=Len, dir=Dir], []) | NoteChilds], NoteChilds, Id) -->
  add_id(Id),
  statep(stemCond(Dir, Len),
         [+(stem, Stem), o(notehead), o(stemSettings), o(stemWidth), o(unit), o(eps)]),
  termp(Stem),
  state(+(direction, Dir)),
  ( beam
  *-> []
  ; scope(flag(Dir))
  *-> []
  ; statep(noFlagCond, [o(duration)])
  ).
stem(NoteChilds, NoteChilds, _) -->
  statep(noStemCond, [+(stem), +(direction), o(notehead)]).

delay:mode(music:flagCond(ground, _)).
delay:mode(music:flagCond(_, ground)).
flagCond('flag8thDown', 8).

flagCond(down, Flag, Stem, Dur, Settings, Unit, Eps) :-
  segHV(v, left, bottom, Stem, StemLeftBottom),
  ccxLeftBottom(Flag, FlagLeftBottom),
  eps(p, Eps, StemLeftBottom, FlagLeftBottom),
  etiqsCond(Flag, Etiq),
  delay(flagCond(Etiq, Dur)),
  freeze(Etiq, memberchk(Etiq-[Width, Height], Settings)),
  ccxWidthHeightCond(Flag, Width, Height, Unit, Eps).
noFlagCond(2).
noFlagCond(4).

flag(Dir, Id) -->
  add_id(Id),
  statep(flagCond(Dir, Flag), [o(stem), o(duration), o(flagSettings), o(unit), o(eps)]),
  termp(Flag).

beamSpanCond(Id, BeamSpansIn, BeamSpansOut, NoteId, State) :-
  BeamSpanIn = element(
    beamSpan,
    ['xml:id'=Id, plist=PList, startid=StartNoteId, endid=EndNoteId],
    []),
  BeamSpanOut = element(
    beamSpan,
    ['xml:id'=Id, plist=PListTail, startid=StartNoteId, endid=EndNoteId],
    []),
  PList = [NoteId | PListTail],
  lists:selectchk(BeamSpanIn, BeamSpansIn, BeamSpanOut, BeamSpansOut),
  when((ground(State) ; (ground(StartNoteId), ground(EndNoteId))),
       beamNoteState(State, StartNoteId, EndNoteId, NoteId)).
beamNoteState(start, StartNote, EndNote, StartNote) :-
  dif(StartNote, EndNote).
beamNoteState(end, StartNote, EndNote, EndNote) :-
  dif(StartNote, EndNote).
beamNoteState(mid, StartNote, EndNote, NoteId) :-
  dif(NoteId, StartNote),
  dif(NoteId, EndNote).

beamRootCond(N, PreviousState-PreviousBeam, State-Beam, Dir, Stem, Duration,
             VerticalOffset, Unit, Eps) :-
  N::integer(1, 6),
  { Duration == 2**(2+N) },
  beamRootStateCond(PreviousState-PreviousBeam, State-Beam, Dir, Stem,
                    VerticalOffset, Unit, Eps).

beamRootStateCond(end-no, start-Beam, Dir, Stem, VerticalOffset, Unit, Eps) :-
  beamRootStartCond(Dir, Beam, Stem, VerticalOffset, Unit, Eps).
beamRootStateCond(start-Beam, mid-Beam, Dir, Stem, VerticalOffset, Unit, Eps) :-
  beamRootMidCond(Dir, Beam, Stem, VerticalOffset, Unit, Eps).
beamRootStateCond(mid-Beam, mid-Beam, Dir, Stem, VerticalOffset, Unit, Eps) :-
  beamRootMidCond(Dir, Beam, Stem, VerticalOffset, Unit, Eps).
beamRootStateCond(start-Beam, end-Beam, Dir, Stem, VerticalOffset, Unit, Eps) :-
  beamRootEndCond(Dir, Beam, Stem, VerticalOffset, Unit, Eps).
beamRootStateCond(mid-Beam, end-Beam, Dir, Stem, VerticalOffset, Unit, Eps) :-
  beamRootEndCond(Dir, Beam, Stem, VerticalOffset, Unit, Eps).

beamChildCond(I, N, ParentState-Parent, PreviousState-PreviousBeam, State-Beam,
              Dir, Stem, NoteHeadSettings, Unit, Eps) :-
  { N > I },
  beamChildStateCond(ParentState-Parent, PreviousState-PreviousBeam, State-Beam,
                     Dir, Stem, NoteHeadSettings, Unit, Eps).

beamChildStateCond(start-Parent, end-no, start-Beam, Dir, Stem, _NoteHeadSettings, Unit, Eps) :-
  beamChildStartStartCond(Dir, Beam, Stem, Parent, Unit, Eps).

beamChildStateCond(mid-Parent, _, start-Beam, Dir, Stem, NoteHeadSettings, Unit, Eps) :-
  beamChildFracLeftCond(Dir, Beam, Stem, Parent, NoteHeadSettings, Unit, Eps).
beamChildStateCond(mid-Parent, _, start-Beam, Dir, Stem, _NoteHeadSettings, Unit, Eps) :-
  beamChildMidStartCond(Dir, Beam, Stem, Parent, Unit, Eps).
beamChildStateCond(mid-Parent, start-Beam, mid-Beam, Dir, Stem, _NoteHeadSettings, Unit, Eps) :-
  beamChildMidMidCond(Dir, Beam, Stem, Parent, Unit, Eps).
beamChildStateCond(mid-Parent, mid-Beam, mid-Beam, Dir, Stem, _NoteHeadSettings, Unit, Eps) :-
  beamChildMidMidCond(Dir, Beam, Stem, Parent, Unit, Eps).
beamChildStateCond(mid-Parent, start-Beam, end-Beam, Dir, Stem, _NoteHeadSettings, Unit, Eps) :-
  beamChildMidEndCond(Dir, Beam, Stem, Parent, Unit, Eps).
beamChildStateCond(mid-Parent, mid-Beam, end-Beam, Dir, Stem, _NoteHeadSettings, Unit, Eps) :-
  beamChildMidEndCond(Dir, Beam, Stem, Parent, Unit, Eps).

beamChildStateCond(end-Parent, start-Beam, end-Beam, Dir, Stem, _NoteHeadSettings, Unit, Eps) :-
  beamChildEndEndCond(Dir, Beam, Stem, Parent, Unit, Eps).
beamChildStateCond(end-Parent, mid-Beam, end-Beam, Dir, Stem, _NoteHeadSettings, Unit, Eps) :-
  beamChildEndEndCond(Dir, Beam, Stem, Parent, Unit, Eps).
beamChildStateCond(end-Parent, _, start-Beam, Dir, Stem, NoteHeadSettings, Unit, Eps) :-
  beamChildFracLeftCond(Dir, Beam, Stem, Parent, NoteHeadSettings, Unit, Eps).

beamRootStartCond(down, Beam, Stem, VerticalOffset, Unit, Eps) :-
  segLeftBottom(h, Beam, point(BeamLeft, BeamBottom)),
  segHV(v, left, bottom, Stem, point(StemLeft, StemBottom)),
  eps(Eps, StemLeft, BeamLeft),
  eps(Eps, StemBottom + VerticalOffset*Unit, BeamBottom),
  segThickness(Beam, BeamThickness),
  eps(Eps, BeamThickness, Unit).
beamChildStartStartCond(down, Beam, Stem, Parent, Unit, Eps) :-
  segLeftBottom(h, Beam, point(BeamLeft, BeamBottom)),
  VCoeff::real(0, 1),
  segHDirCoeff(left, HCoeff),
  segHVCoeff(v, HCoeff, VCoeff, Stem, point(StemLeft, BeamBottom)),
  eps(Eps, StemLeft, BeamLeft),
  segLeftTop(h, Parent, point(_, ParentTop)),
  eps(Eps, BeamBottom + (Unit / 2), ParentTop),
  segThickness(Beam, BeamThickness),
  eps(Eps, BeamThickness, Unit).

beamRootMidCond(down, Beam, Stem, VerticalOffset, Unit, Eps) :-
  HCoeff::real(0, 1),
  segVDirCoeff(bottom, VCoeff),
  segHVCoeff(h, HCoeff, VCoeff, Beam, point(BeamX, BeamY)),
  segHV(v, mid, bottom, Stem, point(BeamX, StemBottom)),
  eps(Eps, StemBottom + VerticalOffset*Unit, BeamY),
  segRight(h, Beam, BeamRight),
  { BeamX + Unit =< BeamRight },
  segThickness(Beam, BeamThickness),
  eps(Eps, BeamThickness, Unit).
beamChildMidStartCond(down, Beam, Stem, Parent, Unit, Eps) :-
  segLeftBottom(h, Beam, point(BeamLeft, BeamBottom)),
  VCoeff::real(0, 1),
  segHDirCoeff(mid, HCoeff),
  segHVCoeff(v, HCoeff, VCoeff, Stem, point(StemLeft, BeamBottom)),
  eps(Eps, StemLeft, BeamLeft),
  segVDirCoeff(top, ParentVCoeff),
  BeamHCoeff::real(0, 1),
  segHVCoeff(h, BeamHCoeff, ParentVCoeff, Parent, point(StemLeft, ParentTop)),
  eps(Eps, BeamBottom + (Unit / 2), ParentTop),
  segThickness(Beam, BeamThickness),
  eps(Eps, BeamThickness, Unit).
beamChildMidMidCond(down, Beam, Stem, Parent, Unit, Eps) :-
  BeamHCoeff::real(0, 1),
  segVDirCoeff(bottom, BeamVCoeff),
  segHVCoeff(h, BeamHCoeff, BeamVCoeff, Beam, point(BeamX, BeamBottom)),
  VCoeff::real(0, 1),
  segHDirCoeff(mid, HCoeff),
  segHVCoeff(v, HCoeff, VCoeff, Stem, point(BeamX, BeamBottom)),
  segVDirCoeff(top, ParentVCoeff),
  segHVCoeff(h, BeamHCoeff, ParentVCoeff, Parent, point(_, ParentTop)),
  eps(Eps, BeamBottom + (Unit / 2), ParentTop),
  segThickness(Beam, BeamThickness),
  eps(Eps, BeamThickness, Unit).
beamChildMidEndCond(down, Beam, Stem, Parent, Unit, Eps) :-
  segRightBottom(h, Beam, point(BeamRight, BeamBottom)),
  VCoeff::real(0, 1),
  segHDirCoeff(mid, HCoeff),
  segHVCoeff(v, HCoeff, VCoeff, Stem, point(StemLeft, BeamBottom)),
  eps(Eps, StemLeft, BeamRight),
  segVDirCoeff(top, ParentVCoeff),
  BeamHCoeff::real(0, 1),
  segHVCoeff(h, BeamHCoeff, ParentVCoeff, Parent, point(StemLeft, ParentTop)),
  eps(Eps, BeamBottom + (Unit / 2), ParentTop),
  segThickness(Beam, BeamThickness),
  eps(Eps, BeamThickness, Unit).

beamRootEndCond(down, Beam, Stem, VerticalOffset, Unit, Eps) :-
  segRightBottom(h, Beam, point(BeamRight, BeamBottom)),
  segHV(v, right, bottom, Stem, point(StemRight, StemBottom)),
  eps(Eps, StemRight, BeamRight),
  eps(Eps, StemBottom + VerticalOffset*Unit, BeamBottom).
beamChildEndEndCond(down, Beam, Stem, Parent, Unit, Eps) :-
  segRightBottom(h, Beam, point(BeamRight, BeamBottom)),
  VCoeff::real(0, 1),
  segHDirCoeff(right, HCoeff),
  segHVCoeff(v, HCoeff, VCoeff, Stem, point(StemRight, BeamBottom)),
  eps(Eps, StemRight, BeamRight),
  segRightTop(h, Parent, point(_, ParentTop)),
  eps(Eps, BeamBottom + (Unit / 2), ParentTop).
beamChildFracLeftCond(down, Beam, Stem, Parent, NoteHeadSettings, Unit, Eps) :-
  memberchk(noteheadBlack-[Width, _], NoteHeadSettings),
  segLength(Beam, Length),
  eps(Eps, Width*Unit, Length),
  segRightBottom(h, Beam, point(BeamRight, BeamRightBottom)),
  VCoeff::real(0, 1),
  segHDirCoeff(right, HCoeff),
  segHVCoeff(v, HCoeff, VCoeff, Stem, point(StemRight, BeamRightBottom)),
  eps(Eps, StemRight, BeamRight),
  segRightTop(h, Parent, point(_, ParentRightTop)),
  eps(Eps, BeamRightBottom + (Unit / 2), ParentRightTop),
  segLeftBottom(h, Beam, point(BeamLeft, BeamLeftBottom)),
  segVDirCoeff(top, ParentVCoeff),
  segHVCoeff(h, _, ParentVCoeff, Parent, point(BeamLeft, ParentLeftTop)),
  eps(Eps, BeamLeftBottom + (Unit / 2), ParentLeftTop),
  segThickness(Beam, BeamThickness),
  eps(Eps, BeamThickness, Unit).

beam -->
  pop_scope(
    pop_scope(
      pop_scope(
        pop_scope(
          pop_contour(state:scope(music:beamSpan)))))).
beamSpan(Id) -->
  statep(beamSpanCond(Id), [-(beamSpans), o(noteId), +(beamState, State)]),
  add_id(Id),
  statep(beamRootCond(N),
         [-(beam-0, _, State-Beam), o(direction), o(stem), o(duration),
          o(beamVerticalOffset), o(unit), o(eps)]),
  termBeam(State, 0, Beam),
  beamSpan(1, N, State-Beam).
beamSpan(I, N, ParentState-Parent) -->
  statep(beamChildCond(I, N, ParentState-Parent),
         [-(beam-I, _, State-Beam), o(direction), o(stem), o(noteheadSettings), o(unit), o(eps)]),
  termBeam(State, I, Beam),
  { I1 is I + 1 },
  beamSpan(I1, N, ParentState-Beam).
beamSpan(N, N, _) -->
  [].

termBeam(start, _N, Beam) -->
  multi_seg(leftright, termp, Beam).
termBeam(mid, _N, _Neam) --> [].
termBeam(end, N, Beam) -->
  state(-(beam-N, end-Beam, end-no)).

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

accid(NoteHeadContour, [element(accid, ['xml:id'=Id, accid=Shape], []) | NoteChilds], NoteChilds, Id) -->
  add_id(Id),
  statep(accidCond(Accidental, Shape, NoteHeadContour),
         [o(notehead), o(accidentalSettings), o(accidentalLeftMargin),
          o(accidentalRightMargin), o(unit), o(eps)]),
  termp(Accidental).
accid(_, NoteChilds, NoteChilds, _) -->
  [].

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
  % etiqsCond(SystemLine, system),
  SystemLineLeftTop = point(LeftTop, _),
  SystemLineLeftBottom = point(LeftBottom, _),
  { Anchor == min(LeftTop, LeftBottom) }.

systemLine -->
  statep(systemLineCond(SystemLine),
         [+(anchor-grpSym), o(systemStaffLines), o(barLineThickness),
          o(unit), o(eps)]),
  termp(SystemLine).

gather_grpSym(element(scoreDef, _, Childs), StaffDefs, SystemStaffLines) -->
  gather_grpSym_(Childs, StaffDefs, SystemStaffLines, scoreDef-Childs).

gather_split([El1 | L1], [El2 | L2],
             [El1 | Grouped1], [El2 | Grouped2],
             Rest1, Rest2) :-
  gather_split(L1, L2, Grouped1, Grouped2, Rest1, Rest2).
gather_split(Rest1, Rest2, [], [], Rest1, Rest2).

gather_grpSym_([], [], [], _) --> [].
gather_grpSym_([element(staffGrp, ['xml:id'=Id | _], [GrpSym | Childs]) | Rest],
               StaffDefs,
               SystemStaffLines, Parent-ParentChilds) -->
  add_id(Id),
  state(o(systemLine, true)),
  scope(grpSym(GrpSym,
               StaffDefs, SystemStaffLines,
               GroupStaffDefs, GroupStaffLines,
               RestStaffDefs, RestStaffLines)),
  gather_grpSym_(Childs, GroupStaffDefs, GroupStaffLines, staffGrp-[GrpSym | Childs]),
  gather_grpSym_(Rest, RestStaffDefs, RestStaffLines, Parent-ParentChilds).
gather_grpSym_([StaffDef | Childs],
               [StaffDef | StaffDefs],
               [_ | SystemStaffLines], Parent-ParentChilds) -->
  { StaffDef = element(staffDef, _, _) },
  gather_grpSym_(Childs, StaffDefs, SystemStaffLines, Parent-ParentChilds).
gather_grpSym_([StaffGrp | Rest],
              StaffDefs,
              SystemStaffLines, Parent-ParentChilds) -->
  { StaffGrp = element(staffGrp, ['xml:id'=Id | _], Childs) },
  add_id(Id),
  { gather_split(StaffDefs, SystemStaffLines,
                 GroupStaffDefs, GroupStaffLines,
                 RestChilds, RestStaffLines) },
  { SystemStaffLines \== [] },
  gather_grpSym_(Childs, GroupStaffDefs, GroupStaffLines, staffGrp-Childs),
  gather_grpSym_(Rest, RestChilds, RestStaffLines, Parent-ParentChilds),
  ( {Parent == staffGrp, ParentChilds == [StaffGrp]}
  -> !, {fail}
  ; []
  ).

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

print_struct -->
  epf:pop_struct(Struct),
  debug(print_struct, "~p~n", [Struct]),
  epf:push_struct(Struct).
