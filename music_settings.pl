:- module(music_settings, [get_settings/3, update_settings/1]).

:- use_module(library(clpBNR)).
:- use_module(library(settings)).

:- setting(pageWidth, pair, real(0, inf)-2100, 'page width').
:- setting(pageHeight, pair, real(0, inf)-2970, 'page height').
:- setting(topMargin, pair, real(0, inf)-50, 'page top margin').
:- setting(leftMargin, pair, real(0, inf)-50, 'page left margin').
:- setting(bottomMargin, pair, real(0, inf)-50, 'page bottom margin').
:- setting(rightMargin, pair, real(0, inf)-50, 'page right margin').
:- setting(eps, pair, real(0, 10)-0, 'Global Eps in pixels').
:- setting(unit, pair, real(0, 1000)-9, 'The MEI unit (1⁄2 of the distance between the staff lines)').
:- setting(thickness, pair, real(0, 1)-0.15, 'line thickness in unit').
:- setting(barLineThickness, pair, real(0, 1)-0.30, 'barline thickness in unit').
:- setting(measureMinWidth, pair, real(0, 100)-15, 'The minimal measure width in MEI units').
:- setting(spacingStaff, pair, real(0, 48)-12, 'The staff minimal spacing in MEI units').
:- setting(spacingSystem, pair, real(0, 48)-24, 'The system minimal spacing in MEI units').
:- setting(clefLeftMargin, pair, real(0, 2)-1, 'The left margin for clefs in MEI units').
:- setting(clefRightMargin, pair, real(0, 2)-1, 'The right margin for clefs in MEI units').
:- setting(gClefWidth, pair, real(3, 9)-5.16, 'The G clef width in MEI units').
:- setting(gClefHeight, pair, real(10, 20)-13.9, 'The G clef height in MEI units').
:- setting(gClefXOffset, pair, real(0, 1)-0, 'The G clef origin horizontal offset from the top in MEI units').
:- setting(gClefYOffset, pair, real(4, 16)-8.7, 'The G clef origin vertical offset from the top in MEI units').
:- setting(fClefWidth, pair, real(3, 9)-5.5, 'The F clef width in MEI units').
:- setting(fClefHeight, pair, real(3, 9)-6.6, 'The F clef height in MEI units').
:- setting(fClefXOffset, pair, real(-1, 1)-0, 'The F clef origin horizontal offset from the top in MEI units').
:- setting(fClefYOffset, pair, real(0, 4)-2, 'The F clef origin vertical offset from the top in MEI units').
:- setting(braceMargin, pair, real(0, 4)-1, 'The brace group symbol margin in MEI units').
:- setting(bracketMargin, pair, real(0, 4)-1, 'The bracket group symbol margin in MEI units').
:- setting(braceWidth, pair, real(0, 4)-2.0, 'The brace width in MEI units').
:- setting(braceVerticalMargin, pair, real(0, 4)-0.0, 'The brace top/bottom margin from the top/bottom staffline of the group in MEI units').
:- setting(bracketThickness, pair, real(0.5, 2)-1.0, 'The bracket thickness in MEI units').
:- setting(bracketVerticalOffset, pair, real(0, 4)-0.63, 'The bracket top/bottom offset from the top/bottom staffline of the group in MEI units').
:- setting(bracketOverlap, pair, real(0, 4)-0.6, 'The overlap between bracket top/bottom symobol and the bracket seg in MEI units').
:- setting(timeSigLeftMargin, pair, real(0, 4)-1, 'The left margin for time signature in MEI units').
:- setting(timeSigRightMargin, pair, real(0, 4)-1, 'The right margin for time signature in MEI units').
:- setting(timeSig3Width, pair, real(1, 9)-5.5, 'The time signature 3 width in MEI units').
:- setting(timeSig3Height, pair, real(1, 9)-6.6, 'The time signature 3 height in MEI units').
:- setting(timeSig3XOffset, pair, real(-1, 1)-0, 'The time signature 3 origin horizontal offset from the top in MEI units').
:- setting(timeSig3YOffset, pair, real(0, 4)-2, 'The time signature 3 origin vertical offset from the top in MEI units').
:- setting(timeSig4Width, pair, real(1, 9)-5.5, 'The time signature 4 width in MEI units').
:- setting(timeSig4Height, pair, real(1, 9)-6.6, 'The time signature 4 height in MEI units').
:- setting(timeSig4XOffset, pair, real(-1, 1)-0, 'The time signature 4 origin horizontal offset from the top in MEI units').
:- setting(timeSig4YOffset, pair, real(0, 4)-2, 'The time signature 4 origin vertical offset from the top in MEI units').
:- setting(timeSig8Width, pair, real(1, 9)-5.5, 'The time signature 8 width in MEI units').
:- setting(timeSig8Height, pair, real(1, 9)-6.6, 'The time signature 8 height in MEI units').
:- setting(timeSig8XOffset, pair, real(-1, 1)-0, 'The time signature 8 origin horizontal offset from the top in MEI units').
:- setting(timeSig8YOffset, pair, real(0, 4)-2, 'The time signature 8 origin vertical offset from the top in MEI units').
:- setting(noteLeftMargin, pair, real(0, 2)-1, 'The left margin for note in MEI units').
:- setting(noteRightMargin, pair, real(0, 2)-1, 'The right margin for note in MEI units').
:- setting(noteheadWholeWidth, pair, real(1, 9)-2.5, 'The whole notehead width in MEI units').
:- setting(noteheadWholeHeight, pair, real(1, 9)-2.12, 'The whole notehead height in MEI units').
:- setting(noteheadBlackWidth, pair, real(1, 9)-2.5, 'The black notehead width in MEI units').
:- setting(noteheadBlackHeight, pair, real(1, 9)-2.12, 'The black notehead height in MEI units').
:- setting(stemWidth, pair, real(0, 1)-0.20, 'The stem width in MEI units').
:- setting(noteheadBlackStemOffset, pair, real(0, 1)-0.30, 'The black notehead stem offset in MEI units').
:- setting(accidentalLeftMargin, pair, real(0, 2)-1, 'The left margin for accidental in MEI units').
:- setting(accidentalRightMargin, pair, real(0, 2)-1, 'The right margin for accidental in MEI units').
:- setting(accidentalSharpWidth, pair, real(1, 9)-5.5, 'The sharp accidental width in MEI units').
:- setting(accidentalSharpHeight, pair, real(1, 9)-6.6, 'The sharp accidental height in MEI units').
:- setting(accidentalSharpXOffset, pair, real(-1, 1)-0, 'The sharp accidental origin horizontal offset from the top in MEI units').
:- setting(accidentalSharpYOffset, pair, real(0, 4)-2, 'The sharp accidental origin vertical offset from the top in MEI units').
:- setting(restLeftMargin, pair, real(0, 2)-1, 'The left margin for rest in MEI units').
:- setting(restRightMargin, pair, real(0, 2)-1, 'The right margin for rest in MEI units').
:- setting(rest8thWidth, pair, real(1, 9)-5.5, 'The 8th rest width in MEI units').
:- setting(rest8thHeight, pair, real(1, 9)-6.6, 'The 8th rest height in MEI units').
:- setting(rest8thXOffset, pair, real(-1, 1)-0, 'The 8th rest origin horizontal offset from the top in MEI units').
:- setting(rest8thYOffset, pair, real(0, 4)-2, 'The 8th rest origin vertical offset from the top in MEI units').
:- setting(ledgerlineThickness, pair, real(0, 1)-0.25, 'The ledger line thickness in MEI units').
:- setting(ledgerlineExtension, pair, real(0, 1)-0.54, 'The amount by which a ledger line should extend either side of a notehead in MEI unit').
:- setting(flag8thDownWidth, pair, real(1, 9)-5.5, 'The 8th flag down width in MEI units').
:- setting(flag8thDownHeight, pair, real(1, 9)-6.6, 'The 8th flag down height in MEI units').
:- setting(beamVerticalOffset, pair, real(0, 1)-0, 'The vertical offset between stem and beam outer border in MEI units').

get_settings(Type, Settings, GroupedSettings) :-
  findall(Setting, get_setting(Type, Setting), Settings),
  group_settings(Settings, GroupedSettings).

get_setting(domain, Name-Value) :-
  setting(_Mod:Name, Range-_),
  ( Value::Range
  -> true
  ; Value = Range
  ).
get_setting(value, Name-Value) :-
  setting(_Mod:Name, _-Value).

group_settings(Settings, [clefSettings-ClefSettings, timeSigSettings-TimeSigSettings,
                          noteheadSettings-NoteHeadSettings, restSettings-RestSettings,
                          accidentalSettings-AccidentalSettings,
                          stemSettings-StemSettings, flagSettings-FlagSettings | Settings]) :-
  group_settings_by_prefix(Settings, ccx, [gClef, fClef], ClefSettings),
  group_settings_by_prefix(Settings, ccx, [timeSig3, timeSig4, timeSig8], TimeSigSettings),
  group_settings_by_prefix(Settings, ccx, [noteheadWhole, noteheadBlack], NoteHeadSettings),
  group_settings_by_prefix(Settings, ccx, [rest8th], RestSettings),
  group_settings_by_prefix(Settings, ccx, [accidentalSharp], AccidentalSettings),
  group_settings_by_prefix(Settings, stem, [noteheadBlack], StemSettings),
  group_settings_by_prefix(Settings, ccx, [flag8thDown], FlagSettings).

group_settings_by_prefix(Settings, Type, Keys, GroupedSettings) :-
  maplist(group_setting(Type, Settings), Keys, GroupedSettings).

memberchk_(List, Key, Value) :-
  memberchk(Key-Value, List).

group_setting(ccx, Settings, Prefix, Prefix-GroupedSettings) :-
  Suffixes = ['LeftMargin', 'RightMargin', 'Width', 'Height', 'XOffset', 'YOffset'],
  maplist(atom_concat(Prefix), Suffixes, Names),
  convlist(memberchk_(Settings), Names, GroupedSettings).
group_setting(stem, Settings, Prefix, Prefix-GroupedSettings) :-
  Suffixes = ['StemOffset'],
  maplist(atom_concat(Prefix), Suffixes, Names),
  convlist(memberchk_(Settings), Names, GroupedSettings).

update_settings(Settings) :-
  memberchk(eps-Eps, Settings),
  global_minimize(Eps, Eps, 3),
  memberchk(unit-Unit, Settings),
  splitsolve([Unit], 3),
  midpoint(Unit, Unit),
  maplist(update_setting, Settings).
update_setting(Name-Value) :-
  setting(Mod:Name, _Domain-Default),
  ( interval(Value)
  ->  domain(Value, NewDomain)
  ; NewDomain = Value
  ),
  set_setting(Mod:Name, NewDomain-Default).
