:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(ninja)).

verovio -->
  "/home/kwon-young/prog/verovio/build/verovio -r /home/kwon-young/prog/verovio/data --scale 200 ".
swipl -->
  "LD_LIBRARY_PATH=/home/kwon-young/mambaforge/envs/dev/lib/ timeout 10 /home/kwon-young/prog/swipl-devel/install/bin/swipl -q -t halt ".

stem_page(_, '').

stem("stafflines").
stem("double-stafflines").
stem("system-barline").
stem("brace").
stem("bracket").
stem("brace-bracket").
stem("staff").
stem("clef-G-2").
stem("clef-F-4").
stem("clef-staff").
stem("meter-3-8").
stem("dynamicpp").
stem("accid-sharp").
stem("rest-8th").
stem("rest-whole").
stem("note-whole").
stem("note-quarter").
stem("note-8th").
stem("beam-2-8th").
stem("beam-2-16th").
stem(Stem) :-
  length(Durs, 3),
  maplist([Dur]>>member(Dur, ['8', '16', '32']), Durs),
  format(string(Stem), "beam-2-~sth-~sth-~sth", Durs).


dir -->
   "data".

suffix(Suffix) -->
   "-", Suffix.

ext(Ext) -->
   ".", Ext.

filename(Stem, Suffix, Ext) -->
   dir, "/", Stem, suffix(Suffix), ext(Ext).
filename(Stem, Suffix, Page, Ext) -->
   dir, "/", Stem, suffix(Suffix),
   (  { number(Page) }
   -> { format(string(PageStr), '~|~`0t~d~3+', [Page]) },
      "_", PageStr
    ; ""
    ),
    ext(Ext).

mei(Stem, Suffix) -->
   filename(Stem, Suffix, "mei").

input(Stem) -->
  mei(Stem, "in").

mei_id(Stem) -->
  mei(Stem, "ids").

svgs(Stem) -->
  foreach(stem_page(Stem, Page),
          svg_page(Stem, Page), " ").

svgs_ids(Stem) -->
  foreach(stem_page(Stem, Page),
          svg_page_ids(Stem, Page), " ").

svg(Stem) -->
  filename(Stem, "verovio", "svg").

svg_page(Stem, Page) -->
  filename(Stem, "verovio", Page, "svg").
svg_page_ids(Stem, Page) -->
  filename(Stem, "verovio_ids", Page, "svg").

pl(Stem) -->
  pl(Stem, "verovio").
pl_noscope(Stem) -->
  pl(Stem, "verovio-noscope").
pl(Stem, Suffix) -->
  filename(Stem, Suffix, "pl").

setting(Stem, Target) -->
  "settings/", Stem, suffix(Target), ext("txt").

deps -->
  deps("music.pl").

graph -->
  rule(generate, (swipl, "-s $in -g main"), [generator-"1"]),
  rule(mei_add_ids, (verovio, "--xml-id-checksum -t mei -o $out $in")),
  rule(mei2svg, (verovio, "--use-brace-glyph -a --footer none -o $output $in")),
  rule(svg_add_ids, "inkscape -o $out $in"),
  rule(svg2pl, "python $in --output $out --type prolog"),
  rule(remove_scopes, (swipl, "-s music.pl -g main -- remove_scopes $in $out")),
  rule(test, (swipl, "-s music.pl -g main -- mainTest $in $out")),
  rule(reco, (swipl, "-s music.pl -g main -- mainReco $in $out")),
  rule(gen, (swipl, "-s music.pl -g main -- mainGen $in $out")),

  build(["build.ninja"], generate, ["ninja.pl"], [implicit_ins([deps("ninja.pl")])]),
  foreach(stem(Stem), graph(Stem)),
  build(["svgs"], phony, [foreach(stem(Stem), svgs(Stem), " ")]),
  build(["all"], phony, [
    foreach(stem(Stem), pl(Stem, "music"), " "),
    foreach(stem(Stem), mei(Stem, "music"), " "),
    foreach(stem(Stem), setting(Stem, "reco"), " ")
  ]),
  build(["test"], phony, [
    foreach(stem(Stem), setting(Stem, "test"), " ")
  ]),
  build(["dataset/IMSLP318757-partial-verovio.svg"], mei2svg,
        ["dataset/IMSLP318757-partial.mei"], [variables([output-"dataset/IMSLP318757-partial-verovio.svg"])]),
  build(["dataset/IMSLP318757-partial-ids.mei"], mei_add_ids, ["dataset/IMSLP318757-partial.mei"]),
  build(["dataset/IMSLP318757.pl"], svg2pl,
        ["svg2pl.py", "data/glyphnames.json", "dataset/IMSLP318757_001.svg"]),
  build(["dataset/IMSLP318757-music.mei", "settings/IMSLP318757-reco.txt"], reco,
        ["dataset/IMSLP318757.pl", "settings/default.txt"], [implicit_ins([deps])]).
  % foreach(gen_stem(Stem),
  %         build([svgs(Stem)], mei2svg, [mei(Stem, "gen")],
  %               [variables([output-svg(Stem)])])).

graph(Stem) -->
  build([mei_id(Stem)], mei_add_ids, [input(Stem)]),
  build([svgs(Stem)], mei2svg, [mei_id(Stem)], [variables([output-svg(Stem)])]),
  foreach(stem_page(Stem, Page), build([svg_page_ids(Stem, Page)], svg_add_ids, [svg_page(Stem, Page)])),
  build([pl(Stem)], svg2pl, ["svg2pl.py", "data/glyphnames.json", svgs_ids(Stem)]),
  build([pl_noscope(Stem)], remove_scopes, [pl(Stem)],
        [implicit_ins([deps("geo.pl")])]),
  build([setting(Stem, "test")], test, [mei_id(Stem), pl(Stem)],
        [implicit_ins([deps])]),
  build([mei(Stem, "music"), setting(Stem, "reco")], reco,
        [pl_noscope(Stem), setting(Stem, "test")], [implicit_ins([deps])]),
  build([pl(Stem, "music")], gen, [mei_id(Stem)], [implicit_ins([deps])]),
  build([Stem], phony, [pl(Stem, "music"), mei(Stem, "music")]).

main :-
  phrase(graph, L),
  open('build.ninja', write, Stream),
  string_codes(S, L),
  write(Stream, S),
  close(Stream).
