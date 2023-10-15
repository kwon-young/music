:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(ninja)).

verovio -->
  "/home/kwon-young/prog/verovio/build/verovio -r /home/kwon-young/prog/verovio/data ".
swipl -->
  "swipl -q -t halt ".

stem_page(_, 1).

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
stem("quarter").
stem("accid-sharp").
stem("rest-8th").

dir -->
   "data".

suffix(Suffix) -->
   "-", Suffix.

ext(Ext) -->
   ".", Ext.

filename(Stem, Suffix, Ext) -->
   dir, "/", Stem, suffix(Suffix), ext(Ext).
filename(Stem, Suffix, Page, Ext) -->
   { format(string(PageStr), '~|~`0t~d~3+', [Page]) },
   dir, "/", Stem, suffix(Suffix), "_", PageStr, ext(Ext).

mei(Stem, Suffix) -->
   filename(Stem, Suffix, "mei").

input(Stem) -->
  mei(Stem, "in").

id(Stem) -->
  mei(Stem, "ids").

svgs(Stem) -->
  foreach(stem_page(Stem, Page),
          filename(Stem, "verovio", Page, "svg"), " ").
svg(Stem) -->
  filename(Stem, "verovio", "svg").

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
  rule(add_ids, (verovio, "--xml-id-checksum -t mei -o $out $in")),
  rule(mei2svg, (verovio, "--use-brace-glyph -a --footer none -o $output $in")),
  rule(svg2pl, "python $in $out"),
  rule(remove_scopes, (swipl, "-s music.pl -g main -- remove_scopes $in $out")),
  rule(test, (swipl, "-s music.pl -g main -- mainTest $in $out")),
  rule(reco, (swipl, "-s music.pl -g main -- mainReco $in $out")),
  rule(gen, (swipl, "-s music.pl -g main -- mainGen $in $out")),

  build(["build.ninja"], generate, ["ninja.pl"], [implicit_ins([deps("ninja.pl")])]),
  foreach(stem(Stem), graph(Stem)),
  build(["all"], phony, [
    foreach(stem(Stem), pl(Stem, "music"), " "),
    foreach(stem(Stem), mei(Stem, "music"), " "),
    foreach(stem(Stem), setting(Stem, "reco"), " ")
  ]),
  build(["test"], phony, [
    foreach(stem(Stem), setting(Stem, "test"), " ")
  ]),
  build(["dataset/IMSLP318757.pl"], svg2pl,
        ["svg2pl.py", "dataset/IMSLP318757_001.svg", "data/glyphnames.json"]),
        build(["dataset/IMSLP318757-music.mei", "settings/IMSLP318757-reco.txt"], reco,
        ["dataset/IMSLP318757.pl", "settings/default.txt"], [implicit_ins([deps])]).

graph(Stem) -->
  build([id(Stem)], add_ids, [input(Stem)]),
  build([svgs(Stem)], mei2svg, [id(Stem)], [variables([output-svg(Stem)])]),
  build([pl(Stem)], svg2pl, ["svg2pl.py", svgs(Stem), "data/glyphnames.json"]),
  build([pl_noscope(Stem)], remove_scopes, [pl(Stem)],
        [implicit_ins([deps("geo.pl")])]),
  build([setting(Stem, "test")], test, [id(Stem), pl(Stem)],
        [implicit_ins([deps])]),
  build([mei(Stem, "music"), setting(Stem, "reco")], reco,
        [pl_noscope(Stem), setting(Stem, "test")], [implicit_ins([deps])]),
  build([pl(Stem, "music")], gen, [id(Stem)], [implicit_ins([deps])]),
  build([Stem], phony, [pl(Stem, "music"), mei(Stem, "music")]).

main :-
  phrase(graph, L),
  open('build.ninja', write, Stream),
  string_codes(S, L),
  write(Stream, S),
  close(Stream).
