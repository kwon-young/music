:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

identity(X) -->
  X.

indent([]) --> [].
indent([H | T]) -->
  sequence("  ", identity, "", "", [H | T]).

ws(List) -->
  sequence(identity, " ", List).
ws(Start, List) -->
  ws_(List, Start).
ws_([], _) -->
  [].
ws_([H | T], Start) -->
  sequence(Start, identity, " ", "", [H | T]).

variable(Name-Value) -->
  variable(Name, Value).
variable(Name, Value) -->
  ws([Name, "=", Value]), "\n".

rule(Name, Command) -->
  rule(Name, Command, []).
rule(Name, Command, Variables) -->
  "rule ", Name, "\n",
  indent([variable("command", Command) | Variables]).

build([H | T], Rule, Ins) -->
  build([H | T], Rule, Ins, [], []).
build([H | T], Rule, Ins, ImplicitIns) -->
  build([H | T], Rule, Ins, ImplicitIns, []).
build([H | T], Rule, Ins, ImplicitIns, Variables) -->
  "build ", ws([H | T]), ": ", Rule, " ", ws(Ins), " ",
  ws("| ", ImplicitIns), "\n",
  indent(Variables).

verovio -->
  "/home/kwon-young/prog/verovio/build/verovio -r /home/kwon-young/prog/verovio/data ".
swipl -->
  "swipl -q -t halt ".

stem_page("stafflines", 1).
stem_page("staff", 1).

stem("stafflines").
stem("staff").

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
deps(Source) -->
  Source, " ",
  { xref_source(Source) },
  foreach(xref_uses_file(Source, _, Path), atom(Path), " ").

graph -->
  rule("generate", (swipl, "-s $in -g main"), [variable("generator", "1")]),
  rule("add_ids", (verovio, "--xml-id-checksum -t mei -o $out $in")),
  rule("mei2svg", (verovio, "-a --footer none -o $output $in")),
  rule("svg2pl", "python $in $out"),
  rule("remove_scopes", (swipl, "-s music.pl -g main -- remove_scopes $in $out")),
  rule("test", (swipl, "-s music.pl -g main -- mainTest $in $out")),
  rule("reco", (swipl, "-s music.pl -g main -- mainReco $in $out")),
  rule("gen", (swipl, "-s music.pl -g main -- mainGen $in $out")),

  build(["build.ninja"], "generate", ["ninja.pl"]),
  foreach(stem(Stem), graph(Stem)),
  build(["all"], "phony", [
    foreach(stem(Stem), pl(Stem, "music"), " "),
    foreach(stem(Stem), mei(Stem, "music"), " "),
    foreach(stem(Stem), setting(Stem, "reco"), " ")
  ]).

graph(Stem) -->
  build([id(Stem)], "add_ids", [input(Stem)]),
  build([svgs(Stem)], "mei2svg", [id(Stem)], [], [variable("output", svg(Stem))]),
  build([pl(Stem)], "svg2pl", ["svg2pl.py", svgs(Stem), "data/glyphnames.json"]),
  build([pl_noscope(Stem)], "remove_scopes", [pl(Stem)], [deps("geo.pl")]),
  build([setting(Stem, "test")], "test", [id(Stem), pl(Stem)], [deps]),
  build([mei(Stem, "music"), setting(Stem, "reco")], "reco",
        [pl_noscope(Stem), setting(Stem, "test")], [deps]),
  build([pl(Stem, "music")], "gen", [id(Stem)], [deps]).

main :-
  phrase(graph, L),
  open('build.ninja', write, Stream),
  string_codes(S, L),
  write(Stream, S),
  close(Stream).
