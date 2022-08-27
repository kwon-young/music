VEROVIO = /home/kwon-young/prog/verovio/build/verovio -r /home/kwon-young/prog/verovio/data --xml-id-checksum

INPUTS = $(wildcard data/*-in.musicxml)
SVGS = $(INPUTS:in.musicxml=verovio.svg)
PLS = $(SVGS:.svg=.pl)
RECO = $(PLS:.pl=.musicxml)
GEN = $(INPUTS:in.musicxml=music.pl)
DIFFXML = $(INPUTS:-in.musicxml=.diffXml)

all: $(DIFFXML) $(GEN)

$(SVGS): data/%-verovio.svg: data/%-in.musicxml
	$(VEROVIO) -o $@ $<

$(PLS): data/%-verovio.pl: svg2pl.py data/%-verovio.svg data/glyphnames.json
	python $^ $@

SRC = ccx.pl \
      cond.pl \
      epf_geo.pl \
      epf.pl \
      geo.pl \
      music.pl \
      music_utils.pl \
      pitch_cond.pl \
      seg.pl \
      utils.pl \
      note.pl

$(RECO): data/%-verovio.musicxml: data/%-verovio.pl settings/musicReco.txt $(SRC)
	swipl -t halt -s music.pl -g "mainReco('$<', 'settings/musicReco.txt', '$@')."

$(GEN): data/%-music.pl: data/%-in.musicxml settings/musicGen.txt $(SRC)
	swipl -t halt -s music.pl -g "mainGen('$<', 'settings/musicGen.txt', '$@')."

$(DIFFXML): data/%.diffXml: data/%-in.musicxml data/%-verovio.musicxml diffxml.pl
	swipl -t halt -s diffxml.pl -g "diffXml('$<', 'data/$*-verovio.musicxml')." && touch $@

.PHONY: clean

clean:
	rm -rf $(SVGS) $(PLS) $(RECO) $(GEN) $(DIFFXML)
