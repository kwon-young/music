main :-
  mei(Term, Durs),
  format(atom(Filename), "data/beam-2-~sth-~sth-~sth-in.mei", Durs),
  open(Filename, write, S),
  xml_write(S, Term, []),
  close(S),
  fail.

mei(Term, [Dur1, Dur2, Dur3]) :-
  Term = [
    pi('xml-model href="https://music-encoding.org/schema/5.0/mei-all.rng" type="application/xml" schematypens="http://relaxng.org/ns/structure/1.0"'),
    pi('xml-model href="https://music-encoding.org/schema/5.0/mei-all.rng" type="application/xml" schematypens="http://purl.oclc.org/dsdl/schematron"'),
    element(mei, [ xmlns='http://www.music-encoding.org/ns/mei', meiversion='5.0' ],
	    [ element(meiHead, [], [ element(fileDesc, [], [ element(titleStmt, [], [element(title,[],[])]) ]) ]),
	      element(music, [],
		      [ element(body, [],
			      	[ element(mdiv, [],
					  [ element(score, [],
						    [ element(scoreDef, [],
							      [ element(staffGrp, [],
								      	[ element(staffDef, [ n = '1', lines = '5' ],
										  [ element(clef, [ shape = 'G', line = '2' ], []),
										    element(keySig, [ sig = '0' ], []),
										    element(meterSig, [ count = '3', unit = '8' ], [])
										  ])
								      	])
							      ]),
						      element(section, [],
							      [ element(measure, [ n = '1' ],
								      	[ element(staff, [ n = '1' ],
										  [ element(layer, [ n = '1' ], [ Note1, Note2, Note3 ]) ]),
									  element(beamSpan, [ startid = note1, endid = note3, plist = 'note1 note2 note3' ], [])
								      	])
							      ])
						    ])
					  ])
			      	])
		      ])
	    ])
  ],
  note(note1, Note1, Dur1, Len),
  note(note2, Note2, Dur2, Len),
  note(note3, Note3, Dur3, Len),
  maplist(atom_number, [Dur1, Dur2, Dur3], Durs),
  max_list(Durs, MaxDur),
  ( MaxDur = 32
  ->  Len = '8.8vu'
  ; Len = '6.8vu'
  ).

note(Id, Note, Dur, Len) :-
  member(Dur, ['8', '16', '32']),
  Note = element(note, [ 'xml:id' = Id, dur = Dur, oct = '5', pname = e ], [
    element(stem, [ len = Len, dir='down' ], [])]).
