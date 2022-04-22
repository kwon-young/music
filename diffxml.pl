diffXml(File1, File2) :-
  load_xml(File1, Xml, [space(remove)]),
  load_xml(File2, Xml, [space(remove)]).
