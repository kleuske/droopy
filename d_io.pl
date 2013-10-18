:-module(d_io, [get_xml/2,
                get_raw/2]).

:- consult(library(http/http_client)).
:- consult(library(http/http_open)).
:- consult(library(http/http_header)).

get_xml(Url, Xml) :-
  /*uri_normalized(Url, Norm),*/
  http_open(Url, Stream, []),
  !,
  load_structure(stream(Stream), Xml, [dialect(xml), space(remove)]),
  close(Stream).
get_xml(Url, []) :-
  writef('get_xml : Failed to open url %w', [Url]),
  !, fail.

get_raw(Url, Xml) :-
  uri_normalized(Url, Norm),
  http_open(Norm, Stream, []),
  !,
  read_stream_to_codes(Stream, Xml, [dialect(xml), space(remove)]),
  close(Stream).
get_raw(Url, []) :-
  writef('get_xml : Failed to open url %w', [Url]),
  !, fail.

