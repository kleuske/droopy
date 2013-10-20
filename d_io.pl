/* ------------------------------------------------------------------------- *
 * FILE:  d_io.pl                                                            *
 * BRIEF: handles actual IO                                                  *
 *                                                                           *
 * ------------------------------------------------------------------------- *
 *  This file is part of Droopy.                                             *
 *                                                                           *
 *  Foobar is free software: you can redistribute it and/or modify           *
 *  it under the terms of the GNU General Public License as published by     *
 *  the Free Software Foundation, either version 3 of the License, or        *
 *  (at your option) any later version.                                      *
 *                                                                           *
 *  Foobar is distributed in the hope that it will be useful,                *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with Foobar.  If not, see <http://www.gnu.org/licenses/>.          *
 * ------------------------------------------------------------------------- */

:-module(d_io, [get_xml/2,
                get_raw/2]).

/* ------------------------------------------------------------------------- */

:- consult(library(http/http_client)).
:- consult(library(http/http_open)).
:- consult(library(http/http_header)).

/* ------------------------------------------------------------------------- *
 * get_xml(+Url, -Xml)                                                       *
 *                                                                           *
 * Fetches XML. URL is expected to be normalized                             *
 * ------------------------------------------------------------------------- */

get_xml(Url, Xml) :-
  http_open(Url, Stream, []),
  !,
  load_structure(stream(Stream), Xml, [dialect(xml), space(remove)]),
  close(Stream).
get_xml(Url, []) :-
  writef('get_xml : Failed to open url %w', [Url]),
  !, fail.

/* ------------------------------------------------------------------------- *
 * get_raw(+Url, -Xml)                                                       *
 *                                                                           *
 * Fetches Raw Wiki content. URL is expected to be normalized                *
 * ------------------------------------------------------------------------- */

get_raw(Url, Xml) :-
  http_open(Url, Stream, []),
  !,
  read_stream_to_codes(Stream, Xml, [dialect(xml), space(remove)]),
  close(Stream).
get_raw(Url, []) :-
  writef('get_xml : Failed to open url %w', [Url]),
  !, fail.

/* ------------------------------------------------------------------------- *
 * END OF FILE                                                               *
 * ------------------------------------------------------------------------- */


