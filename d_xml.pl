/* ------------------------------------------------------------------------- *
 * FILE:  d_xml.pl                                                           *
 * BRIEF: provides basic XML facilities                                      *
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

:- module(d_xml, [attr/3,
		              flag/3,
                  tags/3,
                  xml_dump/1]).

/* ------------------------------------------------------------------------- */

:- consult(library(http/http_client)).
:- consult(library(http/http_open)).
:- consult(library(http/http_header)).

/* ------------------------------------------------------------------------- */

tags(Tag, [element(Tag, Attr, Content)|RestXML],
          [element(Tag, Attr, Content)|RestTags]) :-
  !,
  tags(Tag, RestXML, RestTags).
tags(Tag, [_|RestXML], Tags) :-
  !,
  tags(Tag, RestXML, Tags).
tags(_, [], []).

/* ------------------------------------------------------------------------- */

attr(Attrs, Key, Value) :-
  member(Key=Value, Attrs),
  !.
attr(_, _, 'none').

/* ------------------------------------------------------------------------- */

flag(Attrs, Key, 'true') :-
  member(Key=_, Attrs),
  !.
flag(_, _, 'false').

/* ------------------------------------------------------------------------- */

indent(Indent) :-
  sub_string('                          ',
             0, Indent, _, Str),
  writef('%w', [ Str ]).

/* ------------------------------------------------------------------------- */

xml_dump_attr([Attr|Attrs], Indent) :-
  indent(Indent),
  writef('* %w\n', [Attr]),
  xml_dump_attr(Attrs, Indent).
xml_dump_attr([], _).

/* ------------------------------------------------------------------------- */

xml_dump([element(Tag, Attr, Content)|Elements], Indent) :-
  NewIndent is Indent + 2,
  indent(Indent),
  writef('tag=%w\n', [Tag]),
  !,
  xml_dump_attr(Attr, NewIndent),
  xml_dump(Content, NewIndent),
  xml_dump(Elements, Indent).
xml_dump([], _).

/* ------------------------------------------------------------------------- */

xml_dump(XML) :-
  xml_dump(XML, 0).

/* ------------------------------------------------------------------------- *
 * END OF FILE                                                               *
 * ------------------------------------------------------------------------- */
