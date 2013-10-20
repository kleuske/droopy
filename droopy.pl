/* ------------------------------------------------------------------------- *
 * File droopy.pl                                                            *
 * Brief: main file.                                                         *
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
:- module(droopy, [ droopy/0,
                    test/1    ]).
:- use_module(d_io).
:- use_module(d_job).
:- use_module(d_matrix).
:- use_module(d_pagenet).
:- use_module(d_parse).
:- use_module(d_wiki).
:- use_module(d_xml).

/* Initialize site matrix from fixed site. After this is done, all other URL's
 * can be calculated.
 */

init_sites(URL) :-
  writef('Initializing site matrix from %w\n', [URL]),
  d_io:get_xml(URL, Xml),
  d_parse:api(Xml, SiteMatrix),
  d_matrix:insert(special(commons), SiteMatrix).

droopy :-
  init_sites('http://commons.wikimedia.org/w/api.php?format=xml&action=sitematrix').

test(N) :-
  droopy,
  d_pagenet:start(prj(nl, wiki), 'Aardwerk', N),
  d_job:loop.

/* ------------------------------------------------------------------------- *
 * END OF FILE                                                               *
 * ------------------------------------------------------------------------- */
