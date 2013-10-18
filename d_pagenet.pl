/* ------------------------------------------------------------------------- *
 * FILE:  d_pagenet.pl                                                       *
 * BRIEF: Payload. Creates database with (recursive) links                   *
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

:- module(d_pagenet, [clear/0,
                      once/4,
                      start/3]).
:- use_module(d_job).
:-dynamic(links:ln/2).
:-dynamic(pages:info/8).

clear :-
  abolish(links:ln/2),
  abolish(pages:info/8).

start(Prj, Title, Depth) :-
  d_job:queue(Prj, page_links(Title), d_pagenet, [ Depth ]).

once(Prj, Count) -->
  [ page(_, _, Title, _, _, _, _, Links) ],
  {
    !,
    writef('page: %w\n', [Title]),
    pg_links(Count, Prj, Title, Links, _)
  },
  once(Prj, Count).
once(_, _, Rest, Rest).

pg_links(0, _, Title) -->
  [ pagelink(_, LinkTitle) ],
  {
    !,
    /* writef('leaf : %w\n', [LinkTitle]), */
    pg_store(Title, LinkTitle)
  },
  pg_links(0, _, Title).
/*
pg_links(Count, Prj, Title) -->
  [pagelink(_, LinkTitle)],
  {
    pg_backlink(Title, LinkTitle),
    !,
    writef('back : %w\n', [LinkTitle])
  },
  pg_links(Count, Title).
*/
pg_links(Count, Prj, Title) -->
  [ pagelink(_, LinkTitle) ],
  {
    !,
/*    writef('branch : %w\n', [LinkTitle]), */
    pg_store(Title, LinkTitle),
    pg_seed(Prj, LinkTitle, Count)
  },
  pg_links(Count, Prj, Title).
pg_links(_, _, _, Rest, Rest).

pg_store(Title, LinkTitle) :-
  assertz(links:ln(Title, LinkTitle)),
  !.
pg_store(_, _).

pg_seed(Prj, Title, Count) :-
  \+ links:ln(Title, _),
  !,
  NewCnt is Count - 1,
  d_job:queue(Prj,
              page_links(Title, 0),
              d_pagenet, [NewCnt]).
pg_seed(_, _, _).
/*
pg_backlink(Title, LinkTitle) :-
  links:ln(LinkTitle, Title),
  !.
pg_backlink(Title, LinkTitle) :-
  links:ln(LinkTitle, SomeTitle),
  SomeTitle \= none,
  !,
  pg_backlink(Title, SomeTitle).
*/

/* ------------------------------------------------------------------------- *
 * END OF FILE                                                               *
 * ------------------------------------------------------------------------- */
