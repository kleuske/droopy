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
                      query/2,
                      handle/4,
                      start/3]).
:- use_module(d_job).
:- dynamic(links:pl/2).
:- dynamic(pages:info/5).

/* ------------------------------------------------------------------------- */

clear :-
  abolish(links:pl/2),
  abolish(pages:info/5).

/* ------------------------------------------------------------------------- */

query(Title, page(Title, [info, links(0)])).

/* ------------------------------------------------------------------------- */

start(Prj, Title, Depth) :-
  query(Title, Query),
  d_job:queue(Prj, Query, d_pagenet, [ Depth ]).

/* ------------------------------------------------------------------------- */

handle(Prj, Count) -->
  [ page(loc(none, NS, Title), _, []) ],
  {
    !,
    writef('redl: %w\n', [Title]),
    d_page:insert(loc(Prj, none, NS, Title), none)
  },
  handle(Prj, Count).

handle(Prj, Count) -->
  [ page(loc(ID, NS, Title), Info, PropList) ],
  {
    member(links(Links), PropList),
    !,
    writef('page: %w\n', [Title]),
    d_page:insert(loc(Prj, ID, NS, Title), Info),
    pg_links(Count, Prj, Title, Links, _)
  },
  handle(Prj, Count).

handle(_, _) --> [].

/* ------------------------------------------------------------------------- */

pg_links(0, _, Title) -->
  [ pl(_, LinkTitle) ],
  {
    !,
    /*writef('leaf : %w\n', [LinkTitle]),*/
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
  [ pl(_, LinkTitle) ],
  {
    !,
    /*writef('branch : %w\n', [LinkTitle]),*/
    pg_store(Title, LinkTitle),
    pg_seed(Prj, LinkTitle, Count)
  },
  pg_links(Count, Prj, Title).
pg_links(_, _, _, Rest, Rest).

/* ------------------------------------------------------------------------- */

pg_store(Title, LinkTitle) :-
  \+ links:pl(Title, LinkTitle),
  !,
  assertz(links:pl(Title, LinkTitle)).
pg_store(_, _).

/* ------------------------------------------------------------------------- */

pg_seed(Prj, Title, Count) :-
  \+ d_page:info(title(Prj, '0', Title), _),
  !,
  NewCnt is Count - 1,
  d_job:queue(Prj,
              page(Title, [ info, links(0)]),
              d_pagenet,
              [ NewCnt ]).
pg_seed(_, _, _).

/* ------------------------------------------------------------------------- *
 * END OF FILE                                                               *
 * ------------------------------------------------------------------------- */
