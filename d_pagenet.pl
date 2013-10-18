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



















