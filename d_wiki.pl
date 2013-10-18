/* ------------------------------------------------------------------------- *
 * FILE:  d_wiki.pl                                                          *
 * BRIEF: provides WIKI access                                               *
 *                                                                           *
 * This file is part of 'Droopy' a prolog wikipedia bot           *
 * ------------------------------------------------------------------------- */

:- module(d_wiki,
[
  page_cats/2,
  page_info/2,
  page_links/2,
  page_links/3,
  page_raw/2,
  page_revs/2,
  site_matrix/1,
  site_namespaces/1,
  user_contribs/2,
  wiki/3
]).

/* ------------------------------------------------------------------------- *
 * URL's to access the MediaWiki API                                         *
 * ------------------------------------------------------------------------- */

query([format=xml, action=query]).
page_raw(Title, [format=xml, action=raw, title=Title]).
site_matrix([format=xml, action=sitematrix]).
site_namespaces(Parts) :-
  query(Base),
  append(Base, [meta=siteinfo, siprop=namespaces], Parts).
page_info(Title, Parts) :-
  query(Base),
  append(Base, [titles=Title, prop=info], Parts).
page_cats(Title, Parts) :-
  query(Base),
  append(Base, [titles=Title, prop=categories], Parts).
page_links(Title, Parts) :-
  query(Base),
  append(Base, [titles=Title, prop=links, pllimit=max], Parts).
page_links(Title, NS, Parts) :-
  query(Base),
  append(Base, [titles=Title, prop=links, plnamespace=NS, pllimit=max], Parts).
page_revs(Title, Parts) :-
  query(Base),
  append(Base, [titles=Title, prop=revisions, rvlimit=max], Parts).
user_contribs(User, Parts) :-
  query(Base),
  append(Base, [ucuser=User, list=usercontribs, uclimit=max], Parts).

wiki(Prj, Query, Reply) :-
  wiki_apply(Prj, Query, Parts, []),
  wiki_once(Parts, FirstReply),
  wiki_more(Prj, Query, FirstReply, Reply).

wiki_once(Parts, Reply) :-
  parse_url(URL, Parts),
  !,
  d_io:get_xml(URL, XML),
  d_parse:api(XML, Reply).

wiki_more(Prj, Query, more(Mark, FirstReply), [Reply]) :-
  !,
  writef("More: mark = %w\n", [Mark]),
  wiki_apply(Prj, Query, Parts, [Mark]),
  wiki_once(Parts, NextReply),
  wiki_more(Prj, Query, NextReply, LastReply),
  writef("Merge...\n"),
  wiki_merge(FirstReply, LastReply, Reply).
wiki_more(_, _, Reply, Reply).

wiki_apply(Prj, Query, Parts, Extras) :-
  d_matrix:api_url(Prj, API),
  call(Query, SearchTermsRaw),
  append(SearchTermsRaw, Extras, SearchTerms),
  append(API, [search(SearchTerms)], Parts).

wiki_merge([ObjA|_], [ObjB|_], Result) :-
  ObjA =.. ListA,
  ObjB =.. ListB,
  !,
  wiki_merge_all(ListA, ListB, ListR),
  Result =.. ListR.
wiki_merge(A, B, _) :-
  writef("Weirdness...\n\n\n%w\n\n\n%w\n", [A, B]), !, fail.

wiki_merge_all([A|RestA], [B|RestB], [R|RestR]) :-
  wiki_merge_one(A, B, R),
  !,
  wiki_merge_all(RestA, RestB, RestR).
wiki_merge_all([], [], []).

wiki_merge_one(A, B, Result) :-
  is_list(A),
  is_list(B),
  !,
  append(A, B, Result).
wiki_merge_one(none, none, _) :-
  !,
  writef("Merge none\n").
wiki_merge_one(A, none, A) :-
  atom(A),
  !,
  writef("Merge none %w\n", [A]).
wiki_merge_one(none, A, A) :-
  atom(A),
  !,
  writef("Merge none %w\n", [A]).
wiki_merge_one(A, A, A) :-
  atom(A),
  !,
  writef("Merge unify %w\n", [A]).


/*
raw(Site, Title, Page) :-
  page_raw(Site, Title, Url),
  writef("url=%w\n", [Url]),
  d_io:get_raw(Url, Data),
  writef("got page", [Url]),
  d_raw2:raw_page(Page, Data, _).
*/
/* ------------------------------------------------------------------------- *
 * END OF FILE                                                               *
 * ------------------------------------------------------------------------- */
