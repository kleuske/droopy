/* ------------------------------------------------------------------------- *
 * FILE:  d_wiki.pl                                                          *
 * BRIEF: provides WIKI access                                               *
 *                                                                           *
 * This filke is part of 'Droopy' a prolog wikipedia bot                     *
 * ------------------------------------------------------------------------- */

:- module(d_wiki,
[
  page_info/3,
  page_cats/3,
  page_links/3,
  page_revs/3,
  site_matrix/2,
  site_namespaces/2,
  wiki/2
]).

/* ------------------------------------------------------------------------- *
 * URL's to access the MediaWiki API                                         *
 * ------------------------------------------------------------------------- */

query(Site, URL) :-
  d_matrix:api_url(Site, API),
  format(atom(URL),
         '~w?format=xml&action=query',
         [API]).

page_raw(Site, Title, URL) :-
  www_form_encode(Title, Normalized),
  d_matrix:url(Site, Base),
  format(atom(URL),
         '~w?action=raw&title=~w',
         [Base, Normalized]).
  
site_matrix(Site, URL) :-
  d_matrix:api_url(Site, API),
  format(atom(URL),
         '~w?format=xml&action=sitematrix',
         [API]).

site_namespaces(Site, URL) :-
  d_matrix:api_url(Site, API),
  format(atom(URL),
         '~w?format=xml&action=query&meta=siteinfo&siprop=namespaces',
         [API]).

page_info(Site, Title, URL) :-
  www_form_encode(Title, Normalized),
  query(Site, Query),
  format(atom(URL),
         '~w&titles=~w&prop=info',
         [Query, Normalized]).

page_cats(Site, Title, URL) :-
  www_form_encode(Title, Normalized),
  query(Site, Query),
  format(atom(URL),
         '~w&titles=~w&prop=categories',
         [Query, Normalized]).

page_links(Site, Title, URL) :-
  www_form_encode(Title, Normalized),
  query(Site, Query),
  format(atom(URL),
         '~w&titles=~w&prop=links&pllimit=max',
         [Query, Normalized]).
page_links(Site, Title, Namespace, URL) :-
  www_form_encode(Title, Normalized),
  query(Site, Query),
  format(atom(URL),
         '~w&titles=~w&prop=links&pllimit=max&plnamespace=~w',
         [Query, Normalized, Namespace]).

page_revs(Site, Title, URL) :-
  www_form_encode(Title, Normalized),
  query(Site, Query),
  format(atom(URL),
         '~w&titles=~w&prop=revisions&rvlimit=max',
         [Query, Normalized]).

user_contribs(Site, User, URL) :-
  www_form_encode(User, Normalized),
  query(Site, Query),
  format(atom(URL),
         '~w&ucuser=~w&list=usercontribs&uclimit=max',
         [Query, Normalized]).

wiki(Query, Reply) :-
  apply(Query, [URL]),
  wiki_once(URL, FirstReply),
  wiki_more(Query, FirstReply, Reply).

wiki_once(URL, Reply) :-
  d_io:get_xml(URL, XML),
  d_parse:api(XML, Reply).

wiki_more(Query, more(Mark, FirstReply), Reply) :-
  !,
  writef("More: mark = %w\n", [Mark]),
  apply(Query, [U]),
  www_form_encode(Mark, Normalized),
  format(atom(URL), '~w&~w', [U, Normalized]),
  wiki_once(URL, NextReply),
  wiki_more(Query, NextReply, LastReply),
  append(FirstReply, LastReply, Reply).
wiki_more(_, Reply, Reply).

raw(Site, Title, Page) :-
  page_raw(Site, Title, Url),
  writef("url=%w\n", [Url]),
  d_io:get_raw(Url, Data),
  writef("got page", [Url]),
  d_raw2:raw_page(Page, Data, _).

/* ------------------------------------------------------------------------- *
 * END OF FILE                                                               *
 * ------------------------------------------------------------------------- */
