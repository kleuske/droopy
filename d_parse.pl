/* ------------------------------------------------------------------------- *
 * FILE:  d_parse.pl                                                         *
 * BRIEF: API reply parser                                                   *
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

:- module('d_parse', [ api/2 ] ).
:- use_module(d_xml).

/* ------------------------------------------------------------------------- *
 * api(+XML, -Data)                                                          *
 *                                                                           *
 * toplevel predicate for parsing Wiki-API reply                             *
 * ------------------------------------------------------------------------- */

api(XML, Info) :-
  api(Info, XML, []).

/* ------------------------------------------------------------------------- *
 * api(+XML, -Data)                                                          *
 *                                                                           *
 * parses Wiki-API reply                                                     *
 * ------------------------------------------------------------------------- */

api(Reply) -->
  [ element('api', _, Content) ],
  {
    !,
    api_limits(Content, Rest),
    api_content(Reply, Rest, _)
  }.
api(_, Msg, Msg) :-
  writef("Parsing api failed\n"),
  writef("%w\n", [Msg]),
  !,
  fail.

/* ------------------------------------------------------------------------- *
 * api_limits(+XML, -Data)                                                   *
 *                                                                           *
 * parses limits                                                             *
 * ------------------------------------------------------------------------- */

api_limits -->
  [ element('limits', _, _) ], !.
api_limits(Rest, Rest).

/* ------------------------------------------------------------------------- *
 * api(+XML, -Data)                                                          *
 *                                                                           *
 * parses Wiki-API content (query, sitematrix, etc)                          *
 * ------------------------------------------------------------------------- */

api_content(Reply) -->
  [ element('query', _, Content) ],
  {
    !,
    query(Reply, Content, _)
  }.
api_content(more(Mark, Reply)) -->
  [ element('query-continue', _, Content) ],
  {
    !,
    query_continue(Mark, Content, _)
  },
  api_content(Reply).
api_content(matrix(Count, Specials, Languages)) -->
  [ element('sitematrix', Attr, Content) ],
  {
    !,
    d_xml:attr(Attr, 'count', Count),
    matrix_specials(Specials, Content, Rest),
    matrix_language(Languages, Rest, _)
  }.
api_content(none, Text, _) :-
  !,
  writef("No readable api content"),
  writef("\n---\n%w\n---\n", [Text]),
  fail.

/* ------------------------------------------------------------------------- *
 * api(+XML, -Data)                                                          *
 *                                                                           *
 * parses sitematrix language list                                           *
 * ------------------------------------------------------------------------- */

matrix_language([language(LC, Name, Local, Projects)|Rest]) -->
  [ element('language', Attr, Content) ],
  {
    !,
    d_xml:attr(Attr, 'code',      LC),
    d_xml:attr(Attr, 'name',      Name),
    d_xml:attr(Attr, 'localname', Local),
    matrix_site(Projects, Content, _)
  },
  matrix_language(Rest).
matrix_language([], Rest, Rest).

/* ------------------------------------------------------------------------- *
 * api(+XML, -Data)                                                          *
 *                                                                           *
 * parses sitematrix 'site' item                                             *
 * ------------------------------------------------------------------------- */

matrix_site(Projects) -->
  [ element('site', _, Content) ],
  {
    !,
    matrix_projects(Projects, Content, _)
  }.

/* ------------------------------------------------------------------------- *
 * matrix_projects(-Page)                                                    *
 *                                                                           *
 * Parses sitematrix projects (e.g. nl.wikipedia.org)                        *
 * ------------------------------------------------------------------------- */

matrix_projects([prj(Url,
                     DBName,
                     Code,
                     Name,
                     Closed,
                     Private,
                     Fishbowl)|Rest]) -->
    [element('site', Attr, _) ],
    {
      !,
      d_xml:attr(Attr, 'url',      Url),
      d_xml:attr(Attr, 'dbname',   DBName),
      d_xml:attr(Attr, 'code',     Code),
      d_xml:attr(Attr, 'sitename', Name),

      d_xml:flag(Attr, 'closed',   Closed),
      d_xml:flag(Attr, 'private',  Private),
      d_xml:flag(Attr, 'fishbowl', Fishbowl)
    },
    matrix_projects(Rest).
matrix_projects([], Rest, Rest).

/* ------------------------------------------------------------------------- *
 * matrix_specials(-Page)                                                    *
 *                                                                           *
 * Parses sitematrix special sites (e.g ''commons'')                         *
 * ------------------------------------------------------------------------- */

matrix_specials(Specials) -->
  [ element('specials', _, Content) ],
  {
    !,
    matrix_special(Specials, Content, _)
  }.

/* ------------------------------------------------------------------------- *
 * matrix_special(-Page)                                                     *
 *                                                                           *
 * Parses sitematrix special site (e.g ''commons'')                          *
 * ------------------------------------------------------------------------- */

matrix_special([special(Url,
                              DBName,
                              Name,
                              Closed,
                              Private,
                              Fishbowl)|Rest]) -->
  [ element('special', Attr, _) ],
  {
    !,
    d_xml:attr(Attr, 'url',      Url),
    d_xml:attr(Attr, 'dbname',   DBName),
    d_xml:attr(Attr, 'code',     Name),
    d_xml:flag(Attr, 'closed',   Closed),
    d_xml:flag(Attr, 'private',  Private),
    d_xml:flag(Attr, 'fishbowl', Fishbowl)
  },
  matrix_special(Rest).
matrix_special([], Rest, Rest).

/* ------------------------------------------------------------------------- *
 * query(-Page)                                                              *
 *                                                                           *
 * Parses a query reply                                                      *
 * ------------------------------------------------------------------------- */

query(Reply) -->
  [ element('pages', _, Content) ],
  {
    !,
    pages(Reply, Content, _)
  }.
query(Reply) -->
  [ element('usercontribs', _, Content) ],
  {
    !,
    user_contribs(Reply, Content, _)
  }.

/* ------------------------------------------------------------------------- *
 * query(-Page)                                                              *
 *                                                                           *
 * Parses a query-continue reply                                             *
 * ------------------------------------------------------------------------- */

query_continue(Mark) -->
  [ element('links', [Mark], _) ], !.

/* ------------------------------------------------------------------------- *
 * pages(-Page)                                                              *
 *                                                                           *
 * Parses a list of pages                                                    *
 * ------------------------------------------------------------------------- */

pages([Page|Rest]) -->
  page(Page),
  !,
  pages(Rest).
pages([], Rest, Rest).

/* ------------------------------------------------------------------------- *
 * page(-Page)                                                               *
 *                                                                           *
 * Fetches page info                                                         *
 * ------------------------------------------------------------------------- */

page(page(loc(ID, NS, Title),
          info(LastRev,
               Counter,
               Model,
               Touched,
               Length),
          Data)
    ) -->
  [ element( 'page', Attr, Content ) ],
  {
    !,
    d_xml:attr(Attr, 'pageid',       ID),
    d_xml:attr(Attr, 'ns',           NS),
    d_xml:attr(Attr, 'contentmodel', Model),
    d_xml:attr(Attr, 'title',        Title),
    d_xml:attr(Attr, 'touched',      Touched),
    d_xml:attr(Attr, 'lastrevid',    LastRev),
    d_xml:attr(Attr, 'counter',      Counter),
    d_xml:attr(Attr, 'length',       Length),

    page_content(Data, Content, [])
  }.

/* ------------------------------------------------------------------------- *
 * page_content(-Data)                                                       *
 *                                                                           *
 * Handles all info for pages.                                               *
 * ------------------------------------------------------------------------- */

page_content([Head|Tail]) -->
    page_content_one(Head),
    !,
    page_content(Tail).
page_content([]) --> [].

page_content_one(categories(Categories)) -->
  [ element( 'categories', _, Content) ],
  {
    !,
    page_cats(Categories, Content, [])
  }.
page_content_one(links(Links)) -->
  [ element( 'links', _, Content) ],
  {
    !,
    page_links(Links, Content, [])
  }.
page_content_one(revisions(Revisions)) -->
  [ element( 'revisions', _, Content) ],
  {
    !,
    page_revs(Revisions, Content, [])
  }.

/* ------------------------------------------------------------------------- *
 * page_cats(-Categories)                                                    *
 *                                                                           *
 * Handles categories for pages.                                             *
 * ------------------------------------------------------------------------- */

page_cats([Cat|Rest]) -->
  page_cat(Cat),
  !,
  page_cats(Rest).
page_cats([], Rest, Rest).

page_cat(cat(NS, Title)) -->
  [ element('cl', Attr, []) ],
  {
    !,
    d_xml:attr(Attr, 'ns', NS),
    d_xml:attr(Attr, 'title', Title)
  }.

/* ------------------------------------------------------------------------- *
 * page_links(-Links)                                                        *
 *                                                                           *
 * Handles links for pages.                                                  *
 * ------------------------------------------------------------------------- */

page_links([Link|Rest]) -->
  page_link(Link),
  !,
  page_links(Rest).
page_links([], Rest, Rest).

page_link(pl(NS, Title)) -->
  [ element('pl', Attr, _) ],
  {
    !,
    d_xml:attr(Attr, 'ns', NS),
    d_xml:attr(Attr, 'title', Title)
  }.

/* ------------------------------------------------------------------------- *
 * page_revs(-Revisions)                                                     *
 *                                                                           *
 * Handles revisions for pages.                                              *
 * ------------------------------------------------------------------------- */

page_revs([Rev|Rest]) -->
  page_rev(Rev),
  !,
  page_revs(Rest).
page_revs([], Rest, Rest).

page_rev(rv(ID, Parent, User, Time, Comment)) -->
  [ element('rev', Attr, []) ],
  {
    !,
    d_xml:attr(Attr, 'revid',     ID),
    d_xml:attr(Attr, 'parentid',  Parent),
    d_xml:attr(Attr, 'user',      User),
    d_xml:attr(Attr, 'timestamp', Time),
    d_xml:attr(Attr, 'comment',   Comment)
  }.

/* ------------------------------------------------------------------------- *
 * user_contribs(-Contribs)                                                  *
 *                                                                           *
 * Handles user contributions                                                *
 * ------------------------------------------------------------------------- */

user_contribs([Contrib|Rest]) -->
  user_contrib(Contrib),
  !,
  user_contribs(Rest).
user_contribs([], Rest, Rest).

user_contrib(contr(NS,
                   Page,
                   Title,
                   Revision,
                   Comment,
                   New,
                   Top)) -->
  [ element('item', Attr, _) ],
  {
    d_xml:attr(Attr, 'ns',      NS),
    d_xml:attr(Attr, 'pageid',  Page),
    d_xml:attr(Attr, 'title',   Title),
    d_xml:attr(Attr, 'revid',   Revision),
    d_xml:attr(Attr, 'comment', Comment),
    d_xml:flag(Attr, 'new',     New),
    d_xml:flag(Attr, 'top',     Top)
  }.

/* ------------------------------------------------------------------------- *
 * END OF FILE                                                               *
 * ------------------------------------------------------------------------- */

