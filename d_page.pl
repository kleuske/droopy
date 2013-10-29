/* ------------------------------------------------------------------------- *
 * FILE:  d_page.pl                                                          *
 * BRIEF: page query generator                                               *
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

:- module(d_page,
          [ page_query/3,
            info/2 ]).

/* data:page(ID,       
             Namespace,         
             Title,
             ContentModel,
             Touched,
             LastRev,
             Counter,
             Length,
             Info       
           )
*/

:-dynamic(pages:page/2).

/* ------------------------------------------------------------------------- */

page_query(Title, Options, [ format = xml,
                             action = query,
                             titles = Title,
                             prop   = PropList
                           | Args ]) :-
  query_props(Options, Props, Args),
  query_proplist(Props, PropList).

/* ------------------------------------------------------------------------- */

query_props([Option|Rest], [Prop|RestProp], Args) :-
  !,
  page_prop(Option, Prop, HeadArgs),
  query_props(Rest, RestProp, TailArgs),
  append(HeadArgs, TailArgs, Args).
query_props([], [], []).

/* ------------------------------------------------------------------------- */

page_prop(info,                  info,       [ ]).
page_prop(categories,            categories, [ cllimit=max ]).
page_prop(categories(hidden),    categories, [ cllimit=max,
                                                clshow=hidden]).
page_prop(categories(nothidden), categories, [ cllimit=max,
                                                clshow='!hidden' ]).
page_prop(links,                 links,      [ pllimit=max]).
page_prop(links(Namespaces),     links,      [ plnamespace=Props,
                                                pllimit=max]) :-
  query_proplist(Namespaces, Props).
page_prop(langlinks,             langlinks,  [ lllimit=max ]).
page_prop(langlinks,             langlinks,  [ lllimit=max ]).
page_prop(revisions,             revisions,  [ rvlimit=max ]).
page_prop(revisions(terse),      revisions,  [ rvlimit=max,
                                                rvprop='ids|user|timestamp' ]).
page_prop(revisions(PropList),   revisions,  [ rvlimit=max,
                                                rvprop=Props]) :-
  query_proplist(PropList, Props).
page_prop(templates,             templates,  [ tllimit=max ]).
page_prop(templates(Namespaces), templates,  [ tllimit=max,
                                                tlnamespaces=Props]) :-
  query_proplist(Namespaces, Props).

/* ------------------------------------------------------------------------- */

query_proplist([Prop], Prop) :-
  !.
query_proplist([Prop|Rest], PropsAtom) :-
  !,
  query_proplist(Rest, PropTail),
  format(atom(PropsAtom), '~w|~w', [Prop, PropTail]).
query_proplist(Prop, Prop) :-
  nonvar(Prop).

/* ------------------------------------------------------------------------- */

info(title(Prj, NS, Title), page(loc(Prj, Id, NS, Title), Info)) :-
  pages:page(loc(Prj, Id, NS, Title), Info).
info(id(Prj, Id), page(loc(Prj, Id, NS, Title), Info)) :-
  pages:page(loc(Prj, Id, NS, Title), Info).
info(loc(Prj, Id, NS, Title), page(loc(Prj, Id, NS, Title), Info)) :-
  pages:page(loc(Prj, Id, NS, Title), Info).

/* ------------------------------------------------------------------------- */

insert(Loc, _) :-
  pages:page(Loc, _),
  !.
insert(Loc, Info) :-
  assertz(pages:page(Loc, Info)).

/* ------------------------------------------------------------------------- *
 * END OF FILE                                                               *
 * ------------------------------------------------------------------------- */
