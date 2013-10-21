:- module(d_page,
          [ page_query/4 ]).

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

:-dynamic(data:page/9).

/* ------------------------------------------------------------------------- */

page_query(Prj, Title, Options, Query) :-
  d_matrix:api_url(Prj, Site),
  query_props(Options, Props, Args),
  query_proplist(Props, PropList),
  append(Site, [ search([format = xml,
                         action = query,
                         titles = Title,
                         prop   = PropList|Args])], Query).

/* ------------------------------------------------------------------------- */
query_base([]).

query_props([Option|Rest], [Prop|RestProp], Args) :-
  !,
  query_prop(Option, Prop, HeadArgs),
  query_props(Rest, RestProp, TailArgs),
  append(HeadArgs, TailArgs, Args).
query_props([], [], []).  

/* ------------------------------------------------------------------------- */

query_prop(info,                  info,       [ ]).
query_prop(categories,            categories, [ cllimit=max ]).
query_prop(categories(hidden),    categories, [ cllimit=max,
                                                clshow=hidden]).
query_prop(categories(nothidden), categories, [ cllimit=max,
                                                clshow='!hidden' ]).
query_prop(links(all),            links,      [ pllimit=max]).
query_prop(links(Namespaces),     links,      [ plnamespace=Props,
                                                pllimit=max]) :-
  query_proplist(Namespaces, Props).
query_prop(langlinks,             langlinks,  [ lllimit=max ]).
query_prop(langlinks,             langlinks,  [ lllimit=max ]).
query_prop(revisions,             revisions,  [ rvlimit=max ]).
query_prop(revisions(terse),      revisions,  [ rvlimit=max,
                                                rvprop='ids|user|timestamp' ]).
query_prop(revisions(PropList),   revisions,  [ rvlimit=max,
                                                rvprop=Props]) :-
  query_proplist(PropList, Props).
query_prop(templates,             templates,  [ tllimit=max ]).
query_prop(templates(Namespaces), templates,  [ tllimit=max,
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
