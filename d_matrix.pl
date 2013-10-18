/* ------------------------------------------------------------------------- *
 * FILE:  d_matrix.pl                                                        *
 * BRIEF: populates a module containing the sitematrix                       *
 *                                                                           *
 * This filke is part of 'Droopy' a prolog wikipedia bot                     *
 * ------------------------------------------------------------------------- */

:- module(d_matrix, [api_url/2,
		     closed/1,
		     fishbowl/1,
                     insert/2,
		     private/1,
                     prj/2,
		     public/1,
                     special/1,
                     url/2]).

/* ------------------------------------------------------------------------- *
 * insert(+Source, +Matrix)                                                  *
 *                                                                           *
 * Inserts compete matrix object as delivered by d_parse.                    *
 * ------------------------------------------------------------------------- */

insert(Source, matrix(_, Specials, Languages)) :-
  insert_specials(Source, Specials),
  insert_languages(Source, Languages).

insert_languages(Source, [language(LC, Name, Local, Projects)|Rest]) :-
  !,
  assertz(sites:language(LC, Name, Local, Source)),
  insert_projects(Source, LC, Projects),
  insert_languages(Source, Rest).
insert_languages(_, []).

insert_projects(Source, LC, [prj(Url, DBName, Code, Name, Closed, Private, Fishbowl)|Rest]) :-
  !,
  parse_url(Url, UrlPartsRaw),
  delete(UrlPartsRaw, path(_), UrlParts),
  assertz(sites:prj(LC, UrlParts, DBName, Code, Name, Closed, Private, Fishbowl)),
  insert_projects(Source, LC, Rest).
insert_projects(_, _, []).

insert_specials(Source, [special(Url, DBName, Name, Closed, Private, Fishbowl)|Rest]) :-
  !,
  parse_url(Url, UrlPartsRaw),
  delete(UrlPartsRaw, path(_), UrlParts),
  assertz(sites:special(UrlParts, DBName, Name, Closed, Private, Fishbowl)),
  insert_specials(Source, Rest).
insert_specials(_, []).

prj(LC, Code) :-
  sites:prj(LC, _, _, Code, _, _, _, _).
special(Code) :-
  sites:special(_, _, Code, _, _, _).

url(prj(LC, Code), Url) :-
  sites:prj(LC, Url, _, Code, _, _, _, _).
url(special(Code), Url) :-
  sites:special(Url, _, Code, _, _, _).

closed(prj(LC, Code)) :-
  sites:prj(LC, _, _, Code, _, true, _, _).
closed(special(Code)) :-
  sites:special(_, _, Code, true, _, _).

fishbowl(prj(LC, Code)) :-
  sites:prj(LC, _, _, Code, _, _, _, true).
fishbowl(special(Code)) :-
  sites:special(_, _, Code, _, _, true).

private(prj(LC, Code)) :-
  sites:prj(LC, _, _, Code, _, _, true, _).
private(special(Code)) :-
  sites:special(_, _, Code, _, true, _).

public(prj(LC, Code)) :-
  sites:prj(LC, _, _, Code, _, false, false, false).
public(special(Code)) :-
  sites:special(_, _, Code, false, false, false).

api_url(Site, URL) :-
  url(Site, Base),
  !,
  append(Base, [ path('/w/api.php') ], URL).

/* ------------------------------------------------------------------------- *
 * END OF FILE                                                               *
 * ------------------------------------------------------------------------- */
