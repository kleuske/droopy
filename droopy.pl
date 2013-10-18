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
  d_xml:get_xml(URL, Xml),
  d_parse:api(Xml, SiteMatrix),
  d_matrix:insert(special(commons), SiteMatrix).

droopy :-
  assertz(links:ln(none, none)),
  assertz(jobs:job(none, none, [])),
  init_sites('http://commons.wikimedia.org/w/api.php?format=xml&action=sitematrix').

test(N) :-
  d_pagenet:start(prj(nl, wiki), 'Aardwerk', N).

/* ------------------------------------------------------------------------- *
 * END OF FILE                                                               *
 * ------------------------------------------------------------------------- */
