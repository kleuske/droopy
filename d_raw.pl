/* ------------------------------------------------------------------------- *
 * File d_raw.pl                                                             *
 * Brief: parses wiki raw format                                             *
 *-------------------------------------------------------------------------- */
:- module(d_raw, [raw_page/3]).

/* ------------------------------------------------------------------------- *
 * raw_line/2                                                                *
 *                                                                           *
 * Parses list of prefixes                                                   *
 * ------------------------------------------------------------------------- */

raw_line(line(Prefixes, Content, [])) -->
  raw_line_prefixes(Prefixes),
  raw_line_elements(Content, [ newline ]),
  { Content \= [] },
  raw_token(newline), !.
raw_line(line(Prefixes, Content, [ truncated ])) -->
  raw_line_prefixes(Prefixes),
  raw_line_elements(Content, []),
  { Content \= [], ! }.
raw_line(empty) -->
  raw_token(newline), !.

/* ------------------------------------------------------------------------- *
 * raw_line_element/3                                                        *
 *                                                                           *
 * Parses one element in a line                                              *
 * ------------------------------------------------------------------------- */

raw_line_element(wlink(Target, Params), _) -->
  raw_token(link_open),
  !,
  raw_target(Target),
  raw_params(Params),
  raw_token(link_close).
raw_line_element(template(Target, Params), _) -->
  raw_token(template_open),
  !,
  raw_target(Target),
  raw_params(Params),
  raw_token(template_close).
raw_line_element(Style, _) -->
  raw_style(Style),
  { writef("style: %w\n", [ Style ]) },
  !.
raw_line_element(text(Content), RejectList) -->
  raw_text(Content, [ link_open,
                      template_open,
                      style
                    | RejectList ]),
  { Content \= [] },
  { writef("text: %s\n", [Content]) },
  !.

/* ------------------------------------------------------------------------- *
 * raw_line_elements/3                                                       *
 *                                                                           *
 * Parses list of elements in a line                                         *
 * ------------------------------------------------------------------------- */

raw_line_elements([Element|Rest], RejectList) -->
  raw_line_element(Element, RejectList),
  !,
  raw_line_elements(Rest, RejectList).
raw_line_elements([], _) --> [].
  
/* ------------------------------------------------------------------------- *
 * raw_line_prefixes                                                         *
 *                                                                           *
 * Parses list of prefixes                                                   *
 * ------------------------------------------------------------------------- */

raw_line_prefixes([Prefix|Rest]) -->
  raw_line_prefix(Prefix),
  !,
  raw_line_prefixes(Rest).
raw_line_prefixes([], Rest, Rest).

/* ------------------------------------------------------------------------- *
 * raw_line_prefix                                                           *
 *                                                                           *
 * Parses complete prefix                                                    *
 * ------------------------------------------------------------------------- */

raw_line_prefix(Element) -->
  raw_line_prefix_token(Prefix),
  raw_line_prefix_tail(Prefix, Level),
  {
    !,
    Element =.. [Prefix, Level]
  }.

/* ------------------------------------------------------------------------- *
 * raw_line_prefix_token                                                     *
 *                                                                           *
 * Parses head of prefix                                                     *
 * ------------------------------------------------------------------------- */

raw_line_prefix_token(space)  --> " ".
raw_line_prefix_token(indent) --> ":".
raw_line_prefix_token(bullet) --> "*".
raw_line_prefix_token(hash)   --> "#".

/* ------------------------------------------------------------------------- *
 * raw_line_prefix_tail                                                      *
 *                                                                           *
 * Parses rest of prefix                                                     *
 * ------------------------------------------------------------------------- */

raw_line_prefix_tail(Type, Level)-->
  raw_line_prefix_token(Type),
  raw_line_prefix_tail(Type, SubLevel),
  {
    !,
    Level is SubLevel + 1
  }.
raw_line_prefix_tail(_, 1, Rest, Rest).

/* ------------------------------------------------------------------------- *
 * raw_page\3                                                                *
 *                                                                           *
 * Parses a page                                                             *
 * ------------------------------------------------------------------------- */

raw_page([Line|Rest]) -->
  raw_line(Line),
  !,
  raw_page(Rest).
raw_page([]) --> [].

/* ------------------------------------------------------------------------- *
 * raw_param                                                                 *
 *                                                                           *
 * Parses individual parameters                                              *
 * ------------------------------------------------------------------------- */
raw_param(index(Index, Content)) -->
  raw_uint(Index),
  "=",
  raw_line_elements(Content, [bar]).
  
/* ------------------------------------------------------------------------- *
 * raw_params\4                                                              *
 *                                                                           *
 * Parses parameter list                                                     *
 * ------------------------------------------------------------------------- */

raw_params([Param|Rest], Param) -->
  raw_token(bar),
  !,
  raw_params(Rest, []).
raw_params(Params, OldParam) -->
  \+ raw_token(link_close),
  \+ raw_token(template_close),
  [Ch],
  {
    !,
    append(OldParam, [Ch], NewParam)
  },
  raw_params(Params, NewParam).
raw_params([], []) --> [], !.
raw_params([Param], Param) --> [], !.

/* ------------------------------------------------------------------------- *
 * raw_params\3                                                              *
 *                                                                           *
 * Parses parameter list                                                     *
 * ------------------------------------------------------------------------- */

raw_params(Params) -->
  raw_token(bar),
  !,
  raw_params(Params, []).
raw_params([]) --> [].

/* ------------------------------------------------------------------------- *
 * raw_params\3                                                              *
 *                                                                           *
 * Parses parameter list                                                     *
 * ------------------------------------------------------------------------- */

raw_style(style(Style, Content)) -->
  raw_token(style(Style)),
  { writef("trying style %w\n", [Style]) },
  raw_line_elements(Content, [ style(_) ]),
  { writef("got content %w\n", [Content]) },
  raw_token(style(Style)),
  { writef("got style %w\n", [Style]) },
  !.

/*
raw_style(style(A, Content)) -->
  raw_token(style(A)),
  raw_line_elements(ContentA, [ style(_) ]),
  raw_style(style(_, ContentB)),
  { append(ContentA, [style(bolditalic, ContentB)], Content) },
  !.
raw_style(style(runaway, Content)) -->
  raw_token(style(_)),
  raw_line_elements(Content, [ style(_) ]),
  !.
*/

/* ------------------------------------------------------------------------- *
 * raw_path\4                                                                *
 *                                                                           *
 * Parses a target path (page-subpages)                                      *
 * ------------------------------------------------------------------------- */

raw_target_path([Page|Parts], Page) -->
  raw_token(slash),
  !,
  raw_target_path(Parts, []).
raw_target_path(Parts, OldPage) -->
  \+ raw_token(bar),
  \+ raw_token(link_close),
  \+ raw_token(template_close),
  [ Ch ],
  {
    !,
    append(OldPage, [ Ch ], NewPage)
  },
  raw_target_path(Parts, NewPage).
raw_target_path([], []) --> [], !.
raw_target_path([Page], Page) --> [], !.

/* ------------------------------------------------------------------------- *
 * raw_target_path\3                                                         *
 *                                                                           *
 * Convenience call for raw_path 4                                           *
 * ------------------------------------------------------------------------- */
raw_target_path(Parts) -->
  !,
  raw_target_path(Parts, []).

/* ------------------------------------------------------------------------- *
 * raw_target                                                                *
 *                                                                           *
 * Parses link or template target path                                       *
 * ------------------------------------------------------------------------- */

raw_target(absolute(Parts)) -->
  raw_token(colon),
  !,
  raw_target_parts(Parts, []).
raw_target(reference(Parts)) -->
  raw_target_parts(Parts, []).

/* ------------------------------------------------------------------------- *
 * raw_target_parts                                                          *
 *                                                                           *
 * Parses link or template target path                                       *
 * ------------------------------------------------------------------------- */

raw_target_parts([Part|Parts], Part) -->
  raw_token(colon),
  !,
  raw_target_parts(Parts, []).
raw_target_parts(Parts, OldPart) -->
  \+ raw_token(bar),
  \+ raw_token(link_close),
  \+ raw_token(template_close),
  [Ch], !,
  {
    append(OldPart, [ Ch ], NewPart)
  },
  raw_target_parts(Parts, NewPart).
raw_target_parts([], []) --> [], !.
raw_target_parts([Part], Part) --> [].

/* ------------------------------------------------------------------------- *
 * raw_text                                                                  *
 *                                                                           *
 * Parses raw text                                                           *
 * ------------------------------------------------------------------------- */

raw_text([Elem|Rest], RejectList) -->
  raw_token_reject(RejectList),
  [ Elem ],
  !,
  raw_text(Rest, RejectList).
raw_text([], _) --> [].

/* ------------------------------------------------------------------------- *
 * raw_token                                                                 *
 *                                                                           *
 * defines various tokens.                                                   *
 * ------------------------------------------------------------------------- */

raw_token(bar)               --> "|",     !.
raw_token(colon)             --> ":",     !.
raw_token(newline)           --> "\n",    !.
raw_token(slash)             --> "/",     !.
raw_token(style(bold))       --> "'''",   !.
raw_token(style(italic))     --> "''",    !.
raw_token(template_open)     --> "{{",    !.
raw_token(template_close)    --> "}}",    !.
raw_token(link_open)         --> "[[",    !.
raw_token(link_close)        --> "]]",    !.

raw_token_reject(List) -->
  { member(Elem, List) },
  raw_token(Elem),
  { !, fail }.
raw_token_reject(_) --> [].

raw_id(Id) -->
  [ Ch ],
  { char_type(Ch, csymf), ! },
  raw_id_tail(Tail),
  {
    append([Ch], Tail, IdText),
    name(Id, IdText)
  }.
raw_id_tail([Ch|Rest]) -->
  [ Ch ],
  { char_type(Ch, csym), ! },
  raw_id_tail(Rest).
raw_id_tail([]) --> [].

raw_int(Int) -->
  "-",
  !,
  raw_uint(UInt),
  { Int is UInt * -1 }.
raw_int(Int) -->
  raw_uint(Int).

raw_uint(Int) -->
  raw_uint_list(List),
  { !, name(Int, List) }.

raw_uint_list([Ch|Rest]) -->
  [ Ch ],
  { char_type(Ch, digit), ! },
  raw_uint_list(Rest).
raw_uint_list([]) --> [].

raw_digit(Digit) :-
  member(Digit, "0123456789").
raw_hexdigit(Digit) :-
  member(Digit, "0123456789ABCDEFabcdef").

/* ------------------------------------------------------------------------- *
 * End of file                                                               *
 * ------------------------------------------------------------------------- */
