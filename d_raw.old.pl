:-module(d_raw, [raw/3]).

path([Part|Parts]) -->
  Str,
  "/", !,
  {
    name(Part, Str)
  },
  path(Parts).
path([Part]) -->
  Str,
  {
    name(Part, Str)
  }.

prefix(absolute([Parts]) -->
  ":",
  !,
  prefix_parts(Parts).
prefix(reference([Parts]) -->
  prefix_parts(Parts).

prefix_parts([Part|Parts]) -->
  String,
  ":", !,
  {
    name(Part, String)
  },
  prefix_parts(Parts).
prefix_parts([], Rest, Rest).

raw([Line|Elements]) -->
  raw_line(Line),
  !,
  raw(Elements).
raw([], Rest, Rest).

raw_line(empty)-->
  "\n".
raw_line(bullet(Elem))-->
  raw_line_prefixes(Prefix),
  raw_elements(Elem),
  raw_line(Rest).

raw_element(Element) -->
  raw_link(Element).
raw_element(text(Content)) -->
  raw_text(Content).

raw_link(link(Target, Alias, [])) -->
  "[[",
  target(Target),
  "|"
  Alias,
  "]]".
raw_link(link(Target, Target, [])) -->
  "[[",
  target(Target),
  "]]".

raw_target(Prefix, Path)) -->
  raw_target_prefix(Prefix),
  raw_target_path(Path).
raw_target(reference(Prefix, Path)) -->
  raw_target_prefix(Prefix),
  ":",
  raw_target_path(Path).
raw_target(absolute(none, Path) -->
  ":", !,
  Target.
raw_target(reference) -->
  Target.

raw_text([Elem|Rest]) -->
  \+ token(_),
  [ Elem ],
  !,
  text(Rest).
raw_text([], Rest, Rest).

raw_token(newline) --> "\n".
raw_token(style) --> "''".
raw_token(template_open) --> "{{".
raw_token(template_close) --> "}}".
raw_token(link_open) --> "[[".
raw_token(link_close) --> "]]".