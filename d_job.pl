:- module(d_job, [loop/0, queue/4]).
:- use_module(d_wiki).
:- use_module(d_pagenet).

:-dynamic(jobs:job/4).

queue(Prj, Task, Handler, Args) :-
  jobs:job(Prj, Task, Handler, Args),
  writef('task \'%w\' already scheduled\n', [Task]),
  !.
queue(Prj, Task, Handler, Args) :-
  asserta(jobs:job(Prj, Task, Handler, Args)).

fetch(job(Prj, Task, Handler, Args)) :-
  jobs:job(Prj, Task, Handler, Args),
  !.
fetch(none).

execute(none) :- !.
execute(job(Prj, Task, Handler, Args)) :-
  d_wiki:wiki(Prj, Task, Data),
  job(Prj, Handler, Args, Data),
  !,
  retract(jobs:job(Prj, Task, Handler, Args)).
execute(job(Prj, Task, Handler, Args)) :-
  !,
  writef('\nTask \'%w\' failed.\n', [Task]),
  retract(jobs:job(Prj, Task, Handler, Args)).

job(Prj, _, Args, Data) :-
  Once    =.. [ once, Prj | Args ],

  /* Once is a DCG grammar. Consuming *all* data is required.
   * Hence the '[]'.
   */

  call(Once, Data, []),
  !.
job(Prj, Handler, Args, _) :-
  writef('job:failed %w(%w) on %w\n', [Handler, Args, Prj]),
  fail.

sleep_or_succeed(none) :-
  !,
  writef('Done.\n').
sleep_or_succeed(_) :-
  !,
  sleep(1),
  fail.

loop :-
  !,
  repeat,
    fetch(Job),
    execute(Job),
    sleep_or_succeed(Job).
