/* ------------------------------------------------------------------------- *
 * FILE:  d_job.pl                                                           *
 * BRIEF: Queueing and execution of individual jobs, throttle.               *
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

:- module(d_job, [loop/0, queue/4]).
:- use_module(d_wiki).

/* ------------------------------------------------------------------------- */

:-dynamic(jobs:job/4).

/* ------------------------------------------------------------------------- *
 * queue(+Prj, +Task, +Handler, +Args)                                       *
 *                                                                           *
 * Queue a job for execution                                                 *
 * ------------------------------------------------------------------------- */

queue(Prj, Task, Handler, Args) :-
  jobs:job(Prj, Task, Handler, Args),
  writef('task \'%w\' already scheduled\n', [Task]),
  !.
queue(Prj, Task, Handler, Args) :-
  asserta(jobs:job(Prj, Task, Handler, Args)).

/* ------------------------------------------------------------------------- *
 * queue(-Job)                                                               *
 *                                                                           *
 * Fetch a job or 'none' for execution                                       *
 * ------------------------------------------------------------------------- */

fetch(job(Prj, Task, Handler, Args)) :-
  jobs:job(Prj, Task, Handler, Args),
  !.
fetch(none).

/* ------------------------------------------------------------------------- *
 * queue(-Job)                                                               *
 *                                                                           *
 * Execute and handle a single query                                         *
 * ------------------------------------------------------------------------- */

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


/* ------------------------------------------------------------------------- *
 * job(+Prj, +Handler, +Args, +Data)                                         *
 *                                                                           *
 * Handle query result. Handler is intended to contain a module. Currently   *
 * it's hard-coded.                                                          *
 * ------------------------------------------------------------------------- */

job(Prj, Handler, Args, Data) :-
  Handle    =.. [ handle, Prj | Args ],

  /* Once is a DCG grammar. Consuming *all* data is required.
   * Hence the '[]'.
   */

  add_import_module(Handler, handle, start),
  call(Handler:Handle, Data, []),
  delete_import_module(Handler, handle),
  !.
job(Prj, Handler, Args, _) :-
  writef('job:failed %w(%w) on %w\n', [Handler, Args, Prj]),
  fail.

/* ------------------------------------------------------------------------- *
 * sleep_or_succeed(+Job)                                                    *
 *                                                                           *
 * Throttle jobs                                                             *
 * ------------------------------------------------------------------------- */

sleep_or_succeed(none) :-
  !,
  writef('Done.\n').
sleep_or_succeed(_) :-
  !,
  sleep(1),
  fail.

/* ------------------------------------------------------------------------- *
 * loop                                                                      *
 *                                                                           *
 * Main loop                                                                 *
 * ------------------------------------------------------------------------- */

loop :-
  !,
  repeat,
    fetch(Job),
    execute(Job),
    sleep_or_succeed(Job).

/* ------------------------------------------------------------------------- *
 * END OF FILE                                                               *
 * ------------------------------------------------------------------------- */

