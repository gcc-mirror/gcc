/* Implement tasking-related runtime actions for CHILL.
   Copyright (C) 1992,1993 Free Software Foundation, Inc.
   Author: Wilfried Moser

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include "rtltypes.h"
#include "rts.h"

/*
 * function __continue
 *
 * parameters:
 *     evaddr     pointer to Eventlocation
 *     filename   source file name where function gets called
 *     lineno     linenumber in source file
 *
 * returns:
 *     void
 *
 * exceptions:
 *     none
 *
 * abstract:
 *     implement the CHILL CONTINUE action.
 */

void
__continue (evaddr, filename, lineno)
     Event_Queue   **evaddr;
     char           *filename;
     int             lineno;
{
  Event_Queue  *ev = *evaddr;
  Event_Queue  *wrk;

  if (ev == 0)
    /* nothing to do */
    return;

  /* search for 1st one is not already continued */
  while (ev && ev->is_continued)
    ev = ev->forward;
  if (!ev)
    /* all have been continued in that queue, do nothing */
    return;

  wrk = ev->startlist;
  while (wrk)
    {
      Event_Queue     *tmp = (Event_Queue *)wrk->listhead;
      
      while (tmp->forward != wrk)
	tmp = tmp->forward;
      tmp->forward = wrk->forward;
      wrk = wrk->chain;
    }

  /* so far so good, continue this one */
  ev->is_continued = 1;
  ev->who_continued = THIS;

  /* tell the runtime system to activate the process */
  __continue_that (ev->this, ev->priority, filename, lineno);
}

/* force function print_event to be linked */
extern void __print_event ();
static EntryPoint pev = __print_event;
