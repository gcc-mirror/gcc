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

extern void __cause_ex1 (char *ex, char *file, int lineno);

EXCEPTION (delayfail);
#define CAUSE_DELAYFAIL     __cause_ex1 ("delayfail", filename, lineno)

EXCEPTION (notyetimplemented);
#define CAUSE_NOTIMPLEMENTED   __cause_ex1 ("notyetimplemeyed", filename, lineno)

/*
 * function __delay_event
 *
 * parameters:
 *     ev_got      pointer to location where to write the event got.
 *     nevents     number of events in list
 *     evptrs      array of event descriptors
 *     priority    specified priority
 *     insloc      pointer to resulting instance location
 *     to          timeout value
 *     filename    filename of caller
 *     lineno      linenumber of caller
 *
 * returns:
 *     int         0 .. success
 *                 1 .. timed out
 *
 * exceptions:
 *     delayfail
 *
 * abstract:
 *     implement the CHILL DELAY and DELAY CASE actions.
 *
 */

int
__delay_event (ev_got, nevents, evptrs, priority, to, insloc, filename, lineno)
     void         **ev_got;
     int            nevents;
     Event_Descr   *evptrs;
     int            priority;
     void          *to;
     INSTANCE      *insloc;
     char          *filename;
     int            lineno;
{
  int             i, already_done = 0;
  Event_Queue    *start_list = 0;
  Event_Queue   **retval = 0;
  Event_Queue    *wrk;
  int		  timed_out = 0;
  
  /* check if all specified event queues have enough space left
     to perform the delay */
  for (i = 0; i < nevents; i++)
    {
      Event_Queue  *e;
      unsigned long cnt = 0;
      int j, have_done = 0;

      if (evptrs[i].maxqueuelength == 0)
	CAUSE_DELAYFAIL;
      else if (evptrs[i].maxqueuelength == (unsigned long)-1L)
	/* infinite length */
	continue;

      /* check if we already have processed this one, that means, this
         event is mentioned more then once */
      for (j = 0; j < i; j++)
        {
          if (evptrs[i].ev == evptrs[j].ev)
	    {
              have_done = 1;
              break;
	    }
        }
      if (have_done)
	continue;
      
      memcpy (&e, evptrs[i].ev, sizeof (Event_Queue *));
      while (e)
	{
	  cnt++;
	  e = e->forward;
	}
      if (cnt >= evptrs[i].maxqueuelength)
	CAUSE_DELAYFAIL;
    }

  for (i = 0; i < nevents; i++)
    {
      /* queue that stuff on each event */
      Event_Queue      *wrk;
      Event_Queue      *ev;
      Event_Queue      *prev_queue_entry = 0;
      Event_Queue      *prev_list_entry;
      int               j, have_done = 0;
      
      /* check for this event already processed */
      for (j = 0; j < i; j++)
	{
          if (evptrs[i].ev == evptrs[j].ev)
	    {
              have_done = 1;
              break;
	    }
	}
      if (have_done)
	continue;

      memcpy (&ev, &evptrs[i].ev, sizeof (Event_Queue *));
      MALLOC (wrk, sizeof (Event_Queue));
      memset (wrk, 0, sizeof (Event_Queue));

      wrk->priority = priority;
      wrk->this = THIS;
      wrk->listhead = evptrs[i].ev;

      /* search for the place to queue this entry in */
      while (ev->forward != 0 && ev->priority >= priority)
	{
	  prev_queue_entry = ev;
	  ev = ev->forward;
	}

      /* ready to put entry into queue */
      if (ev->forward == 0 || prev_queue_entry == 0)
	{
	  /* beginning or end of the list */
	  wrk->forward = ev->forward;
	  ev->forward = wrk;
	}
      else
	{
	  /* this is somewhere in the middle */
	  wrk->forward = prev_queue_entry->forward;
	  prev_queue_entry->forward = wrk;
	}

      /* queue it into list */
      wrk->startlist = start_list;
      if (! start_list)
	{
	  /* we are the first in the list */
	  start_list = wrk;
	  prev_list_entry = wrk;
	  wrk->startlist = start_list;
	}
      else
	{
	  prev_list_entry->chain = wrk;
	  prev_list_entry = wrk;
	}
    }

  /* tell runtime system to delay that process */
  timed_out = __delay_this (wait_event_delay, to, filename, lineno);
  if (timed_out)
    {
      /* we have to remove the entries from the queue's */
      wrk = start_list;
      while (wrk)
        {
	  Event_Queue *tmp = (Event_Queue *)wrk->listhead;
	  
	  while (tmp->forward != wrk)
	    tmp = tmp->forward;
	  tmp->forward = wrk->forward;
	  wrk = wrk->chain;
        }
    }
  
  wrk = start_list;
  while (wrk)
    {
      Event_Queue  *tmp;

      if (wrk->is_continued && ! already_done)
	{
	  already_done = 1;
	  retval = wrk->listhead;
	  if (insloc && !timed_out)
	    {
	      insloc->ptype = wrk->who_continued.ptype;
	      insloc->pcopy = wrk->who_continued.pcopy;
	    }
	}
      tmp = wrk->chain;
      FREE (wrk);
      wrk = tmp;
    }
  if (!timed_out && ev_got)
    *ev_got = (void *)retval;
  return timed_out;
}

/* force function print_event to be linked */
extern void __print_event ();
static EntryPoint pev = __print_event;
