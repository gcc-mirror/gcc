/* GNU CHILL compiler regression test file
 Copyright (C) 1992, 1993 Free Software Foundation, Inc.
 
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>
#include <signal.h>

#include "rts.h"


/* some allocation/reallocation functions */

static void *
xmalloc (size)
     int size;
{
  void *tmp = malloc (size);

  if (!tmp)
    {
      fprintf (stderr, "Out of heap space.\n");
      exit (1);
    }
  return (tmp);
}

static void *
xrealloc (ptr, size)
     void *ptr;
     int size;
{
  void *tmp = realloc (ptr, size);

  if (!tmp)
    {
      fprintf (stderr, "Out of heap space.\n");
      exit (1);
    }
  return (tmp);
}

/* the necessary data */
#define MAX_NUMBER 100
typedef char UsedValues[MAX_NUMBER];

#define MAX_COPIES 100

#define MAX_PER_ITEM 20
typedef struct TASKINGSTRUCTLIST
{
  struct TASKINGSTRUCTLIST *forward;
  int    num;
  TaskingStruct *data[MAX_PER_ITEM];
  char copies[MAX_COPIES];
  jmp_buf where;
} TaskingStructList;

static TaskingStructList *task_array[LAST_AND_UNUSED];
static UsedValues used_values[LAST_AND_UNUSED];

static short
get_next_free_number (vals)
     UsedValues vals;
{
  short  i;
  for (i = 1; i < MAX_NUMBER; i++)
    {
      if (!vals[i])
	{
	  vals[i] = 1;
	  return (i);
	}
    }
  fprintf (stderr, "There are no more free numbers.\n");
  exit (1);
}

/* function search for the next available copy number */
static short
get_next_copy_number (p)
     TaskingStructList *p;
{
  short i;

  for (i = 0; i < MAX_COPIES; i++)
    {
      if (!p->copies[i])
	{
	  p->copies[i] = 1;
	  return (i);
	}
    }
  fprintf (stderr, "No more copies available for \"%s\".\n",
	   p->data[0]->name);
  exit (1);
}

/* function registers a tasking entry from a module and assign
   a value to the type */

void
__register_tasking (t)
     TaskingStruct *t;
{
  TaskingStructList *p;

  /* check first if a value was provided and if it is in range */
  if (t->value_defined && *t->value >= MAX_NUMBER)
    {
      fprintf (stderr, "Value %d out of range.\n", *t->value);
      exit (1);
    }

  /* look for item defined */
  p = task_array[t->type];
  while (p)
    {
      if (!strcmp (p->data[0]->name, t->name))
	/* have found it */
	break;
      p = p->forward;
    }

  if (!p)
    {
      TaskingStructList *wrk = (TaskingStructList *)&task_array[t->type];

      /* this is a new one -- allocate space */
      p = xmalloc (sizeof (TaskingStructList));
      memset (p->copies, 0, sizeof (p->copies));
      p->forward = 0;
      p->num = 1;
      p->data[0] = t;

      /* queue it in */
      while (wrk->forward)
	wrk = wrk->forward;
      wrk->forward = p;
    }
  else
    {
      if (p->num >= MAX_PER_ITEM)
	{
	  fprintf (stderr, "Too many registrations of \"%s\".\n", t->name);
	  exit (1);
	}
      p->data[p->num++] = t;
    }
}

/* define all the entries for the runtime system. They will be
   needed by chillrt0.o */

typedef char *(*fetch_names) ();
typedef int (*fetch_numbers) ();

static char tmp_for_fetch_name[100];

char *
__fetch_name (number)
     int number;
{
  TaskingStructList *p = task_array[Process];

  while (p)
    {
      if (*(p->data[0]->value) == number)
	return (p->data[0]->name);
      p = p->forward;
    }
  sprintf (tmp_for_fetch_name, "%d", number);
  return (tmp_for_fetch_name);
}
fetch_names	__RTS_FETCH_NAMES__ = __fetch_name;

static int 
__fetch_number (name)
     char *name;
{
  TaskingStructList *p = task_array[Process];

  while (p)
    {
      if (!strcmp (p->data[0]->name, name))
	return (*(p->data[0]->value));
      p = p->forward;
    }
  return (-1);
}
fetch_numbers	__RTS_FETCH_NUMBERS__ = __fetch_number;


/* here we go to check all registered items */
static void
 __rts_init ()
{
  int i;
  TaskingStructList *p;

  for (i = Process; i <= Event; i++)
    {
      p = task_array[i];
      while (p)
	{
	  TaskingStruct *t = 0;
	  int j;
	  short val;

	  for (j = 0; j < p->num; j++)
	    {
	      if (p->data[j]->value_defined)
		{
		  if (t)
		    {
		      if (*(t->value) != *(p->data[j]->value))
			{
			  fprintf (stderr, "Different values (%d & %d) for \"%s\".",
				   *(t->value), *(p->data[j]->value), t->name);
			  exit (1);
			}
		    }
		  else
		    t = p->data[j];
		}
	    }

	  if (t)
	    {

	      val = *(t->value);

	      if (used_values[t->type][val])
		{
		  fprintf (stderr, "Value %d for \"%s\" is already used.\n",
			   val, t->name);
		  exit (1);
	        }
	      used_values[t->type][val] = 1;
	    }
	  else
	    {
	      /* we have to create a new value */
	      val = get_next_free_number (used_values[p->data[0]->type]);
	    }
	      
	  for (j = 0; j < p->num; j++)
	    {
	      p->data[j]->value_defined = 1;
	      *(p->data[j]->value) = val;
	    }

	  p = p->forward;
	}
    }
}
EntryPoint	__RTS_INIT__ = __rts_init;

/* define the start process queue */
typedef struct STARTENTRY
{
  struct STARTENTRY *forward;
  INSTANCE whoami;
  EntryPoint entry;
  void *data;
  int datalen;
} StartEntry;

static StartEntry *start_queue = 0;
static StartEntry *current_process = 0;

/* the jump buffer for the main loop */
static jmp_buf jump_buffer;
static int jump_buffer_initialized = 0;

/* look for entries in start_queue and start the process */
static void
__rts_main_loop ()
{
  StartEntry *s;

  while (1)
    {
      if (setjmp (jump_buffer) == 0)
	{
	  jump_buffer_initialized = 1;
	  s = start_queue;
	  while (s)
	    {
	      current_process = s;
	      start_queue = s->forward;
	      
	      /* call the process */
	      (*s->entry) (s->data);
	      s = start_queue;
	    }
	  /* when queue empty we have finished */
	  return;
	}
      else
	{
	  /* stop executed */
	  if (current_process->data)
	    free (current_process->data);
	  free (current_process);
	  current_process = 0;
	}
    }
}
EntryPoint	__RTS_MAIN_LOOP__ = __rts_main_loop;


void
__start_process (ptype, pcopy, arg_size, args, ins)
     short ptype;
     short pcopy;
     int arg_size;
     void *args;
     INSTANCE *ins;
{
  TaskingStructList *p = task_array[Process];
  EntryPoint pc = 0;
  int i;
  short this_copy = pcopy;
  StartEntry *s, *wrk;

  /* search for the process */
  while (p)
    {
      if (*(p->data[0]->value) == ptype)
	break;
      p = p->forward;
    }
  if (!p)
    {
      fprintf (stderr, "Cannot find a process with type %d.\n", ptype);
      exit (1);
    }
  
  /* search for the entry point */
  for (i = 0; i < p->num; i++)
    {
      if (p->data[i]->entry)
	{
	  pc = p->data[i]->entry;
	  break;
	}
    }
  if (!pc)
    {
      fprintf (stderr, "Process \"%s\" doesn't have an entry point.\n",
	       p->data[0]->name);
      exit (1);
    }

  /* check the copy */
  if (pcopy >= MAX_COPIES)
    {
      fprintf (stderr, "Copy number (%d) out of range.\n", pcopy);
      exit (1);
    }
  if (pcopy == -1)
    {
      /* search for a copy number */
      this_copy = get_next_copy_number (p);
    }
  else
    {
      if (p->copies[pcopy])
	{
	  /* FIXME: should be exception 'startfail' */
	  fprintf (stderr, "Copy number %d already in use for \"%s\".\n",
		   pcopy, p->data[0]->name);
	  exit (1);
	}
      p->copies[this_copy = pcopy] = 1;
    }

  /* ready to build start_queue entry */
  s = xmalloc (sizeof (StartEntry));
  s->forward = 0;
  s->whoami.pcopy = this_copy;
  s->whoami.ptype = ptype;
  s->entry = pc;
  s->datalen = arg_size;
  if (args)
    {
      s->data = xmalloc (arg_size);
      memcpy (s->data, args, arg_size);
    }
  else
    s->data = 0;

  /* queue that stuff in */
  wrk = (StartEntry *)&start_queue;
  while (wrk->forward)
    wrk = wrk->forward;
  wrk->forward = s;

  /* if we have a pointer to ins -- set it */
  if (ins)
    {
      ins->ptype = ptype;
      ins->pcopy = this_copy;
    }
}

void
__stop_process ()
{
  if (!jump_buffer_initialized)
    {
      fprintf (stderr, "STOP called before START.\n");
      exit (1);
    }
  longjmp (jump_buffer, 1);
}


/* function returns INSTANCE of current process */
INSTANCE
__whoami ()
{
  INSTANCE whoami;
  if (current_process)
    whoami = current_process->whoami;
  else
    {
      whoami.ptype = 0;
      whoami.pcopy = 0;
    }
  return (whoami);
}

typedef struct
{
  short *sc;
  int    data_len;
  void  *data;
} SignalDescr;

typedef struct SIGNALQUEUE
{
  struct SIGNALQUEUE *forward;
  short    sc;
  int      data_len;
  void    *data;
  INSTANCE to;
  INSTANCE from;
} SignalQueue;

/* define the signal queue */
static SignalQueue *msg_queue = 0;

/* send a signal */
void
__send_signal (s, to, prio, with_len, with)
     SignalDescr *s;
     INSTANCE     to;
     int          prio;
     int          with_len;
     void        *with;
{
  SignalQueue *wrk = (SignalQueue *)&msg_queue;
  SignalQueue *p;
  TaskingStructList *t = task_array[Process];

  /* search for process is defined and running */
  while (t)
    {
      if (*(t->data[0]->value) == to.ptype)
	break;
      t = t->forward;
    }
  if (!t || !t->copies[to.pcopy])
    {
      fprintf (stderr, "Can't find instance [%d,%d].\n",
	       to.ptype, to.pcopy);
      exit (1);
    }

  /* go to the end of the msg_queue */
  while (wrk->forward)
    wrk = wrk->forward;

  p = xmalloc (sizeof (SignalQueue));
  p->sc = *(s->sc);
  if (p->data_len = s->data_len)
    {
      p->data = xmalloc (s->data_len);
      memcpy (p->data, s->data, s->data_len);
    }
  else
    p->data = 0;
  p->to = to;
  p->from = __whoami ();
  p->forward = 0;
  wrk->forward = p;
}

void
start_signal_timeout (i, s, j)
     int i;
     SignalDescr *s;
     int j;
{
  __send_signal (s, __whoami (), 0, 0, 0);
}


/* receive a signal */
int
__wait_signal_timed (sig_got, nsigs, sigptr, datap,
		     datalen, ins, else_branche,
		     to, filename, lineno)
     short    *sig_got;
     int       nsigs;
     short    *sigptr[];
     void     *datap;
     int       datalen;
     INSTANCE *ins;
     int       else_branche;
     void     *to; 
     char     *filename;
     int       lineno; 
{
  INSTANCE me = __whoami ();
  SignalQueue *wrk, *p = msg_queue;
  int i;
  short sc;

  /* search for a signal to `me' */
  wrk = (SignalQueue *)&msg_queue;

  while (p)
    {
      if (p->to.ptype == me.ptype
	  && p->to.pcopy == me.pcopy)
	break;
      wrk = p;
      p = p->forward;
    }

  if (!p)
    {
      fprintf (stderr, "No signal for [%d,%d].\n",
	       me.ptype, me.pcopy);
      exit (1);
    }

  /* queue the message out */
  wrk->forward = p->forward;

  /* now look for signal in list */
  for (i = 0; i < nsigs; i++)
    if (*(sigptr[i]) == p->sc)
      break;

  if (i >= nsigs && ! else_branche)
    /* signal not in list and no ELSE in code */
    __cause_exception ("signalfail", __FILE__, __LINE__);

  if (i >= nsigs)
    {
      /* signal not in list */
      sc = p->sc;
      if (ins)
	*ins = p->from;
      if (p->data)
	free (p->data);
      free (p);
      *sig_got = sc;
      return (0);
    }

  /* we have found a signal in the list */
  if (p->data_len)
    {
      if (datalen >= p->data_len
	  && datap)
	memcpy (datap, p->data, p->data_len);
      else
	__cause_exception ("spacefail", __FILE__, __LINE__);
    }

  sc = p->sc;
  if (ins)
    *ins = p->from;
  if (p->data)
    free (p->data);
  free (p);
  *sig_got = sc;
  return (0);
}

/* wait a certain amount of seconds */
int
__sleep_till (abstime, reltime, fname, lineno)
     time_t abstime;
     int    reltime;
     char  *fname;
     int    lineno;
{
  sleep (reltime);
  return 0;
}

/* set up an alarm */
static int timeout_flag = 0;

static void alarm_handler ()
{
  timeout_flag = 1;
}

int *
__define_timeout (howlong, filename, lineno)
     unsigned long howlong;  /* comes in millisecs */
     char         *filename;
     int           lineno;
{
  unsigned int  prev_alarm_value;

  signal (SIGALRM, alarm_handler);
  prev_alarm_value = alarm ((unsigned int)(howlong / 1000));
  return &timeout_flag;
}

/* wait till timeout expires */
void
__wait_timeout (toid, filename, lineno)
     volatile int    *toid;
     char   *filename;
     int     lineno;
{
  while (! *toid) ;
  *toid = 0;
}
