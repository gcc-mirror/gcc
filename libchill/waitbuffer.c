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

EXCEPTION (bufferinconsistency)
#define CAUSE_BUFFINCONS  __cause_ex1 ("bufferinconsistency", filename, lineno)
EXCEPTION (spacefail);
#define CAUSE_SPACEFAIL   __cause_ex1 ("spacefail", filename, lineno)
    
/*
 * function __wait_buffer
 *
 * parameters:
 *     buf_got	   pointer to location for writing the received buffer address
 *     nbuf        number of buffers in RECEIVE CASE
 *     bufptr      array of pointers to buffer descriptor
 *     datap       pointer where to store data
 *     datalen     length of data
 *     ins         pointer to instance location or 0
 *     else_clause else specified or not
 *     to_loc      pointer to timesupervision value
 *     filename    source file name where function gets called
 *     lineno      linenumber in source file
 *
 * returns:
 *     int	   0 .. success
 *                 1 .. timed out
 *
 * exceptions:
 *     bufferinconsistency  if something's wrong in the buffer queue's
 *     spacefail            out of heap space of datalength of receiver
 *                          less then data avilable.
 *
 * abstract:
 *     implement the CHILL RECEIVE buffer CASE action.
 */

int
__wait_buffer (buf_got, nbuf, bufptr, datap, datalen, ins,
               else_clause, to, filename, lineno)
     void           **buf_got;
     int              nbuf;
     Buffer_Descr    *bufptr[];
     void            *datap;
     int              datalen;
     INSTANCE        *ins;
     int              else_clause;
     void            *to;
     char            *filename;
     int              lineno;
{
  int i;
  Buffer_Wait_Queue     *start_list;
  Buffer_Queue         **retval;
  Buffer_Queue         **highprio;
  int                    timed_out;
  
  /* look if there is a buffer already sent */
  highprio = 0;
  for (i = 0; i < nbuf; i++)
    {
      Buffer_Queue      *bq;

      memcpy (&bq, bufptr[i]->buf, sizeof (Buffer_Queue *));
      if (bq != 0 && bq->sendqueue != 0)
        {
          if (highprio != 0)
            {
              Buffer_Queue      *bsq = *highprio;
              
              if (bq->sendqueue->priority > bsq->sendqueue->priority)
                highprio = bufptr[i]->buf;
            }
          else
            highprio = bufptr[i]->buf;
        }
    }
  
  if (highprio != 0)
    {
      Buffer_Queue      *bq;

      memcpy (&bq, highprio, sizeof (Buffer_Queue *));
      if (bq != 0 && bq->sendqueue != 0)
        {
          Buffer_Send_Queue *bsq = bq->sendqueue;
          Buffer_Send_Queue *tmp;

          /* check data length */
          if (datalen < bsq->datalen)
            /* something's totaly wrong. Raise exception */
            CAUSE_SPACEFAIL;

          /* copy data out */
          memcpy (datap, bsq->dataptr, bsq->datalen);

          /* update instance, if present */
          if (ins != 0)
            memcpy (ins, &bsq->this, sizeof (INSTANCE));

          /* dequeue entry */
          tmp = bsq;
          bq->sendqueue = tmp->forward;

          if (tmp->is_delayed)
            {
              /* there is an instance delayed on a send,
                 continue it. */
              __continue_that (tmp->this, tmp->priority, filename, lineno);
              FREE (tmp);

              /* return the buffer we have received from */
	      *buf_got = (void *)highprio;
              return 0;
            }

          /* just decrease sendqueue length */
          bq->sendqueuelength--;

          FREE (tmp);

          /* as we got an entry free, we should continue
             an INSTANCE which is delayed on a send at this
             buffer */
          bsq = bq->sendqueue;
          while (bsq != 0)
            {
              if (bsq->is_delayed)
                {
                  bq->sendqueuelength++;
                  bsq->is_delayed = 0;
                  __continue_that (bsq->this, bsq->priority, filename, lineno);
                  break;
                }
              bsq = bsq->forward;
            }
          /* return the buffer we have received from */
          *buf_got = (void *)highprio;
	  return 0;
        }
      }

  /* if we come here, there is no buffer already sent */
  if (else_clause != 0)
    {
      /* in that case we return immediately */
      *buf_got = 0;
      return 0;
    }
  
  /* now we have to queue ourself to the wait queue(s) */
  start_list = 0;
  for (i = 0; i < nbuf; i++)
    {
      Buffer_Queue      *bq;
      Buffer_Wait_Queue *wrk;
      Buffer_Wait_Queue *bwq;
      Buffer_Wait_Queue *prev_queue_entry = 0;
      Buffer_Wait_Queue *prev_list_entry;
      int                j, have_done = 0;
      
      for (j = 0; j < i; j++)
        {
          if (bufptr[i]->buf == bufptr[j]->buf)
            {
              have_done = 1;
              break;
            }
        }
      if (have_done)
        continue;
      
      memcpy (&bq, bufptr[i]->buf, sizeof (Buffer_Queue *));
      if (bq == 0)
        {
          MALLOC (bq, sizeof (Buffer_Queue));
          memset (bq, 0, sizeof (Buffer_Queue));
          /* *(bufptr[i]->buf) = bq; may be unaligned */
	  memcpy (bufptr[i]->buf, &bq, sizeof (Buffer_Queue *));
        }
      MALLOC (wrk, sizeof (Buffer_Wait_Queue));
      memset (wrk, 0, sizeof (Buffer_Wait_Queue));
      bwq = (Buffer_Wait_Queue *)&bq->waitqueue;
      
      wrk->this = THIS;
      wrk->datalen = datalen;
      wrk->dataptr = datap;
      wrk->bufferaddr = bufptr[i]->buf;

      /* queue it at the end of buffer wait queue */
      while (bwq->forward != 0)
          bwq = bwq->forward;
      wrk->forward = bwq->forward;
      bwq->forward = wrk;
      
      /* queue it into list */
      wrk->startlist = start_list;
      if (! start_list)
        {
          start_list = wrk;
          prev_list_entry = wrk;
          wrk->startlist = start_list;
        }
      else
        {
          prev_list_entry->chain = wrk;
          prev_list_entry = wrk;
        }
      
      /* increment wait queue count */
      bq->waitqueuelength++;
    }

  /* tell runtime system to delay this process */
  timed_out = __delay_this (wait_buffer_receive, to, filename, lineno);
  if (timed_out)
    {
      /* remove all entries from buffer queues */
      Buffer_Wait_Queue *listentry = start_list;
      
      while (listentry != 0)
        {
	  Buffer_Queue *bq = *(listentry->bufferaddr);
	  Buffer_Wait_Queue *prev_entry = (Buffer_Wait_Queue *)&bq->waitqueue;
	  Buffer_Wait_Queue *bwq = bq->waitqueue;
	  
	  while (bwq != listentry)
	    {
	      prev_entry = bwq;
	      bwq = bwq->forward;
	    }
	  /* dequeue it */
	  prev_entry->forward = bwq->forward;
	  bq->waitqueuelength--;
	  listentry = listentry->chain;
        }
    }
  
  /* someone has continued us, find which buffer got ready */
  retval = 0;

  while (start_list != 0)
    {
      Buffer_Wait_Queue *tmp = start_list->chain;
      
      if (start_list->is_sent)
        {
          /* this one has been sent */
          /* save return value */
          if (retval == 0)
              retval = start_list->bufferaddr;
          else
              /* more then one has been sent, that's wrong */
              CAUSE_BUFFINCONS;
          
          /* update instance, if present */
          if (ins != 0)
            memcpy (ins, &start_list->who_sent, sizeof (INSTANCE));
        }
      FREE (start_list);
      start_list = tmp;
    }

  /* now check if there was really a buffer got */
  if (retval == 0 && !timed_out)
    /* something's totally wrong, raise an exception  */
    CAUSE_BUFFINCONS;

  if (!timed_out)
    *buf_got = (void *)retval;
  return timed_out;
}

/* force function __print_buffer to be linked */
extern void __print_buffer ();
static EntryPoint pev = __print_buffer;
