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

EXCEPTION (sendfail);

extern void __cause_ex1 (char *ex, char *file, int lineno);

#define CAUSE_SENDFAIL  __cause_ex1 ("sendfail", filename, lineno)

/*
 * function __send_buffer
 *
 * parameters:
 *     buffer     pointer to buffer descriptor
 *     data       pointer to data descriptor
 *     prio       priority for send action
 *     timeout	  pointer to timeout value
 *     filename   source file name where function gets called
 *     lineno     linenumber in source file
 *
 * returns:
 *     int	  0 .. success
 *                1 .. timeout
 *
 * exceptions:
 *     sendfail
 *
 * abstract:
 *     implement the CHILL SEND buffer action.
 */

int
__send_buffer (buffer, data, prio, timeout, filename, lineno)
     Buffer_Descr    *buffer;
     Data_Descr      *data;
     int              prio;
     void            *timeout;
     char            *filename;
     int              lineno;
{
  Buffer_Queue        *bq;
  Buffer_Send_Queue   *bsq, *bsq_entry, *prev_bsq_entry;
  int                  cnt = 0;
  int		       retval = 0;
  
  /* if we don't have anything queued on that buffer,
     set up the structure */
  memcpy (&bq, buffer->buf, sizeof (Buffer_Queue *));
  if (bq == 0)
    {
      MALLOC (bq, sizeof (Buffer_Queue));
      memset (bq, 0, sizeof (Buffer_Queue));
      memcpy (buffer->buf, &bq, sizeof (Buffer_Queue *));
    }

  /* look if there is a process delayed on that buffer */
  if (bq->waitqueue != 0)
    {
	Buffer_Wait_Queue	*listentry;
	
	/* there is already a processes waiting for that buffer,
	   check datalength and copy the data in */
	if (bq->waitqueue->datalen < data->length)
	    CAUSE_SENDFAIL;
	memcpy (bq->waitqueue->dataptr, data->ptr, data->length);
	
	/* set up the entry */
	bq->waitqueue->is_sent = 1;
	bq->waitqueue->who_sent = THIS;

	/* continue waiting process */
	__continue_that (bq->waitqueue->this, prio, filename, lineno);
	
	/* now dequeue all entries of this list */
	listentry = bq->waitqueue->startlist;
	while (listentry != 0)
	{
	    Buffer_Wait_Queue	*tmp, *prev_entry, *bwq;
	    Buffer_Queue	*bq;

	    tmp = listentry->chain;
	    memcpy (&bq, listentry->bufferaddr, sizeof (Buffer_Queue *));
	    prev_entry = (Buffer_Wait_Queue *)&bq->waitqueue;
	    bwq = bq->waitqueue;

	    while (bwq != listentry)
	    {
		prev_entry = bwq;
		bwq = bwq->forward;
	    }
	    /* dequeue it */
	    prev_entry->forward = bwq->forward;
	    bq->waitqueuelength--;
	    listentry = tmp;
	}
	
	/* all done */
	return 0;
    }

  /* nothing in waitqueue, set up an entry for sendqueue.
     Note: we allocate here space for the data too, to reduce
     calls to malloc and let the dataptr point just behind
     the Buffer_Send_Queue structure. */
  MALLOC (bsq_entry, sizeof (Buffer_Send_Queue) + data->length);
  memset (bsq_entry, 0, sizeof (Buffer_Send_Queue));

  bsq_entry->priority = prio;
  bsq_entry->this = THIS;
  bsq_entry->datalen = data->length;
  bsq_entry->dataptr = bsq_entry + 1;
  memcpy (bsq_entry->dataptr, data->ptr, data->length);

  /* add entry to sendqueue */
  prev_bsq_entry = (Buffer_Send_Queue *)&bq->sendqueue;
  bsq = bq->sendqueue;

  while (bsq != 0 && bsq->priority >= prio)
    {
      prev_bsq_entry = bsq;
      bsq = bsq->forward;
    }
  if (bsq == 0)
    {
      /* beginning or end of the list */
      prev_bsq_entry->forward = bsq_entry;
    }
  else
    {
      /* somewhere in the middle */
      bsq_entry->forward = prev_bsq_entry->forward;
      prev_bsq_entry->forward = bsq_entry;
    }

  if (buffer->maxqueuelength != (unsigned long)-1L &&
      bq->sendqueuelength >= buffer->maxqueuelength)
    {
      /* we have to delay this process */
      bsq_entry->is_delayed = 1;
      retval = __delay_this (wait_buffer_send, timeout, filename, lineno);
      if (retval)
        {
	  prev_bsq_entry->forward = bsq_entry->forward;
	  FREE (bsq_entry);
        }
    }
  else
    /* just say that there is one more entry in the queue */
    bq->sendqueuelength++;
  return retval;
}

/* force function __print_buffer to be linked */
extern void __print_buffer ();
static EntryPoint pev = __print_buffer;
