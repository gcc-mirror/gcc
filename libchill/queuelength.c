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
 * function __queue_length
 *
 * parameters:
 *     buf_ev      Buffer or event location
 *     is_event    0 .. buf_ev is a buffer location
 *                 1 .. buf_ev is an event location
 *
 * returns:
 *     int         number of delayed processeson an event location
 *                 or number of send delayed processes on a buffer
 *
 * exceptions:
 *     none
 *
 * abstract:
 *     implements the QUEUE_LENGTH built-in.
 *
 */

int
__queue_length (buf_ev, is_event)
     void  *buf_ev;
     int    is_event;
{
  int            retval = 0;
  
  /* if buf_ev == 0 then we don't have anything */
  if (buf_ev == 0)
    return 0;

  if (is_event)
    {
      /* process an event queue */
      Event_Queue   *ev = buf_ev;

      while (ev)
	{
	  retval++;
	  ev = ev->forward;
	}
    }
  else
    {
      /* process a buffer queue */
      Buffer_Queue *bq = buf_ev;
      Buffer_Send_Queue *bsq = bq->sendqueue;

      while (bsq)
	{
	  retval++;
	  bsq = bsq->forward;
	}
    }
  return retval;
}
