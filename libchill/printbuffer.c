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

#include <stdio.h>
#include "rtltypes.h"
#include "rts.h"

typedef char *(*fetch_names) (int number);
extern fetch_names	__RTS_FETCH_NAMES__;

/*
 * function print_instance
 *
 */

static char *print_instance (ins)
     INSTANCE ins;
{
  static char  buf[256];
  char *f;

  if (!__RTS_FETCH_NAMES__)
    f = 0;
  else
    f = (*__RTS_FETCH_NAMES__) (ins.ptype);
  if (!f)
    sprintf (buf, "[%u;%u]", ins.ptype, ins.pcopy);
  else
    sprintf (buf, "[%s;%u]", f, ins.pcopy);
  return buf;
}

/*
 * function __print_buffer
 *
 * parameters:
 *     buffer      buffer location
 *
 * returns:
 *     void
 *
 * exceptions:
 *     none
 *
 * abstract:
 *     Function is used for debugging purposes only to print a
 *     buffer queue
 */

void
__print_buffer (buffer, name)
     Buffer_Queue   **buffer;
     char            *name;
{
  Buffer_Queue      *bq;
  int                bsqcnt = 0, bwqcnt = 0;
  Buffer_Send_Queue *bsq;
  Buffer_Wait_Queue *bwq;

  if (name)
    printf ("Buffer %s:\n", name);
  else
    printf ("Buffer at address H'%X:\n", buffer);

  memcpy (&bq, buffer, sizeof (Buffer_Queue *));
  if (bq == 0)
    {
      printf ("EMPTY\n");
      return;
    }

  bsq = bq->sendqueue;
  if (bsq != 0)
      printf ("Send Queue:\n");
  while (bsq)
    {
      printf (" %3d: ", ++bsqcnt);
      printf ("Process %s, ", print_instance (bsq->this));
      printf ("Priority %d", bsq->priority);
      if (bsq->is_delayed)
	printf (", Delayed");
      printf ("\n");
      bsq = bsq->forward;
    }
  bwq = bq->waitqueue;
  if (bwq != 0)
      printf ("Wait Queue:\n");
  while (bwq)
    {
      printf (" %3d: ", ++bwqcnt);
      printf ("Process %s, ", print_instance (bwq->this));
      if (bwq->is_sent)
	printf (", Send by %s", print_instance (bwq->who_sent));
      printf ("\n");
      bwq = bwq->forward;
    }
  if (bsqcnt == 0 && bwqcnt == 0)
    printf ("EMPTY\n");
}
