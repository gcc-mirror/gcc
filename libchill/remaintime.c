/* Implement timing-related runtime actions for CHILL.
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

#include "rts.h"

/*
 * function __remaintime
 *
 * parameters:
 *      since   pointer to the  initial RtsTime
 *      dur     the duration value
 *      t       pointer to the remaining RtsTime
 *
 * returns:
 *      int     0 .. there is a remaining time
 *              1 .. there is no remaining time
 *
 * exceptions:
 *      none
 *
 * abstract:
 *      This function writes to t the remaining duration value in RtsTime format
 *      from a given start (since) and the current RtsTime.
 *
 */

extern void __convert_duration_rtstime (unsigned long dur, RtsTime *t);

int
  __remaintime (since, dur, t)
RtsTime       *since;
unsigned long  dur;
RtsTime       *t;
{
  RtsTime now, dur_in_rtstime, tmp, diff;
  
  __rtstime (&now);
  __convert_duration_rtstime (dur, &dur_in_rtstime);
  
  tmp.secs = since->secs;
  tmp.nanosecs = since->nanosecs;
  
  /* calculate the difference of absolute times */
  if (tmp.nanosecs > now.nanosecs)
    {
      tmp.secs--;
      tmp.nanosecs += 1000000000;
    }
  diff.secs = now.secs - tmp.secs;
  diff.nanosecs = now.nanosecs - tmp.nanosecs;
  
  /* substract diff from duration */
  if (diff.nanosecs > dur_in_rtstime.nanosecs)
    {
      dur_in_rtstime.secs--;
      dur_in_rtstime.nanosecs += 1000000000;
    }
  
  t->secs = dur_in_rtstime.secs - diff.secs;
  t->nanosecs = dur_in_rtstime.nanosecs - diff.nanosecs;
  
  if (t->secs > dur_in_rtstime.secs)
    return 1;
  else
    return 0;
}
