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

#include "rtltypes.h"
#include "rts.h"

EXCEPTION (timerfail);

/*
 * function __wait_until
 *
 * parameters:
 *     abstime   absolute time value
 *     filename
 *     linenumber
 *
 * returns:
 *     int   0 on success, 1 on failure
 *
 * exceptions:
 *     timerfail
 *
 * abstract:
 *     check for given argument is valid, calculate how long to wait in
 *     seconds and call os to do it.
 *
 */

int
__wait_until (abstime, filename, linenumber)
     unsigned long  abstime;
     char          *filename;
     int            linenumber;
{
  RtsTime	now, delta, abs_rtstime;
    
  /* get current time */
  __rtstime (&now);
    
  abs_rtstime.secs = abstime;
  abs_rtstime.nanosecs = 0;
  
  if (abs_rtstime.nanosecs < now.nanosecs)
    {
      abs_rtstime.secs--;
      abs_rtstime.nanosecs += 1000000000;
    }
  
  delta.secs = abs_rtstime.secs - now.secs;
  delta.nanosecs = abs_rtstime.nanosecs - now.nanosecs;
  
  if (delta.secs > abs_rtstime.secs)
    /* cannot wait into past */
    return 1;
  
  return __delay_this (wait_wait, &delta, filename, linenumber) == 1 ? 0 : 1;
}
