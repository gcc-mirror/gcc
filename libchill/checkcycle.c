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

EXCEPTION (cyclefail);

/*
 * function __check_cycle
 *
 * parameters:
 *      t         pointer to initial time
 *      dur       duration
 *      filename  filename of call
 *      lineno    linenumber of call
 *
 * returns:
 *      void
 *
 * exceptions:
 *      cyclefail
 *
 * abstract:
 *      Function checks if cycle is possible (there is time left) and wait the
 *      remaining time.
 *
 */

extern int __remaintime (RtsTime *since, unsigned long dur, RtsTime *remain);
extern int __cause_ex1 (char *ex, char *file, int lineno);
    
void
__check_cycle (t, dur, fname, lineno)
    RtsTime       *t;
    unsigned long  dur;
    char          *fname;
    int            lineno;
{
  RtsTime remain;
  
  if (__remaintime (t, dur, &remain) != 0)
    /* no time left -- cause exception */
    __cause_ex1 ("cyclefail", fname, lineno);
  
  /* delay the process */
  __delay_this (wait_wait, &remain, fname, lineno);
}
