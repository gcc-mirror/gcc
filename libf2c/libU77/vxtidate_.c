/* Copyright (C) 1995, 1996 Free Software Foundation, Inc.
This file is part of GNU Fortran libU77 library.

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with GNU Fortran; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <stdio.h>
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
#include "f2c.h"

/* VMS and Irix versions (at least) differ from libU77 elsewhere */

/* VMS style: */

/* Subroutine */
#ifdef KR_headers
int G77_vxtidate_y2kbug_0 (m, d, y)
     integer *y, *m, *d;
#else
int G77_vxtidate_y2kbug_0 (integer *m, integer *d, integer *y)
#endif
{
  struct tm *lt;
  time_t tim;
  tim = time(NULL);
  lt = localtime(&tim);
  *y = lt->tm_year % 100;
  *m = lt->tm_mon+1;
  *d = lt->tm_mday;
  return 0;
}
