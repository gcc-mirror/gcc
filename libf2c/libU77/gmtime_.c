/* Copyright (C) 1995 Free Software Foundation, Inc.
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
/* fixme: do we need to use TM_IN_SYS_TIME? */
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

#ifdef KR_headers
/* Subroutine */ int G77_gmtime_0 (xstime, tarray)
     integer *xstime, tarray[9];
#else
/* Subroutine */ int G77_gmtime_0 (const integer * xstime, integer tarray[9])
#endif
{
  struct tm *lt;
  time_t stime = *xstime;
  lt = gmtime (&stime);
  tarray[0] = lt->tm_sec;
  tarray[1] = lt->tm_min;
  tarray[2] = lt->tm_hour;
  tarray[3] = lt->tm_mday;
  tarray[4] = lt->tm_mon;
  tarray[5] = lt->tm_year;
  tarray[6] = lt->tm_wday;
  tarray[7] = lt->tm_yday;
  tarray[8] = lt->tm_isdst;
  return 0;
}
