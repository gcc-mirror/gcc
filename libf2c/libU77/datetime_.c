/* Copyright (C) 1997, 1998, 1999, 2001 Free Software Foundation, Inc.
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
#include <sys/types.h>
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
VOID s_copy ();
#else
void s_copy(register char *a, register char *b, ftnlen la, ftnlen lb);
#endif

int G77_date_and_time_0 (char *date, char *fftime, char *zone,
			 integer *values, ftnlen date_len,
			 ftnlen fftime_len, ftnlen zone_len)
{
  time_t lt=time(&lt);
  struct tm ltime = *localtime(&lt), gtime = *gmtime(&lt);
  char dat[9], zon[6], ftim[11];
  int i, vals[8];

  vals[0] = 1900 + ltime.tm_year;
  vals[1] = 1 + ltime.tm_mon;
  vals[2] = ltime.tm_mday;
  /* fixme: year boundaries */
  vals[3] = (ltime.tm_min - gtime.tm_min +
	     60*(ltime.tm_hour - gtime.tm_hour +
		 24*(ltime.tm_yday -gtime.tm_yday)));
  vals[4] = ltime.tm_hour;
  vals[5] = ltime.tm_min;
  vals[6] = ltime.tm_sec;
  vals[7] = 0;                  /* no STDC/POSIX way to get this */
  /* GNUish way; maybe use `ftime' on other systems. */
#if HAVE_GETTIMEOFDAY
  {
    struct timeval tp;
#  if GETTIMEOFDAY_ONE_ARGUMENT
    if (! gettimeofday (&tp))
#  else
#    if HAVE_STRUCT_TIMEZONE
    struct timezone tzp;
    /* Some systems such as HPUX, do have struct timezone, but
       gettimeofday takes void* as the 2nd arg.  However, the effect
       of passing anything other than a null pointer is unspecified on
       HPUX.  Configure checks if gettimeofday actually fails with a
       non-NULL arg and pretends that struct timezone is missing if it
       does fail.  */
    if (! gettimeofday (&tp, &tzp))
#    else
    if (! gettimeofday (&tp, (void *) 0))
#    endif /* HAVE_STRUCT_TIMEZONE */
#  endif /* GETTIMEOFDAY_ONE_ARGUMENT */
      vals[7] = tp.tv_usec/1000;
  }
#endif /* HAVE_GETTIMEOFDAY */
  if (values)			/* null pointer for missing optional */
    for (i=0; i<=7; i++)
      values[i] = vals[i];
  sprintf (dat, "%04d%02d%02d", vals[0], vals[1], vals[2]);
  s_copy(date, dat, date_len, 8);
  if (zone) {
    sprintf(zon, "%+03d%02d", vals[3] / 60, abs(vals[3] % 60));
    s_copy(zone, zon, zone_len, 5);
  }
  if (fftime) {
    sprintf (ftim, "%02d%02d%02d.%03d", vals[4], vals[5], vals[6], vals[7]);
    s_copy(fftime, ftim, fftime_len, 10);
  }
  return 0;
}
