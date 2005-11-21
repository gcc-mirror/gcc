/* Implementation of the SYSTEM_CLOCK intrinsic.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include <sys/types.h>
#include "libgfortran.h"

#include <limits.h>

#if defined(HAVE_SYS_TIME_H) && defined(HAVE_GETTIMEOFDAY)
#  include <sys/time.h>
#  define TCK 1000
#elif defined(HAVE_TIME_H)
#  include <time.h>
#  define TCK 1
#else
#define TCK 0
#endif


extern void system_clock_4 (GFC_INTEGER_4 *, GFC_INTEGER_4 *, GFC_INTEGER_4 *);
export_proto(system_clock_4);

extern void system_clock_8 (GFC_INTEGER_8 *, GFC_INTEGER_8 *, GFC_INTEGER_8 *);
export_proto(system_clock_8);


/* prefix(system_clock_4) is the INTEGER(4) version of the SYSTEM_CLOCK
   intrinsic subroutine.  It returns the number of clock ticks for the current
   system time, the number of ticks per second, and the maximum possible value
   for COUNT.  On the first call to SYSTEM_CLOCK, COUNT is set to zero. */

void
system_clock_4(GFC_INTEGER_4 *count, GFC_INTEGER_4 *count_rate,
	       GFC_INTEGER_4 *count_max)
{
  GFC_INTEGER_4 cnt;
  GFC_INTEGER_4 rate;
  GFC_INTEGER_4 mx;

#if defined(HAVE_SYS_TIME_H) && defined(HAVE_GETTIMEOFDAY)
  struct timeval tp1;
  struct timezone tzp;

  if (sizeof (tp1.tv_sec) < sizeof (GFC_INTEGER_4))
    internal_error (NULL, "tv_sec too small");

  if (gettimeofday(&tp1, &tzp) == 0)
    {
      GFC_UINTEGER_4 ucnt = (GFC_UINTEGER_4) tp1.tv_sec * TCK;
      ucnt += (tp1.tv_usec + 500000 / TCK) / (1000000 / TCK);
      if (ucnt > GFC_INTEGER_4_HUGE)
	cnt = ucnt - GFC_INTEGER_4_HUGE - 1;
      else
	cnt = ucnt;
      rate = TCK;
      mx = GFC_INTEGER_4_HUGE;
    }
  else
    {
      if (count != NULL)
	*count = - GFC_INTEGER_4_HUGE;
      if (count_rate != NULL)
	*count_rate = 0;
      if (count_max != NULL)
	*count_max = 0;
      return;
    }
#elif defined(HAVE_TIME_H)
  GFC_UINTEGER_4 ucnt;

  if (sizeof (time_t) < sizeof (GFC_INTEGER_4))
    internal_error (NULL, "time_t too small");

  ucnt = time (NULL);
  if (ucnt > GFC_INTEGER_4_HUGE)
    cnt = ucnt - GFC_INTEGER_4_HUGE - 1;
  else
    cnt = ucnt;
  mx = GFC_INTEGER_4_HUGE;
#else
  cnt = - GFC_INTEGER_4_HUGE;
  mx = 0;
#endif
  if (count != NULL)
    *count = cnt;
  if (count_rate != NULL)
    *count_rate = TCK;
  if (count_max != NULL)
    *count_max = mx;
}


/* INTEGER(8) version of the above routine.  */

void
system_clock_8 (GFC_INTEGER_8 *count, GFC_INTEGER_8 *count_rate,
		GFC_INTEGER_8 *count_max)
{
  GFC_INTEGER_8 cnt;
  GFC_INTEGER_8 rate;
  GFC_INTEGER_8 mx;

#if defined(HAVE_SYS_TIME_H) && defined(HAVE_GETTIMEOFDAY)
  struct timeval tp1;
  struct timezone tzp;

  if (sizeof (tp1.tv_sec) < sizeof (GFC_INTEGER_4))
    internal_error (NULL, "tv_sec too small");

  if (gettimeofday(&tp1, &tzp) == 0)
    {
      if (sizeof (tp1.tv_sec) < sizeof (GFC_INTEGER_8))
	{
	  GFC_UINTEGER_4 ucnt = (GFC_UINTEGER_4) tp1.tv_sec * TCK;
	  ucnt += (tp1.tv_usec + 500000 / TCK) / (1000000 / TCK);
	  if (ucnt > GFC_INTEGER_4_HUGE)
	    cnt = ucnt - GFC_INTEGER_4_HUGE - 1;
	  else
	    cnt = ucnt;
	  mx = GFC_INTEGER_4_HUGE;
	}
      else
	{
	  GFC_UINTEGER_8 ucnt = (GFC_UINTEGER_8) tp1.tv_sec * TCK;
	  ucnt += (tp1.tv_usec + 500000 / TCK) / (1000000 / TCK);
	  if (ucnt > GFC_INTEGER_8_HUGE)
	    cnt = ucnt - GFC_INTEGER_8_HUGE - 1;
	  else
	    cnt = ucnt;
	  mx = GFC_INTEGER_8_HUGE;
	}
      rate = TCK;
    }
  else
    {
      if (count != NULL)
	*count = - GFC_INTEGER_8_HUGE;
      if (count_rate != NULL)
	*count_rate = 0;
      if (count_max != NULL)
	*count_max = 0;

      return;
    }
#elif defined(HAVE_TIME_H)
  if (sizeof (time_t) < sizeof (GFC_INTEGER_4))
    internal_error (NULL, "time_t too small");
  else if (sizeof (time_t) == sizeof (GFC_INTEGER_4))
    {
      GFC_UINTEGER_4 ucnt = time (NULL);
      if (ucnt > GFC_INTEGER_4_HUGE)
	cnt = ucnt - GFC_INTEGER_4_HUGE - 1;
      else
	cnt = ucnt;
      mx = GFC_INTEGER_4_HUGE;
    }
  else
    {
      GFC_UINTEGER_8 ucnt = time (NULL);
      if (ucnt > GFC_INTEGER_8_HUGE)
	cnt = ucnt - GFC_INTEGER_8_HUGE - 1;
      else
	cnt = ucnt;
      mx = GFC_INTEGER_8_HUGE;
    }
#else
  cnt = - GFC_INTEGER_8_HUGE;
  mx = 0;
#endif
  if (count != NULL)
    *count = cnt;
  if (count_rate != NULL)
    *count_rate = TCK;
  if (count_max != NULL)
    *count_max = mx;
}
