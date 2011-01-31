/* Implementation of the SYSTEM_CLOCK intrinsic.
   Copyright (C) 2004, 2005, 2007, 2009, 2010, 2011 Free Software
   Foundation, Inc.

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgfortran.h"

#include <limits.h>

#include "time_1.h"

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
#undef TCK
#define TCK 1000
  GFC_INTEGER_4 cnt;
  GFC_INTEGER_4 mx;

  time_t secs;
  long nanosecs;

  if (sizeof (secs) < sizeof (GFC_INTEGER_4))
    internal_error (NULL, "secs too small");

  if (gf_gettime (GF_CLOCK_MONOTONIC, &secs, &nanosecs) == 0)
    {
      GFC_UINTEGER_4 ucnt = (GFC_UINTEGER_4) secs * TCK;
      ucnt += (nanosecs + 500000000 / TCK) / (1000000000 / TCK);
      if (ucnt > GFC_INTEGER_4_HUGE)
	cnt = ucnt - GFC_INTEGER_4_HUGE - 1;
      else
	cnt = ucnt;
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
#undef TCK
#define TCK 1000000000
  GFC_INTEGER_8 cnt;
  GFC_INTEGER_8 mx;

  time_t secs;
  long nanosecs;

  if (sizeof (secs) < sizeof (GFC_INTEGER_4))
    internal_error (NULL, "secs too small");

  if (gf_gettime (GF_CLOCK_MONOTONIC, &secs, &nanosecs) == 0)
    {
      GFC_UINTEGER_8 ucnt = (GFC_UINTEGER_8) secs * TCK;
      ucnt += (nanosecs + 500000000 / TCK) / (1000000000 / TCK);
      if (ucnt > GFC_INTEGER_8_HUGE)
	cnt = ucnt - GFC_INTEGER_8_HUGE - 1;
      else
	cnt = ucnt;
      mx = GFC_INTEGER_8_HUGE;
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

  if (count != NULL)
    *count = cnt;
  if (count_rate != NULL)
    *count_rate = TCK;
  if (count_max != NULL)
    *count_max = mx;
}
