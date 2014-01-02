/* Implementation of the SYSTEM_CLOCK intrinsic.
   Copyright (C) 2004-2014 Free Software Foundation, Inc.

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


#if !defined(__MINGW32__) && !defined(__CYGWIN__)

/* POSIX states that CLOCK_REALTIME must be present if clock_gettime
   is available, others are optional.  */
#if defined(HAVE_CLOCK_GETTIME) || defined(HAVE_CLOCK_GETTIME_LIBRT)
#if defined(CLOCK_MONOTONIC) && defined(_POSIX_MONOTONIC_CLOCK) \
  && _POSIX_MONOTONIC_CLOCK >= 0
#define GF_CLOCK_MONOTONIC CLOCK_MONOTONIC
#else
#define GF_CLOCK_MONOTONIC CLOCK_REALTIME
#endif
#endif

/* Weakref trickery for clock_gettime().  On Glibc <= 2.16,
   clock_gettime() requires us to link in librt, which also pulls in
   libpthread.  In order to avoid this by default, only call
   clock_gettime() through a weak reference.

   Some targets don't support weak undefined references; on these
   GTHREAD_USE_WEAK is 0. So we need to define it to 1 on other
   targets.  */
#ifndef GTHREAD_USE_WEAK
#define GTHREAD_USE_WEAK 1
#endif

#if SUPPORTS_WEAK && GTHREAD_USE_WEAK && defined(HAVE_CLOCK_GETTIME_LIBRT)
static int weak_gettime (clockid_t, struct timespec *) 
  __attribute__((__weakref__("clock_gettime")));
#endif


/* High resolution monotonic clock, falling back to the realtime clock
   if the target does not support such a clock.

   Arguments:
   secs     - OUTPUT, seconds
   fracsecs - OUTPUT, fractional seconds, units given by tk argument
   tk       - OUTPUT, clock resolution [counts/sec]

   If the target supports a monotonic clock, the OUTPUT arguments
   represent a monotonically incrementing clock starting from some
   unspecified time in the past.

   If a monotonic clock is not available, falls back to the realtime
   clock which is not monotonic.

   Return value: 0 for success, -1 for error. In case of error, errno
   is set.
*/
static int
gf_gettime_mono (time_t * secs, long * fracsecs, long * tck)
{
  int err;
#ifdef HAVE_CLOCK_GETTIME
  struct timespec ts;
  *tck = 1000000000;
  err = clock_gettime (GF_CLOCK_MONOTONIC, &ts);
  *secs = ts.tv_sec;
  *fracsecs = ts.tv_nsec;
  return err;
#else
#if defined(HAVE_CLOCK_GETTIME_LIBRT) && SUPPORTS_WEAK && GTHREAD_USE_WEAK
  if (weak_gettime)
    {
      struct timespec ts;
      *tck = 1000000000;
      err = weak_gettime (GF_CLOCK_MONOTONIC, &ts);
      *secs = ts.tv_sec;
      *fracsecs = ts.tv_nsec;
      return err;
    }
#endif
  *tck = 1000000;
  err = gf_gettime (secs, fracsecs);
  return err;
#endif
}

#endif /* !__MINGW32 && !__CYGWIN__  */

extern void system_clock_4 (GFC_INTEGER_4 *, GFC_INTEGER_4 *, GFC_INTEGER_4 *);
export_proto(system_clock_4);

extern void system_clock_8 (GFC_INTEGER_8 *, GFC_INTEGER_8 *, GFC_INTEGER_8 *);
export_proto(system_clock_8);


/* prefix(system_clock_4) is the INTEGER(4) version of the SYSTEM_CLOCK
   intrinsic subroutine.  It returns the number of clock ticks for the current
   system time, the number of ticks per second, and the maximum possible value
   for COUNT.  */

void
system_clock_4(GFC_INTEGER_4 *count, GFC_INTEGER_4 *count_rate,
	       GFC_INTEGER_4 *count_max)
{
#if defined(__MINGW32__) || defined(__CYGWIN__) 
  if (count)
    {
      /* Use GetTickCount here as the resolution and range is
	 sufficient for the INTEGER(kind=4) version, and
	 QueryPerformanceCounter has potential issues.  */
      uint32_t cnt = GetTickCount ();
      if (cnt > GFC_INTEGER_4_HUGE)
	cnt = cnt - GFC_INTEGER_4_HUGE - 1;
      *count = cnt;
    }
  if (count_rate)
    *count_rate = 1000;
  if (count_max)
    *count_max = GFC_INTEGER_4_HUGE;
#else
  time_t secs;
  long fracsecs, tck;

  if (gf_gettime_mono (&secs, &fracsecs, &tck) == 0)
    {
      long tck_out = tck > 1000 ? 1000 : tck;
      long tck_r = tck / tck_out;
      GFC_UINTEGER_4 ucnt = (GFC_UINTEGER_4) secs * tck_out;
      ucnt += fracsecs / tck_r;
      if (ucnt > GFC_INTEGER_4_HUGE)
	ucnt = ucnt - GFC_INTEGER_4_HUGE - 1;
      if (count)
	*count = ucnt;
      if (count_rate)
	*count_rate = tck_out;
      if (count_max)
	*count_max = GFC_INTEGER_4_HUGE;
    }
  else
    {
      if (count)
	*count = - GFC_INTEGER_4_HUGE;
      if (count_rate)
	*count_rate = 0;
      if (count_max)
	*count_max = 0;
    }
#endif
}


/* INTEGER(8) version of the above routine.  */

void
system_clock_8 (GFC_INTEGER_8 *count, GFC_INTEGER_8 *count_rate,
		GFC_INTEGER_8 *count_max)
{
#if defined(__MINGW32__) || defined(__CYGWIN__) 
  LARGE_INTEGER cnt;
  LARGE_INTEGER freq;
  bool fail = false;
  if (count && !QueryPerformanceCounter (&cnt))
    fail = true;
  if (count_rate && !QueryPerformanceFrequency (&freq))
    fail = true;
  if (fail)
    {
      if (count)
	*count = - GFC_INTEGER_8_HUGE;
      if (count_rate)
	*count_rate = 0;
      if (count_max)
	*count_max = 0;
    }
  else
    {
      if (count)
	*count = cnt.QuadPart;
      if (count_rate)
	*count_rate = freq.QuadPart;
      if (count_max)
	*count_max = GFC_INTEGER_8_HUGE;
    }
#else
  time_t secs;
  long fracsecs, tck;

  if (gf_gettime_mono (&secs, &fracsecs, &tck) == 0)
    {
      GFC_UINTEGER_8 ucnt = (GFC_UINTEGER_8) secs * tck;
      ucnt += fracsecs;
      if (ucnt > GFC_INTEGER_8_HUGE)
	ucnt = ucnt - GFC_INTEGER_8_HUGE - 1;
      if (count)
	*count = ucnt;
      if (count_rate)
	*count_rate = tck;
      if (count_max)
	*count_max = GFC_INTEGER_8_HUGE;
    }
  else
    {
      if (count)
	*count = - GFC_INTEGER_8_HUGE;
      if (count_rate)
	*count_rate = 0;
      if (count_max)
	*count_max = 0;
    }
#endif
}
