/* wrapclock.cc provides access to time related system calls.

Copyright (C) 2009-2022 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include <m2rts.h>

#define EXPORT(FUNC) m2iso ## _wrapclock_ ## FUNC
#define M2EXPORT(FUNC) m2iso ## _M2_wrapclock_ ## FUNC
#define M2LIBNAME "m2iso"

#if defined(HAVE_STDLIB_H)
#include "stdlib.h"
#endif

#if defined(HAVE_UNISTD_H)
#include "unistd.h"
#endif

#if defined(HAVE_SYS_TYPES_H)
#include "sys/types.h"
#endif

#if defined(HAVE_SYS_TIME_H)
#include "sys/time.h"
#endif

#if defined(HAVE_TIME_H)
#include "time.h"
#endif

#if defined(HAVE_MALLOC_H)
#include "malloc.h"
#endif

#if defined(HAVE_LIMITS_H)
#include "limits.h"
#endif

#if !defined(NULL)
#define NULL (void *)0
#endif


extern "C" long int
EXPORT(timezone) (void)
{
  struct tm result;
  struct timespec ts;

  if (clock_gettime (CLOCK_REALTIME, &ts) == 0)
    {
      time_t time = ts.tv_sec;
      localtime_r (&time, &result);
      return result.tm_gmtoff;
    }
  else
    return timezone;
}


extern "C" int
EXPORT(daylight) (void)
{
  return daylight;
}


/* isdst returns 1 if daylight saving time is currently in effect and
   returns 0 if it is not.  */

extern "C" int
EXPORT(isdst) (void)
{
  struct tm result;
  struct timespec ts;

  if (clock_gettime (CLOCK_REALTIME, &ts) == 0)
    {
      time_t time = ts.tv_sec;
      localtime_r (&time, &result);
      return result.tm_isdst;
    }
  else
    return 0;
}


/* tzname returns the string associated with the local timezone.
   The daylight value is 0 or 1.  The value 0 returns the non
   daylight saving timezone string and the value of 1 returns the
   daylight saving timezone string.  */

extern "C" char *
EXPORT(tzname) (int daylight)
{
  return tzname[daylight];
}


/* GetTimeRealtime performs return gettime (CLOCK_REALTIME, ts).
   gettime returns 0 on success and -1 on failure.  If the underlying
   system does not have gettime then GetTimeRealtime returns 1.  */

extern "C" int
EXPORT(GetTimeRealtime) (struct timespec *ts)
{
#if defined(HAVE_CLOCK_GETTIME)
  return clock_gettime (CLOCK_REALTIME, ts);
#else
  return 1;
#endif
}


/* SetTimeRealtime performs return settime (CLOCK_REALTIME, ts).
   gettime returns 0 on success and -1 on failure.  If the underlying
   system does not have gettime then GetTimeRealtime returns 1.  */

extern "C" int
EXPORT(SetTimeRealtime) (struct timespec *ts)
{
#if defined(HAVE_CLOCK_GETTIME)
  return clock_settime (CLOCK_REALTIME, ts);
#else
  return 1;
#endif
}


/* InitTimespec returns a newly created opaque type.  */

extern "C" struct timespec *
EXPORT(InitTimespec) (void)
{
  return (struct timespec *)malloc (sizeof (struct timespec));
}


/* KillTimeval deallocates the memory associated with an opaque type.  */

extern "C" struct timespec *
EXPORT(KillTimespec) (void *ts)
{
#if defined(HAVE_MALLOC_H)
  free (ts);
#endif
  return NULL;
}


/* GetTimespec retrieves the number of seconds and nanoseconds from the
   timespec.  */

extern "C" void
EXPORT(GetTimespec) (timespec *ts, unsigned long *sec, unsigned long *nano)
{
  *sec = ts->tv_sec;
  *nano = ts->tv_nsec;
}


/* SetTimespec sets the number of seconds and nanoseconds into timespec.  */

extern "C" void
EXPORT(SetTimespec) (timespec *ts, unsigned long sec, unsigned long nano)
{
  ts->tv_sec = sec;
  ts->tv_nsec = nano;
}


/* init - init/finish functions for the module */

/* GNU Modula-2 linking hooks.  */

extern "C" void
M2EXPORT(init) (int, char **, char **)
{
}

extern "C" void
M2EXPORT(fini) (int, char **, char **)
{
}

extern "C" void
M2EXPORT(dep) (void)
{
}

extern "C" void __attribute__((__constructor__))
M2EXPORT(ctor) (void)
{
  m2iso_M2RTS_RegisterModule ("wrapclock", M2LIBNAME,
			      M2EXPORT(init), M2EXPORT(fini),
			      M2EXPORT(dep));
}
