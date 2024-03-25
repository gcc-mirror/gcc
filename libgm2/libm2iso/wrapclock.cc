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

// Conditional inclusion of sys/time.h for gettimeofday
#if !defined(_GLIBCXX_USE_CLOCK_MONOTONIC) && \
    !defined(_GLIBCXX_USE_CLOCK_REALTIME) && \
     defined(_GLIBCXX_USE_GETTIMEOFDAY)
#include <sys/time.h>
#endif

#if defined(_GLIBCXX_USE_CLOCK_GETTIME_SYSCALL)
#include <unistd.h>
#include <sys/syscall.h>
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

typedef long long int longint_t;


/* GetTimeRealtime performs return gettime (CLOCK_REALTIME, ts).
   gettime returns 0 on success and -1 on failure.  If the underlying
   system does not have gettime then GetTimeRealtime returns 1.  */

#if defined(HAVE_STRUCT_TIMESPEC) && defined(_GLIBCXX_USE_CLOCK_REALTIME)
extern "C" int
EXPORT(GetTimeRealtime) (struct timespec *ts)
{
  timespec tp;
#if defined(_GLIBCXX_USE_CLOCK_GETTIME_SYSCALL)
  return syscall (SYS_clock_gettime, CLOCK_REALTIME, ts);
#else
  return clock_gettime (CLOCK_REALTIME, ts);
#endif
}

#else

extern "C" int
EXPORT(GetTimeRealtime) (void *ts)
{
  return 1;
}
#endif

/* SetTimeRealtime performs return settime (CLOCK_REALTIME, ts).
   gettime returns 0 on success and -1 on failure.  If the underlying
   system does not have gettime then GetTimeRealtime returns 1.  */

#if defined(HAVE_STRUCT_TIMESPEC) && defined(_GLIBCXX_USE_CLOCK_REALTIME)
extern "C" int
EXPORT(SetTimeRealtime) (struct timespec *ts)
{
#if defined(_GLIBCXX_USE_CLOCK_SETTIME_SYSCALL)
  return syscall (SYS_clock_settime, CLOCK_REALTIME, ts);
#elif defined(HAVE_CLOCK_SETTIME)
  return clock_settime (CLOCK_REALTIME, ts);
#else
  return 1;
#endif
}

#else

extern "C" int
EXPORT(SetTimeRealtime) (void *ts)
{
  return 1;
}
#endif

/* InitTimespec returns a newly created opaque type.  */

#if defined(HAVE_STRUCT_TIMESPEC)
extern "C" struct timespec *
EXPORT(InitTimespec) (void)
{
#if defined(HAVE_STRUCT_TIMESPEC) && defined(HAVE_MALLOC_H)
  return (struct timespec *)malloc (sizeof (struct timespec));
#else
  return NULL;
#endif
}

#else

extern "C" void *
EXPORT(InitTimespec) (void)
{
  return NULL;
}
#endif

/* KillTimeval deallocates the memory associated with an opaque type.  */

#if defined(HAVE_STRUCT_TIMESPEC)
extern "C" struct timespec *
EXPORT(KillTimespec) (void *ts)
{
#if defined(HAVE_MALLOC_H)
  free (ts);
#endif
  return NULL;
}

#else

extern "C" void *
EXPORT(KillTimespec) (void *ts)
{
  return NULL;
}
#endif

/* GetTimespec retrieves the number of seconds and nanoseconds from the
   timespec.  1 is returned if successful and 0 otherwise.  */

#if defined(HAVE_STRUCT_TIMESPEC)
extern "C" int
EXPORT(GetTimespec) (timespec *ts, longint_t *sec, longint_t *nano)
{
#if defined(HAVE_STRUCT_TIMESPEC)
  *sec = ts->tv_sec;
  *nano = ts->tv_nsec;
  return 1;
#else
  return 0;
#endif
}

#else
extern "C" int
EXPORT(GetTimespec) (void *ts, longint_t *sec, longint_t *nano)
{
  return 0;
}
#endif

/* SetTimespec sets the number of seconds and nanoseconds into timespec.
   1 is returned if successful and 0 otherwise.  */

#if defined(HAVE_STRUCT_TIMESPEC)
extern "C" int
EXPORT(SetTimespec) (timespec *ts, longint_t sec, longint_t nano)
{
#if defined(HAVE_STRUCT_TIMESPEC)
  ts->tv_sec = sec;
  ts->tv_nsec = nano;
  return 1;
#else
  return 0;
#endif
}

#else

extern "C" int
EXPORT(SetTimespec) (void *ts, longint_t sec, longint_t nano)
{
  return 0;
}
#endif

extern "C" longint_t
EXPORT(timezone) (void)
{
#if defined(HAVE_STRUCT_TIMESPEC)
  struct tm result;
  struct timespec ts;

#if defined(HAVE_TM_TM_GMTOFF)
  if (EXPORT(GetTimeRealtime) (&ts) == 0)
    {
      time_t time = ts.tv_sec;
      localtime_r (&time, &result);
      return result.tm_gmtoff;
    }
#endif
#endif
  return 0;
}

/* istimezone returns 1 if timezone in wrapclock.cc can resolve the
   timezone value using the timezone C library call or by using
   clock_gettime, localtime_r and tm_gmtoff.  */

extern "C" int
EXPORT(istimezone) (void)
{
#if defined(HAVE_STRUCT_TIMESPEC)
#if defined(HAVE_TM_TM_GMTOFF)
#if defined(_GLIBCXX_USE_CLOCK_REALTIME)
  return 1;
#endif
#endif
#endif
  return 0;
}

extern "C" int
EXPORT(daylight) (void)
{
#if defined(HAVE_DAYLIGHT)
  return daylight;
#else
  return 0;
#endif
}

/* isdst returns 1 if daylight saving time is currently in effect and
   returns 0 if it is not.  */

extern "C" int
EXPORT(isdst) (void)
{
#if defined(HAVE_STRUCT_TIMESPEC)
  struct tm result;
  struct timespec ts;

  if (EXPORT(GetTimeRealtime) (&ts) == 0)
    {
      time_t time = ts.tv_sec;
      localtime_r (&time, &result);
      return result.tm_isdst;
    }
  else
    return 0;
#else
  return 0;
#endif
}

/* tzname returns the string associated with the local timezone.
   The daylight value is 0 or 1.  The value 0 returns the non
   daylight saving timezone string and the value of 1 returns the
   daylight saving timezone string.  It returns NULL if tzname is
   unavailable.  */

extern "C" char *
EXPORT(tzname) (int daylight)
{
#if defined(HAVE_TZNAME)
  return tzname[daylight];
#else
  return NULL;
#endif
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
