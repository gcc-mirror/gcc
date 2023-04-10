/* wraptime.c provides access to time related system calls.

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

#define EXPORT(FUNC) m2iso ## _wraptime_ ## FUNC
#define M2EXPORT(FUNC) m2iso ## _M2_wraptime_ ## FUNC
#define M2LIBNAME "m2iso"

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

/* InitTimeval returns a newly created opaque type.  */

#if defined(HAVE_TIMEVAL) && defined(HAVE_MALLOC_H)
extern "C" struct timeval *
EXPORT(InitTimeval) (void)
{
  return (struct timeval *)malloc (sizeof (struct timeval));
}
#else
extern "C" void *
EXPORT(InitTimeval) (void)
{
  return NULL;
}
#endif

/* KillTimeval deallocates the memory associated with an opaque type.  */

extern "C" struct timeval *
EXPORT(KillTimeval) (void *tv)
{
#if defined(HAVE_MALLOC_H)
  free (tv);
#endif
  return NULL;
}

/* InitTimezone returns a newly created opaque type.  */

#if defined(HAVE_STRUCT_TIMEZONE) && defined(HAVE_MALLOC_H)
extern "C" struct timezone *
EXPORT(InitTimezone) (void)
{
  return (struct timezone *)malloc (sizeof (struct timezone));
}
#else
extern "C" void *
EXPORT(InitTimezone) (void)
{
  return NULL;
}
#endif

/* KillTimezone - deallocates the memory associated with an opaque
   type.  */

extern "C" struct timezone *
EXPORT(KillTimezone) (struct timezone *tv)
{
#if defined(HAVE_MALLOC_H)
  free (tv);
#endif
  return NULL;
}

/* InitTM - returns a newly created opaque type.  */

#if defined(HAVE_STRUCT_TM) && defined(HAVE_MALLOC_H)
extern "C" struct tm *
EXPORT(InitTM) (void)
{
  return (struct tm *)malloc (sizeof (struct tm));
}
#else
extern "C" void *
EXPORT(InitTM) (void)
{
  return NULL;
}
#endif

/* KillTM - deallocates the memory associated with an opaque type.  */

extern "C" struct tm *
EXPORT(KillTM) (struct tm *tv)
{
#if defined(HAVE_MALLOC_H)
  free (tv);
#endif
  return NULL;
}

/* gettimeofday - calls gettimeofday(2) with the same parameters, tv,
   and, tz.  It returns 0 on success.  */

#if defined(HAVE_STRUCT_TIMEZONE) && defined(HAVE_GETTIMEOFDAY)
extern "C" int
EXPORT(gettimeofday) (void *tv, struct timezone *tz)
{
  return gettimeofday (tv, tz);
}
#else
extern "C" int
EXPORT(gettimeofday) (void *tv, void *tz)
{
  return -1;
}
#endif

/* settimeofday - calls settimeofday(2) with the same parameters, tv,
   and, tz.  It returns 0 on success.  */

#if defined(HAVE_STRUCT_TIMEZONE) && defined(HAVE_SETTIMEOFDAY)
extern "C" int
EXPORT(settimeofday) (void *tv, struct timezone *tz)
{
  return settimeofday (tv, tz);
}
#else
extern "C" int
EXPORT(settimeofday) (void *tv, void *tz)
{
  return -1;
}
#endif

/* wraptime_GetFractions - returns the tv_usec field inside the
   timeval structure.  */

#if defined(HAVE_TIMEVAL)
extern "C" unsigned int
EXPORT(GetFractions) (struct timeval *tv)
{
  return (unsigned int)tv->tv_usec;
}
#else
extern "C" unsigned int
EXPORT(GetFractions) (void *tv)
{
  return (unsigned int)-1;
}
#endif

/* localtime_r - returns the tm parameter, m, after it has been
   assigned with appropriate contents determined by, tv.  Notice that
   this procedure function expects, timeval, as its first parameter
   and not a time_t (as expected by the posix equivalent).  */

#if defined(HAVE_TIMEVAL)
extern "C" struct tm *
EXPORT(localtime_r) (struct timeval *tv, struct tm *m)
{
  return localtime_r (&tv->tv_sec, m);
}
#else
extern "C" struct tm *
EXPORT(localtime_r) (void *tv, struct tm *m)
{
  return m;
}
#endif

/* wraptime_GetYear - returns the year from the structure, m.  */

#if defined(HAVE_STRUCT_TM)
extern "C" unsigned int
EXPORT(GetYear) (struct tm *m)
{
  return m->tm_year;
}
#else
extern "C" unsigned int
EXPORT(GetYear) (void *m)
{
  return (unsigned int)-1;
}
#endif

/* wraptime_GetMonth - returns the month from the structure, m.  */

#if defined(HAVE_STRUCT_TM)
extern "C" unsigned int
EXPORT(GetMonth) (struct tm *m)
{
  return m->tm_mon;
}
#else
extern "C" unsigned int
EXPORT(GetMonth) (void *m)
{
  return (unsigned int)-1;
}
#endif

/* wraptime_GetDay - returns the day of the month from the structure,
   m.  */

#if defined(HAVE_STRUCT_TM)
extern "C" unsigned int
EXPORT(GetDay) (struct tm *m)
{
  return m->tm_mday;
}
#else
extern "C" unsigned int
EXPORT(GetDay) (void *m)
{
  return (unsigned int)-1;
}
#endif

/* wraptime_GetHour - returns the hour of the day from the structure,
   m.  */

#if defined(HAVE_STRUCT_TM)
extern "C" unsigned int
EXPORT(GetHour) (struct tm *m)
{
  return m->tm_hour;
}
#else
extern "C" unsigned int
EXPORT(GetHour) (void *m)
{
  return (unsigned int)-1;
}
#endif

/* wraptime_GetMinute - returns the minute within the hour from the
   structure, m.  */

#if defined(HAVE_STRUCT_TM)
extern "C" unsigned int
EXPORT(GetMinute) (struct tm *m)
{
  return m->tm_min;
}
#else
extern "C" unsigned int
EXPORT(GetMinute) (void *m)
{
  return (unsigned int)-1;
}
#endif

/* wraptime_GetSecond - returns the seconds in the minute from the
   structure, m.  The return value will always be in the range 0..59.
   A leap minute of value 60 will be truncated to 59.  */

#if defined(HAVE_STRUCT_TM)
extern "C" unsigned int
EXPORT(GetSecond) (struct tm *m)
{
  if (m->tm_sec == 60)
    return 59;
  else
    return m->tm_sec;
}
#else
extern "C" unsigned int
EXPORT(GetSecond) (void *m)
{
  return (unsigned int)-1;
}
#endif

/* wraptime_GetSummerTime - returns true if summer time is in effect.  */

#if defined(HAVE_STRUCT_TIMEZONE)
extern "C" bool
EXPORT(GetSummerTime) (struct timezone *tz)
{
  return tz->tz_dsttime != 0;
}
#else
extern "C" bool
EXPORT(GetSummerTime) (void *tz)
{
  return false;
}
#endif

/* wraptime_GetDST - returns the number of minutes west of GMT.  */

#if defined(HAVE_STRUCT_TIMEZONE)
extern "C" int
EXPORT(GetDST) (struct timezone *tz)
{
  return tz->tz_minuteswest;
}
#else
extern "C" int
EXPORT(GetDST) (void *tz)
{
#if defined(INT_MIN)
  return INT_MIN;
#else
  return (int)((unsigned int)-1);
#endif
}
#endif

/* SetTimezone - set the timezone field inside timeval, tv.  */

#if defined(HAVE_STRUCT_TIMEZONE)
extern "C" void
EXPORT(SetTimezone) (struct timezone *tz, int zone, int minuteswest)
{
  tz->tz_dsttime = zone;
  tz->tz_minuteswest = minuteswest;
}
#else
extern "C" void
EXPORT(SetTimezone) (void *tz, int zone, int minuteswest)
{
}
#endif

/* SetTimeval - sets the fields in tm, t, with: second, minute, hour,
   day, month, year, fractions.  */

#if defined(HAVE_TIMEVAL)
extern "C" void
EXPORT(SetTimeval) (struct tm *t, unsigned int second, unsigned int minute,
		    unsigned int hour, unsigned int day, unsigned int month,
		    unsigned int year, unsigned int yday, unsigned int wday,
		    unsigned int isdst)
{
  t->tm_sec = second;
  t->tm_min = minute;
  t->tm_hour = hour;
  t->tm_mday = day;
  t->tm_mon = month;
  t->tm_year = year;
  t->tm_yday = yday;
  t->tm_wday = wday;
  t->tm_isdst = isdst;
}
#else
extern "C" void
EXPORT(SetTimeval) (void *t, unsigned int second, unsigned int minute,
		    unsigned int hour, unsigned int day, unsigned int month,
		    unsigned int year, unsigned int yday, unsigned int wday,
		    unsigned int isdst)
{
}
#endif

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
  m2iso_M2RTS_RegisterModule ("wraptime", M2LIBNAME,
			      M2EXPORT(init), M2EXPORT(fini),
			      M2EXPORT(dep));
}
