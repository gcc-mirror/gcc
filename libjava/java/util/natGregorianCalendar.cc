/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

// We want to make sure to pick up the POSIX `_r' functions.  Some
// systems, such as Solaris 2.6, require this define in order to
// declare the functions in the appropriate header.
#if defined (HAVE_GMTIME_R) || defined (HAVE_LOCALTIME_R)
#  define _POSIX_PTHREAD_SEMANTICS
#  ifndef _REENTRANT
#    define _REENTRANT
#  endif /* _REENTRANT */
#endif

#ifdef ECOS
#include <string.h>
#endif

#include <gcj/cni.h>
#include <java/util/TimeZone.h>
#include <java/util/GregorianCalendar.h>
#include <time.h>

void
java::util::GregorianCalendar::computeTime ()
{
  struct tm tim;
  tim.tm_sec = elements(fields)[SECOND];
  tim.tm_min = elements(fields)[MINUTE];
  tim.tm_hour = elements(fields)[HOUR_OF_DAY];
  tim.tm_mday = elements(fields)[DATE];
  tim.tm_mon = elements(fields)[MONTH];
  tim.tm_year = elements(fields)[YEAR] - 1900;
  tim.tm_isdst = 0;  // FIXME
#ifndef ECOS
  // FIXME: None of the standard C library access to the ECOS calendar
  // is yet available.
  time_t t = mktime (&tim);
#else
  time_t t = 0;
#endif

  // Adjust for local timezone (introduced by mktime) and our
  // timezone.
#if defined (STRUCT_TM_HAS_GMTOFF)
  t += tim.tm_gmtoff;
#elif defined (HAVE_TIMEZONE)
  t -= timezone;
#endif
  java::util::TimeZone *zone = getTimeZone ();
  t += zone->getRawOffset();

  // Adjust for milliseconds.
  time = t * (jlong) 1000 + elements(fields)[MILLISECOND];

  isTimeSet = true;
}

void
java::util::GregorianCalendar::computeFields ()
{
  time_t t = time / 1000;
  int millis = time % 1000;
  if (t < 0 && millis != 0)
    {
      t--;
      millis = t - 1000 * t;
    }
  elements(fields)[MILLISECOND] = millis;
  struct tm tim;
  java::util::TimeZone *zone = getTimeZone ();

  // FIXME: None of the standard C library access to the ECOS calendar
  // is yet available.
#ifdef ECOS
  memset (&tim, 0, sizeof tim); 
#else
  if (zone->getRawOffset() == 0 || ! zone->useDaylightTime())
    {
#if defined(__JV_POSIX_THREADS__) && defined(HAVE_GMTIME_R)
      gmtime_r (&t, &tim);
#else
      // Get global lock (because gmtime uses a global buffer).  FIXME
      tim = *(struct tm*) gmtime (&t);
      // Release global lock.  FIXME
#endif
    }
  else
    {
#if defined(__JV_POSIX_THREADS__) && defined(HAVE_LOCALTIME_R)
      localtime_r (&t, &tim);
#else
      // Get global lock (because localtime uses a global buffer).  FIXME
      tim = *(struct tm*) localtime (&t);
      // Release global lock.  FIXME
#endif
    }
#endif /* ECOS */
  elements(fields)[SECOND] = tim.tm_sec;
  elements(fields)[MINUTE] = tim.tm_min;
  elements(fields)[HOUR_OF_DAY] = tim.tm_hour;
  elements(fields)[AM_PM] = tim.tm_hour < 12 ? AM : PM;
  elements(fields)[HOUR] = tim.tm_hour % 12;
  elements(fields)[DATE] = tim.tm_mday;
  elements(fields)[MONTH] = tim.tm_mon;
  elements(fields)[YEAR] = 1900 + tim.tm_year;
  elements(fields)[DAY_OF_WEEK] = tim.tm_wday + 1;
  elements(fields)[DAY_OF_WEEK_IN_MONTH] = ((tim.tm_mday - 1) / 7) + 1;
  elements(fields)[DAY_OF_YEAR] = tim.tm_yday + 1;
  elements(fields)[WEEK_OF_MONTH]
    = (tim.tm_mday + 6 + (5 - tim.tm_wday + getFirstDayOfWeek()) % 7) / 7;
  elements(fields)[WEEK_OF_YEAR]
    = (tim.tm_yday + 7 + (5 - tim.tm_wday + getFirstDayOfWeek()) % 7) / 7;
  elements(fields)[ERA] = AD;
  elements(fields)[DST_OFFSET] = tim.tm_isdst <= 0 ? 0 : 60*60*1000;
  elements(fields)[ZONE_OFFSET] = getTimeZone()->getRawOffset();
  areFieldsSet = true;
}
