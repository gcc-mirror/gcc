/* Copyright (C) 1998, 1999, 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#ifdef ECOS
#include <string.h>
#endif

#include <gcj/cni.h>
#include <java/util/TimeZone.h>
#include <java/util/GregorianCalendar.h>
#include <java/lang/IllegalArgumentException.h>
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
  tim.tm_isdst = 0;
#ifndef ECOS
  // FIXME: None of the standard C library access to the ECOS calendar
  // is yet available.
  time_t t = mktime (&tim);

  if (!isLenient ())
    {
      // mktime will correct for any time leniencies (e.g. 31-Apr becomes
      // 1-May).
      // Daylight savings time is a special case since times in hour 23
      // will compute to hour 0 of the next day.
      if (tim.tm_isdst == 0 || elements(fields)[HOUR_OF_DAY] != 23)
        {
	  if (tim.tm_sec != elements(fields)[SECOND] ||
	      tim.tm_min != elements(fields)[MINUTE] ||
	      tim.tm_hour != elements(fields)[HOUR_OF_DAY] +
	      		     (tim.tm_isdst > 0 ? 1 : 0) ||
	      tim.tm_mday != elements(fields)[DATE] ||
	      tim.tm_mon != elements(fields)[MONTH] ||
	      tim.tm_year != elements(fields)[YEAR] - 1900)
	    throw new java::lang::IllegalArgumentException ();
        }
      else
        {
	  // The easiest thing to do is to temporarily shift the clock
	  // back from the 23th hour so mktime doesn't cause the extra
	  // hour for DST to roll the date to the next day.
	  struct tm tmp_tim;
	  tmp_tim.tm_sec = elements(fields)[SECOND];
	  tmp_tim.tm_min = elements(fields)[MINUTE];
	  tmp_tim.tm_hour = elements(fields)[HOUR_OF_DAY] - 1;
	  tmp_tim.tm_mday = elements(fields)[DATE];
	  tmp_tim.tm_mon = elements(fields)[MONTH];
	  tmp_tim.tm_year = elements(fields)[YEAR] - 1900;
	  tmp_tim.tm_isdst = 0;
	  mktime (&tmp_tim);
	  if (tmp_tim.tm_sec != elements(fields)[SECOND] ||
	      tmp_tim.tm_min != elements(fields)[MINUTE] ||
	      tmp_tim.tm_hour != elements(fields)[HOUR_OF_DAY] ||
	      tmp_tim.tm_mday != elements(fields)[DATE] ||
	      tmp_tim.tm_mon != elements(fields)[MONTH] ||
	      tmp_tim.tm_year != elements(fields)[YEAR] - 1900)
	    throw new java::lang::IllegalArgumentException ();
	}
    }
#else
  time_t t = 0;
#endif

  // Adjust for local timezone (introduced by mktime) and our
  // timezone.
#if defined (STRUCT_TM_HAS_GMTOFF)
  t -= tim.tm_gmtoff;
#elif defined (HAVE_TIMEZONE)
  t += timezone;
#endif
  // Adjust for milliseconds.
  time = t * (jlong) 1000 + elements(fields)[MILLISECOND];

  // Now adjust for the real timezone, i.e. our timezone, which is in millis.
  java::util::TimeZone *zone = getTimeZone ();
  time += zone->getRawOffset();

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
  for (int i = 0; i < FIELD_COUNT; i++)
    elements(isSet__)[i] = true;
}
