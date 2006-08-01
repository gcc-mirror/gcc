// natVMTimeZone.cc -- Native side of VMTimeZone class.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2006
   Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/util/VMTimeZone.h>
#include <java/lang/Character.h>
#include <java/lang/Integer.h>

#include <stdio.h>

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

#include <string.h>

/**
 * This method returns a time zone id string which is in the form
 * (standard zone name) or (standard zone name)(GMT offset) or
 * (standard zone name)(GMT offset)(daylight time zone name).  The
 * GMT offset can be in seconds, or where it is evenly divisible by
 * 3600, then it can be in hours.  The offset must be the time to
 * add to the local time to get GMT.  If a offset is given and the
 * time zone observes daylight saving then the (daylight time zone
 * name) must also be given (otherwise it is assumed the time zone
 * does not observe any daylight savings).
 * <p>
 * The result of this method is given to getDefaultTimeZone(String)
 * which tries to map the time zone id to a known TimeZone.  See
 * that method on how the returned String is mapped to a real
 * TimeZone object.
 */
jstring
java::util::VMTimeZone::getSystemTimeZoneId()
{
  struct tm tim;
#if !defined(HAVE_LOCALTIME_R) || !defined(_POSIX_PTHREAD_SEMANTICS)
  struct tm *lt_tim;
#endif
#ifdef HAVE_TM_ZONE
  int month;
#endif
  time_t current_time;
  long tzoffset;
  const char *tz1, *tz2;
  char *tzid;

  time(&current_time);
#if defined(HAVE_LOCALTIME_R) && defined(_POSIX_PTHREAD_SEMANTICS)
  localtime_r(&current_time, &tim);
#else
  /* Fall back on non-thread safe localtime. */
  lt_tim = localtime(&current_time);
  memcpy(&tim, lt_tim, sizeof (struct tm));
#endif
  mktime(&tim);

#ifdef HAVE_TM_ZONE
  /* We will cycle through the months to make sure we hit dst. */
  month = tim.tm_mon;
  tz1 = tz2 = NULL;
  while (tz1 == NULL || tz2 == NULL)
    {
      if (tim.tm_isdst > 0)
        tz2 = tim.tm_zone;
      else if (tz1 == NULL)
        {
          tz1 = tim.tm_zone;
          month = tim.tm_mon;
        }

      if (tz1 == NULL || tz2 == NULL)
        {
          tim.tm_mon++;
          tim.tm_mon %= 12;
        }

      if (tim.tm_mon == month && tz2 == NULL)
        tz2 = "";
      else
        mktime(&tim);
    }
  /* We want to make sure the tm struct we use later on is not dst. */
  tim.tm_mon = month;
  mktime(&tim);
#elif defined (HAVE_TZNAME)
  /* If dst is never used, tzname[1] is the empty string. */
  tzset();
  tz1 = tzname[0];
  tz2 = tzname[1];
#else
  /* Some targets have no concept of timezones. Assume GMT without dst. */
  tz1 = "GMT";
  tz2 = "";
#endif

#ifdef STRUCT_TM_HAS_GMTOFF
  /* tm_gmtoff is the number of seconds that you must add to GMT to get
     local time, we need the number of seconds to add to the local time
     to get GMT. */
  tzoffset = -1L * tim.tm_gmtoff;
#elif HAVE_UNDERSCORE_TIMEZONE
  tzoffset = _timezone;
#elif HAVE_TIMEZONE
  /* timezone is secs WEST of UTC. */
  tzoffset = timezone;	
#else
  /* FIXME: there must be another global if neither tm_gmtoff nor timezone
     is available, esp. if tzname is valid.
     Richard Earnshaw <rearnsha@arm.com> has suggested using difftime to
     calculate between gmtime and localtime (and accounting for possible
     daylight savings time) as an alternative. */
  tzoffset = 0L;
#endif

  if ((tzoffset % 3600) == 0)
    tzoffset = tzoffset / 3600;

  tzid = (char*) _Jv_Malloc (strlen(tz1) + strlen(tz2) + 6);
  sprintf(tzid, "%s%ld%s", tz1, tzoffset, tz2);
  jstring retval = JvNewStringUTF (tzid);
  _Jv_Free (tzid);

  return retval;
}
