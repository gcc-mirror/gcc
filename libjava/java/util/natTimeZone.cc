// natTimeZone.cc -- Native side of TimeZone class.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/util/TimeZone.h>
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

/*
 * This method returns a time zone string that is used by init_properties
 * to set the default timezone property 'user.timezone'.  That value is
 * used by default as a key into the timezone table used by the
 * java::util::TimeZone class.
 */
static jstring
getSystemTimeZone (void)
{
  struct tm *tim;
  time_t current_time;
  long tzoffset;
  const char *tz1, *tz2;
  char *tzid;

  current_time = time(0);

  mktime(tim = localtime(&current_time));
#ifdef STRUCT_TM_HAS_GMTOFF
  // tm_gmtoff is secs EAST of UTC.
  tzoffset = -(tim->tm_gmtoff) + tim->tm_isdst * 3600L;
#elif HAVE_UNDERSCORE_TIMEZONE
  tzoffset = _timezone;
#elif HAVE_TIMEZONE
  // timezone is secs WEST of UTC.
  tzoffset = timezone;	
#else
  // FIXME: there must be another global if neither tm_gmtoff nor timezone
  // is available, esp. if tzname is valid.
  // Richard Earnshaw <rearnsha@arm.com> has suggested using difftime to
  // calculate between gmtime and localtime (and accounting for possible
  // daylight savings time) as an alternative.
  tzoffset = 0L;
#endif

#ifdef HAVE_TM_ZONE
  tz1 = tim->tm_zone;
  tz2 = "";
#elif defined (HAVE_TZNAME)
  tz1 = tzname[0];
  tz2 = strcmp (tzname[0], tzname[1]) ? tzname[1] : "";
#else
  // Some targets have no concept of timezones.
  tz1 = "???";
  tz2 = tz1;
#endif

  if ((tzoffset % 3600) == 0)
    tzoffset = tzoffset / 3600;

  tzid = (char*) _Jv_Malloc (strlen(tz1) + strlen(tz2) + 6);
  sprintf(tzid, "%s%ld%s", tz1, tzoffset, tz2);
  jstring retval = JvNewStringUTF (tzid);
  _Jv_Free (tzid);

  return retval;
}

// Get the System Timezone as reported by the OS.  It should be in
// the form PST8PDT so we'll need to parse it and check that it's valid.
// FIXME: Using the code from Classpath for generating the System
// Timezone IMO is suboptimal because it ignores whether the rules for
// DST match up.
jstring
java::util::TimeZone::getDefaultTimeZoneId ()
{
  jstring sysTimeZoneId = getSystemTimeZone ();

  using namespace java::lang;

  // Check if this is a valid timezone.  Make sure the IDs match
  // since getTimeZone returns GMT if no match is found.
  TimeZone *tz = TimeZone::getTimeZone (sysTimeZoneId);
  if (tz->getID ()->equals (sysTimeZoneId))
    return sysTimeZoneId;

  // Check if the base part of sysTimeZoneId is a valid timezone that
  // matches with daylight usage and rawOffset.  Make sure the IDs match
  // since getTimeZone returns GMT if no match is found.
  // First find start of GMT offset info and any Daylight zone name.
  int startGMToffset = 0;
  int sysTimeZoneIdLength = sysTimeZoneId->length();
  for (int i = 0; i < sysTimeZoneIdLength && startGMToffset == 0; i++)
    {
      if (Character::isDigit (sysTimeZoneId->charAt (i)))
	startGMToffset = i;
    }

  int startDaylightZoneName = 0;
  jboolean usesDaylight = false;
  for (int i = sysTimeZoneIdLength - 1;
       i >= 0 && !Character::isDigit (sysTimeZoneId->charAt (i)); --i)
    {
      startDaylightZoneName = i;
    }
  if (startDaylightZoneName > 0)
    usesDaylight = true;

  int GMToffset
    = Integer::parseInt (startDaylightZoneName == 0 ?
			 sysTimeZoneId->substring (startGMToffset) :
			 sysTimeZoneId->substring (startGMToffset,
						   startDaylightZoneName));

  // Offset could be in hours or seconds.  Convert to millis.
  if (GMToffset < 24)
    GMToffset *= 60 * 60;
  GMToffset *= -1000;

  jstring tzBasename = sysTimeZoneId->substring (0, startGMToffset);
  tz = TimeZone::getTimeZone (tzBasename);
  if (tz->getID ()->equals (tzBasename) && tz->getRawOffset () == GMToffset)
    {
      jboolean tzUsesDaylight = tz->useDaylightTime ();
      if (usesDaylight && tzUsesDaylight || !usesDaylight && !tzUsesDaylight)
	return tzBasename;
    }

  // If no match, see if a valid timezone has the same attributes as this
  // and then use it instead.
  jstringArray IDs = TimeZone::getAvailableIDs (GMToffset);
  jstring *elts = elements (IDs);
  for (int i = 0; i < IDs->length; ++i)
    {
      // FIXME: The daylight savings rules may not match the rules
      // for the desired zone.
      jboolean IDusesDaylight =
	TimeZone::getTimeZone (elts[i])->useDaylightTime ();
      if (usesDaylight && IDusesDaylight || !usesDaylight && !IDusesDaylight)
	return elts[i];
    }

  // If all else fails, return null.
  return NULL;
}
