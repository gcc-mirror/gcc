/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <java/util/TimeZone.h>

#include <stdio.h>
#include <string.h>

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

/*
 * This method returns a time zone string that is used by the static
 * initializer in java.util.TimeZone to create the default timezone
 * instance.  This is a key into the timezone table used by
 * that class.
 */
jstring
java::util::TimeZone::getDefaultTimeZoneId (void)
{
  time_t current_time;
  char **tzinfo, *tzid;
  long tzoffset;
  jstring retval;

  current_time = time(0);

  mktime(localtime(&current_time));
  tzinfo = tzname;
  tzoffset = timezone;

  if ((tzoffset % 3600) == 0)
    tzoffset = tzoffset / 3600;

  if (!strcmp(tzinfo[0], tzinfo[1]))  
    {
      tzid = (char*) _Jv_Malloc (strlen(tzinfo[0]) + 6);
      if (!tzid)
        return NULL;

      sprintf(tzid, "%s%ld", tzinfo[0], tzoffset);
    }
  else
    {
      tzid = (char*) _Jv_Malloc (strlen(tzinfo[0]) + strlen(tzinfo[1]) + 6);
      if (!tzid)
        return NULL;

      sprintf(tzid, "%s%ld%s", tzinfo[0], tzoffset, tzinfo[1]);
    }

  retval = JvNewStringUTF (tzid);
  _Jv_Free (tzid);
  return retval;
}

