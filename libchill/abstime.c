/* Implement timing-related runtime actions for CHILL.
   Copyright (C) 1992,1993 Free Software Foundation, Inc.
   Author: Wilfried Moser

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include <time.h>
#include "rtltypes.h"

EXCEPTION (rangefail);

#define SECOND_VALID         1
#define MINUTE_VALID         2
#define HOUR_VALID           4
#define DAY_VALID            8
#define MONTH_VALID         16
#define YEAR_VALID          32

extern void __cause_ex1 (char *ex, char *file, int lineno);

#define CAUSE_RANGEFAIL     __cause_ex1 ("rangefail", filename, lineno)

/*
 * function _abstime
 *
 * parameters:
 *     mask - mask of valid values
 *     year
 *     month
 *     day
 *     hour
 *     minute
 *     second
 *
 * returns:
 *     unsigned long
 *
 * exceptions:
 *     rangefail
 *
 * abstract:
 *     perform the ABSTIME builtin call
 *
 */

unsigned long
_abstime (mask, year, month, day, hour, minute, second,
	  filename, lineno)
     int  mask, year, month, day, hour, minute, second;
     char *filename;
     int  lineno;
{
  struct tm   *time_str;
  time_t      result, current_time;

  /* first of all get current time */
  if ((current_time = time (0)) == (time_t)-1)
    /* FIXME: what excpetion ?? */
    CAUSE_RANGEFAIL;

  /* if we just have to determine the current time, we are ready.
     This is shown by mask == 0. */
  if (mask == 0)
    return (unsigned long)current_time;

  /* convert current time to struct tm */
  time_str = localtime (&current_time);

  if (mask & YEAR_VALID)
    {
      if (year < 1900)
	CAUSE_RANGEFAIL;
      time_str->tm_year = year - 1900;
    }

  if (mask & MONTH_VALID)
    {
      if (month < 1 || month > 12)
	CAUSE_RANGEFAIL;
      time_str->tm_mon = month - 1;
    }

  if (mask & DAY_VALID)
    {
      if (day < 1 || day > 31)
	CAUSE_RANGEFAIL;
      time_str->tm_mday = day;
    }

  if (mask & HOUR_VALID)
    {
      if (hour < 0 || hour > 23)
	CAUSE_RANGEFAIL;
      time_str->tm_hour = hour;
    }

  if (mask & MINUTE_VALID)
    {
      if (minute < 0 || minute > 59)
	CAUSE_RANGEFAIL;
      time_str->tm_min = minute;
    }

  if (mask & SECOND_VALID)
    {
      if (second < 0 || second > 59)
	CAUSE_RANGEFAIL;
      time_str->tm_sec = second;
    }

  /* do it */
  time_str->tm_isdst = -1;
  if ((result = mktime (time_str)) == (time_t)-1)
    CAUSE_RANGEFAIL;

  return (unsigned long)result;
}
