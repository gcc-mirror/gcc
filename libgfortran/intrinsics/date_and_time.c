/* Implementation of the DATE_AND_TIME intrinsic.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Steven Bosscher.

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
#include <string.h>
#include <assert.h>
#include <stdlib.h>

#include "time_1.h"

#ifndef abs
#define abs(x) ((x)>=0 ? (x) : -(x))
#endif


/* If the re-entrant version of gmtime is not available, provide a
   fallback implementation.  On some targets where the _r version is
   not available, gmtime uses thread-local storage so it's
   threadsafe.  */

#ifndef HAVE_GMTIME_R
/* If _POSIX is defined gmtime_r gets defined by mingw-w64 headers.  */
#ifdef gmtime_r
#undef gmtime_r
#endif

static struct tm *
gmtime_r (const time_t * timep, struct tm * result)
{
  *result = *gmtime (timep);
  return result;
}
#endif


/* DATE_AND_TIME ([DATE, TIME, ZONE, VALUES])

   Description: Returns data on the real-time clock and date in a form
   compatible with the representations defined in ISO 8601:1988.

   Class: Non-elemental subroutine.

   Arguments:

   DATE (optional) shall be scalar and of type default character.
   It is an INTENT(OUT) argument.  It is assigned a value of the
   form CCYYMMDD, where CC is the century, YY the year within the
   century, MM the month within the year, and DD the day within the
   month.  If there is no date available, they are assigned blanks.

   TIME (optional) shall be scalar and of type default character.
   It is an INTENT(OUT) argument. It is assigned a value of the
   form hhmmss.sss, where hh is the hour of the day, mm is the
   minutes of the hour, and ss.sss is the seconds and milliseconds
   of the minute.  If there is no clock available, they are assigned
   blanks.

   ZONE (optional) shall be scalar and of type default character.
   It is an INTENT(OUT) argument.  It is assigned a value of the
   form [+-]hhmm, where hh and mm are the time difference with
   respect to Coordinated Universal Time (UTC) in hours and parts
   of an hour expressed in minutes, respectively.  If there is no
   clock available, they are assigned blanks.

   VALUES (optional) shall be of type default integer and of rank
   one. It is an INTENT(OUT) argument. Its size shall be at least
   8. The values returned in VALUES are as follows:

      VALUES(1) the year (for example, 2003), or -HUGE(0) if there is
      no date available;

      VALUES(2) the month of the year, or -HUGE(0) if there
      is no date available;

      VALUES(3) the day of the month, or -HUGE(0) if there is no date
      available;

      VALUES(4) the time difference with respect to Coordinated
      Universal Time (UTC) in minutes, or -HUGE(0) if this information
      is not available;

      VALUES(5) the hour of the day, in the range of 0 to 23, or
      -HUGE(0) if there is no clock;

      VALUES(6) the minutes of the hour, in the range 0 to 59, or
      -HUGE(0) if there is no clock;

      VALUES(7) the seconds of the minute, in the range 0 to 60, or
      -HUGE(0) if there is no clock;

      VALUES(8) the milliseconds of the second, in the range 0 to
      999, or -HUGE(0) if there is no clock.

   NULL pointer represent missing OPTIONAL arguments.  All arguments
   have INTENT(OUT).  Because of the -i8 option, we must implement
   VALUES for INTEGER(kind=4) and INTEGER(kind=8).

   Based on libU77's date_time_.c.

   TODO :
   - Check year boundaries.
*/
#define DATE_LEN 8
#define TIME_LEN 10   
#define ZONE_LEN 5
#define VALUES_SIZE 8

extern void date_and_time (char *, char *, char *, gfc_array_i4 *,
			   GFC_INTEGER_4, GFC_INTEGER_4, GFC_INTEGER_4);
export_proto(date_and_time);

void
date_and_time (char *__date, char *__time, char *__zone,
	       gfc_array_i4 *__values, GFC_INTEGER_4 __date_len,
	       GFC_INTEGER_4 __time_len, GFC_INTEGER_4 __zone_len)
{
  int i;
  char date[DATE_LEN + 1];
  char timec[TIME_LEN + 1];
  char zone[ZONE_LEN + 1];
  GFC_INTEGER_4 values[VALUES_SIZE];

#ifndef HAVE_NO_DATE_TIME
  time_t lt;
  struct tm local_time;
  struct tm UTC_time;

  long usecs;

  if (!gf_gettime (&lt, &usecs))
    {
      values[7] = usecs / 1000;

      localtime_r (&lt, &local_time);
      gmtime_r (&lt, &UTC_time);

      /* All arguments can be derived from VALUES.  */
      values[0] = 1900 + local_time.tm_year;
      values[1] = 1 + local_time.tm_mon;
      values[2] = local_time.tm_mday;
      values[3] = (local_time.tm_min - UTC_time.tm_min +
	           60 * (local_time.tm_hour - UTC_time.tm_hour +
		     24 * (local_time.tm_yday - UTC_time.tm_yday)));
      values[4] = local_time.tm_hour;
      values[5] = local_time.tm_min;
      values[6] = local_time.tm_sec;

#if HAVE_SNPRINTF
      if (__date)
	snprintf (date, DATE_LEN + 1, "%04d%02d%02d",
		  values[0], values[1], values[2]);
      if (__time)
	snprintf (timec, TIME_LEN + 1, "%02d%02d%02d.%03d",
		  values[4], values[5], values[6], values[7]);

      if (__zone)
	snprintf (zone, ZONE_LEN + 1, "%+03d%02d",
		  values[3] / 60, abs (values[3] % 60));
#else
      if (__date)
	sprintf (date, "%04d%02d%02d", values[0], values[1], values[2]);

      if (__time)
	sprintf (timec, "%02d%02d%02d.%03d",
		 values[4], values[5], values[6], values[7]);

      if (__zone)
	sprintf (zone, "%+03d%02d",
		 values[3] / 60, abs (values[3] % 60));
#endif
    }
  else
    {
      memset (date, ' ', DATE_LEN);
      date[DATE_LEN] = '\0';

      memset (timec, ' ', TIME_LEN);
      timec[TIME_LEN] = '\0';

      memset (zone, ' ', ZONE_LEN);
      zone[ZONE_LEN] = '\0';

      for (i = 0; i < VALUES_SIZE; i++)
	values[i] = - GFC_INTEGER_4_HUGE;
    }   
#else /* if defined HAVE_NO_DATE_TIME  */
  /* We really have *nothing* to return, so return blanks and HUGE(0).  */
      
  memset (date, ' ', DATE_LEN);
  date[DATE_LEN] = '\0';

  memset (timec, ' ', TIME_LEN);
  timec[TIME_LEN] = '\0';

  memset (zone, ' ', ZONE_LEN);
  zone[ZONE_LEN] = '\0';

  for (i = 0; i < VALUES_SIZE; i++)
    values[i] = - GFC_INTEGER_4_HUGE;
#endif  /* HAVE_NO_DATE_TIME  */

  /* Copy the values into the arguments.  */
  if (__values)
    {
      index_type len, delta, elt_size;

      elt_size = GFC_DESCRIPTOR_SIZE (__values);
      len = GFC_DESCRIPTOR_EXTENT(__values,0);
      delta = GFC_DESCRIPTOR_STRIDE(__values,0);
      if (delta == 0)
	delta = 1;
      
      if (unlikely (len < VALUES_SIZE))
	  runtime_error ("Incorrect extent in VALUE argument to"
			 " DATE_AND_TIME intrinsic: is %ld, should"
			 " be >=%ld", (long int) len, (long int) VALUES_SIZE);

      /* Cope with different type kinds.  */
      if (elt_size == 4)
        {
	  GFC_INTEGER_4 *vptr4 = __values->data;

	  for (i = 0; i < VALUES_SIZE; i++, vptr4 += delta)
	    *vptr4 = values[i];
	}
      else if (elt_size == 8)
        {
	  GFC_INTEGER_8 *vptr8 = (GFC_INTEGER_8 *)__values->data;

	  for (i = 0; i < VALUES_SIZE; i++, vptr8 += delta)
	    {
	      if (values[i] == - GFC_INTEGER_4_HUGE)
		*vptr8 = - GFC_INTEGER_8_HUGE;
	      else
		*vptr8 = values[i];
	    }
	}
      else 
	abort ();
    }

  if (__zone)
    fstrcpy (__zone, __zone_len, zone, ZONE_LEN);

  if (__time)
    fstrcpy (__time, __time_len, timec, TIME_LEN);

  if (__date)
    fstrcpy (__date, __date_len, date, DATE_LEN);
}


/* SECNDS (X) - Non-standard

   Description: Returns the system time of day, or elapsed time, as a GFC_REAL_4
   in seconds.

   Class: Non-elemental subroutine.

   Arguments:

   X must be REAL(4) and the result is of the same type.  The accuracy is system
   dependent.

   Usage:

	T = SECNDS (X)

   yields the time in elapsed seconds since X.  If X is 0.0, T is the time in
   seconds since midnight. Note that a time that spans midnight but is less than
   24hours will be calculated correctly.  */

extern GFC_REAL_4 secnds (GFC_REAL_4 *);
export_proto(secnds);

GFC_REAL_4
secnds (GFC_REAL_4 *x)
{
  GFC_INTEGER_4 values[VALUES_SIZE];
  GFC_REAL_4 temp1, temp2;

  /* Make the INTEGER*4 array for passing to date_and_time.  */
  gfc_array_i4 *avalues = internal_malloc_size (sizeof (gfc_array_i4));
  avalues->data = &values[0];
  GFC_DESCRIPTOR_DTYPE (avalues) = ((BT_REAL << GFC_DTYPE_TYPE_SHIFT)
				        & GFC_DTYPE_TYPE_MASK) +
				    (4 << GFC_DTYPE_SIZE_SHIFT);

  GFC_DIMENSION_SET(avalues->dim[0], 0, 7, 1);

  date_and_time (NULL, NULL, NULL, avalues, 0, 0, 0);

  free (avalues);

  temp1 = 3600.0 * (GFC_REAL_4)values[4] +
	    60.0 * (GFC_REAL_4)values[5] +
		   (GFC_REAL_4)values[6] +
	   0.001 * (GFC_REAL_4)values[7];
  temp2 = fmod (*x, 86400.0);
  temp2 = (temp1 - temp2 >= 0.0) ? temp2 : (temp2 - 86400.0);
  return temp1 - temp2;
}



/* ITIME(X) - Non-standard

   Description: Returns the current local time hour, minutes, and seconds
   in elements 1, 2, and 3 of X, respectively.  */

static void
itime0 (int x[3])
{
#ifndef HAVE_NO_DATE_TIME
  time_t lt;
  struct tm local_time;

  lt = time (NULL);

  if (lt != (time_t) -1)
    {
      localtime_r (&lt, &local_time);

      x[0] = local_time.tm_hour;
      x[1] = local_time.tm_min;
      x[2] = local_time.tm_sec;
    }
#else
  x[0] = x[1] = x[2] = -1;
#endif
}

extern void itime_i4 (gfc_array_i4 *);
export_proto(itime_i4);

void
itime_i4 (gfc_array_i4 *__values)
{
  int x[3], i;
  index_type len, delta;
  GFC_INTEGER_4 *vptr;
  
  /* Call helper function.  */
  itime0(x);

  /* Copy the value into the array.  */
  len = GFC_DESCRIPTOR_EXTENT(__values,0);
  assert (len >= 3);
  delta = GFC_DESCRIPTOR_STRIDE(__values,0);
  if (delta == 0)
    delta = 1;

  vptr = __values->data;
  for (i = 0; i < 3; i++, vptr += delta)
    *vptr = x[i];
}


extern void itime_i8 (gfc_array_i8 *);
export_proto(itime_i8);

void
itime_i8 (gfc_array_i8 *__values)
{
  int x[3], i;
  index_type len, delta;
  GFC_INTEGER_8 *vptr;
  
  /* Call helper function.  */
  itime0(x);

  /* Copy the value into the array.  */
  len = GFC_DESCRIPTOR_EXTENT(__values,0);
  assert (len >= 3);
  delta = GFC_DESCRIPTOR_STRIDE(__values,0);
  if (delta == 0)
    delta = 1;

  vptr = __values->data;
  for (i = 0; i < 3; i++, vptr += delta)
    *vptr = x[i];
}



/* IDATE(X) - Non-standard

   Description: Fills TArray with the numerical values at the current
   local time. The day (in the range 1-31), month (in the range 1-12),
   and year appear in elements 1, 2, and 3 of X, respectively.
   The year has four significant digits.  */

static void
idate0 (int x[3])
{
#ifndef HAVE_NO_DATE_TIME
  time_t lt;
  struct tm local_time;

  lt = time (NULL);

  if (lt != (time_t) -1)
    {
      localtime_r (&lt, &local_time);

      x[0] = local_time.tm_mday;
      x[1] = 1 + local_time.tm_mon;
      x[2] = 1900 + local_time.tm_year;
    }
#else
  x[0] = x[1] = x[2] = -1;
#endif
}

extern void idate_i4 (gfc_array_i4 *);
export_proto(idate_i4);

void
idate_i4 (gfc_array_i4 *__values)
{
  int x[3], i;
  index_type len, delta;
  GFC_INTEGER_4 *vptr;
  
  /* Call helper function.  */
  idate0(x);

  /* Copy the value into the array.  */
  len = GFC_DESCRIPTOR_EXTENT(__values,0);
  assert (len >= 3);
  delta = GFC_DESCRIPTOR_STRIDE(__values,0);
  if (delta == 0)
    delta = 1;

  vptr = __values->data;
  for (i = 0; i < 3; i++, vptr += delta)
    *vptr = x[i];
}


extern void idate_i8 (gfc_array_i8 *);
export_proto(idate_i8);

void
idate_i8 (gfc_array_i8 *__values)
{
  int x[3], i;
  index_type len, delta;
  GFC_INTEGER_8 *vptr;
  
  /* Call helper function.  */
  idate0(x);

  /* Copy the value into the array.  */
  len = GFC_DESCRIPTOR_EXTENT(__values,0);
  assert (len >= 3);
  delta = GFC_DESCRIPTOR_STRIDE(__values,0);
  if (delta == 0)
    delta = 1;

  vptr = __values->data;
  for (i = 0; i < 3; i++, vptr += delta)
    *vptr = x[i];
}



/* GMTIME(STIME, TARRAY) - Non-standard

   Description: Given a system time value STime, fills TArray with values
   extracted from it appropriate to the GMT time zone using gmtime_r(3).

   The array elements are as follows:

      1. Seconds after the minute, range 0-59 or 0-61 to allow for leap seconds
      2. Minutes after the hour, range 0-59
      3. Hours past midnight, range 0-23
      4. Day of month, range 0-31
      5. Number of months since January, range 0-11
      6. Years since 1900
      7. Number of days since Sunday, range 0-6
      8. Days since January 1
      9. Daylight savings indicator: positive if daylight savings is in effect,
         zero if not, and negative if the information isn't available.  */

static void
gmtime_0 (const time_t * t, int x[9])
{
  struct tm lt;

  gmtime_r (t, &lt);
  x[0] = lt.tm_sec;
  x[1] = lt.tm_min;
  x[2] = lt.tm_hour;
  x[3] = lt.tm_mday;
  x[4] = lt.tm_mon;
  x[5] = lt.tm_year;
  x[6] = lt.tm_wday;
  x[7] = lt.tm_yday;
  x[8] = lt.tm_isdst;
}

extern void gmtime_i4 (GFC_INTEGER_4 *, gfc_array_i4 *);
export_proto(gmtime_i4);

void
gmtime_i4 (GFC_INTEGER_4 * t, gfc_array_i4 * tarray)
{
  int x[9], i;
  index_type len, delta;
  GFC_INTEGER_4 *vptr;
  time_t tt;
  
  /* Call helper function.  */
  tt = (time_t) *t;
  gmtime_0(&tt, x);

  /* Copy the values into the array.  */
  len = GFC_DESCRIPTOR_EXTENT(tarray,0);
  assert (len >= 9);
  delta = GFC_DESCRIPTOR_STRIDE(tarray,0);
  if (delta == 0)
    delta = 1;

  vptr = tarray->data;
  for (i = 0; i < 9; i++, vptr += delta)
    *vptr = x[i];
}

extern void gmtime_i8 (GFC_INTEGER_8 *, gfc_array_i8 *);
export_proto(gmtime_i8);

void
gmtime_i8 (GFC_INTEGER_8 * t, gfc_array_i8 * tarray)
{
  int x[9], i;
  index_type len, delta;
  GFC_INTEGER_8 *vptr;
  time_t tt;
  
  /* Call helper function.  */
  tt = (time_t) *t;
  gmtime_0(&tt, x);

  /* Copy the values into the array.  */
  len = GFC_DESCRIPTOR_EXTENT(tarray,0);
  assert (len >= 9);
  delta = GFC_DESCRIPTOR_STRIDE(tarray,0);
  if (delta == 0)
    delta = 1;

  vptr = tarray->data;
  for (i = 0; i < 9; i++, vptr += delta)
    *vptr = x[i];
}




/* LTIME(STIME, TARRAY) - Non-standard

   Description: Given a system time value STime, fills TArray with values
   extracted from it appropriate to the local time zone using localtime_r(3).

   The array elements are as follows:

      1. Seconds after the minute, range 0-59 or 0-61 to allow for leap seconds
      2. Minutes after the hour, range 0-59
      3. Hours past midnight, range 0-23
      4. Day of month, range 0-31
      5. Number of months since January, range 0-11
      6. Years since 1900
      7. Number of days since Sunday, range 0-6
      8. Days since January 1
      9. Daylight savings indicator: positive if daylight savings is in effect,
         zero if not, and negative if the information isn't available.  */

static void
ltime_0 (const time_t * t, int x[9])
{
  struct tm lt;

  localtime_r (t, &lt);
  x[0] = lt.tm_sec;
  x[1] = lt.tm_min;
  x[2] = lt.tm_hour;
  x[3] = lt.tm_mday;
  x[4] = lt.tm_mon;
  x[5] = lt.tm_year;
  x[6] = lt.tm_wday;
  x[7] = lt.tm_yday;
  x[8] = lt.tm_isdst;
}

extern void ltime_i4 (GFC_INTEGER_4 *, gfc_array_i4 *);
export_proto(ltime_i4);

void
ltime_i4 (GFC_INTEGER_4 * t, gfc_array_i4 * tarray)
{
  int x[9], i;
  index_type len, delta;
  GFC_INTEGER_4 *vptr;
  time_t tt;
  
  /* Call helper function.  */
  tt = (time_t) *t;
  ltime_0(&tt, x);

  /* Copy the values into the array.  */
  len = GFC_DESCRIPTOR_EXTENT(tarray,0);
  assert (len >= 9);
  delta = GFC_DESCRIPTOR_STRIDE(tarray,0);
  if (delta == 0)
    delta = 1;

  vptr = tarray->data;
  for (i = 0; i < 9; i++, vptr += delta)
    *vptr = x[i];
}

extern void ltime_i8 (GFC_INTEGER_8 *, gfc_array_i8 *);
export_proto(ltime_i8);

void
ltime_i8 (GFC_INTEGER_8 * t, gfc_array_i8 * tarray)
{
  int x[9], i;
  index_type len, delta;
  GFC_INTEGER_8 *vptr;
  time_t tt;
  
  /* Call helper function.  */
  tt = (time_t) * t;
  ltime_0(&tt, x);

  /* Copy the values into the array.  */
  len = GFC_DESCRIPTOR_EXTENT(tarray,0);
  assert (len >= 9);
  delta = GFC_DESCRIPTOR_STRIDE(tarray,0);
  if (delta == 0)
    delta = 1;

  vptr = tarray->data;
  for (i = 0; i < 9; i++, vptr += delta)
    *vptr = x[i];
}


