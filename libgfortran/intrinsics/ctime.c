/* Implementation of the CTIME and FDATE g77 intrinsics.
   Copyright (C) 2005-2021 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

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

#include "time_1.h"

#include <string.h>


/* Maximum space a ctime-like string might need. A "normal" ctime
   string is 26 bytes, and in our case 24 bytes as we don't include
   the trailing newline and null. However, the longest possible year
   number is -2,147,481,748 (1900 - 2,147,483,648, since tm_year is a
   32-bit signed integer) so an extra 7 bytes are needed. */
#define CTIME_BUFSZ 31


/* Thread-safe ctime-like function that fills a Fortran
   string. ctime_r is a portability headache and marked as obsolescent
   in POSIX 2008, which recommends strftime in its place. However,
   strftime(..., "%c",...)  doesn't produce ctime-like output on
   MinGW, so do it manually with snprintf.  */

static int
gf_ctime (char *s, size_t max, const time_t timev)
{
  struct tm ltm;
  int failed;
  char buf[CTIME_BUFSZ + 1];
  /* Some targets provide a localtime_r based on a draft of the POSIX
     standard where the return type is int rather than the
     standardized struct tm*.  */
  __builtin_choose_expr (__builtin_classify_type (localtime_r (&timev, &ltm)) 
			 == 5,
			 failed = localtime_r (&timev, &ltm) == NULL,
			 failed = localtime_r (&timev, &ltm) != 0);
  if (failed)
    goto blank;
  int n = snprintf (buf, sizeof (buf), 
		    "%3.3s %3.3s%3d %.2d:%.2d:%.2d %d",
		    "SunMonTueWedThuFriSat" + ltm.tm_wday * 3,
		    "JanFebMarAprMayJunJulAugSepOctNovDec" + ltm.tm_mon * 3,
		    ltm.tm_mday, ltm.tm_hour, ltm.tm_min, ltm.tm_sec, 
		    1900 + ltm.tm_year);
  if (n < 0)
    goto blank;
  if ((size_t) n <= max)
    {
      cf_strcpy (s, max, buf);
      return n;
    }
 blank:
  memset (s, ' ', max);
  return 0;
}


extern void fdate (char **, gfc_charlen_type *);
export_proto(fdate);

void
fdate (char ** date, gfc_charlen_type * date_len)
{
  time_t now = time(NULL);
  *date = xmalloc (CTIME_BUFSZ);
  *date_len = gf_ctime (*date, CTIME_BUFSZ, now);
}


extern void fdate_sub (char *, gfc_charlen_type);
export_proto(fdate_sub);

void
fdate_sub (char * date, gfc_charlen_type date_len)
{
  time_t now = time(NULL);
  gf_ctime (date, date_len, now);
}



extern void PREFIX(ctime) (char **, gfc_charlen_type *, GFC_INTEGER_8);
export_proto_np(PREFIX(ctime));

void
PREFIX(ctime) (char ** date, gfc_charlen_type * date_len, GFC_INTEGER_8 t)
{
  time_t now = t;
  *date = xmalloc (CTIME_BUFSZ);
  *date_len = gf_ctime (*date, CTIME_BUFSZ, now);
}


extern void ctime_sub (GFC_INTEGER_8 *, char *, gfc_charlen_type);
export_proto(ctime_sub);

void
ctime_sub (GFC_INTEGER_8 * t, char * date, gfc_charlen_type date_len)
{
  time_t now = *t;
  gf_ctime (date, date_len, now);
}
