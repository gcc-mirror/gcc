/* Implementation of the CTIME and FDATE g77 intrinsics.
   Copyright (C) 2005-2014 Free Software Foundation, Inc.
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

#include <stdlib.h>
#include <string.h>


/* strftime-like function that fills a C string with %c format which
   is identical to ctime in the default locale. As ctime and ctime_r
   are poorly specified and their usage not recommended, the
   implementation instead uses strftime.  */

static size_t
strctime (char *s, size_t max, const time_t *timep)
{
  struct tm ltm;
  int failed;
  /* Some targets provide a localtime_r based on a draft of the POSIX
     standard where the return type is int rather than the
     standardized struct tm*.  */
  __builtin_choose_expr (__builtin_classify_type (localtime_r (timep, &ltm)) 
			 == 5,
			 failed = localtime_r (timep, &ltm) == NULL,
			 failed = localtime_r (timep, &ltm) != 0);
  if (failed)
    return 0;
  return strftime (s, max, "%c", &ltm);
}

/* In the default locale, the date and time representation fits in 26
   bytes. However, other locales might need more space.  */
#define CSZ 100

extern void fdate (char **, gfc_charlen_type *);
export_proto(fdate);

void
fdate (char ** date, gfc_charlen_type * date_len)
{
  time_t now = time(NULL);
  *date = xmalloc (CSZ);
  *date_len = strctime (*date, CSZ, &now);
}


extern void fdate_sub (char *, gfc_charlen_type);
export_proto(fdate_sub);

void
fdate_sub (char * date, gfc_charlen_type date_len)
{
  time_t now = time(NULL);
  char *s = xmalloc (date_len + 1);
  size_t n = strctime (s, date_len + 1, &now);
  fstrcpy (date, date_len, s, n);
  free (s);
}



extern void PREFIX(ctime) (char **, gfc_charlen_type *, GFC_INTEGER_8);
export_proto_np(PREFIX(ctime));

void
PREFIX(ctime) (char ** date, gfc_charlen_type * date_len, GFC_INTEGER_8 t)
{
  time_t now = t;
  *date = xmalloc (CSZ);
  *date_len = strctime (*date, CSZ, &now);
}


extern void ctime_sub (GFC_INTEGER_8 *, char *, gfc_charlen_type);
export_proto(ctime_sub);

void
ctime_sub (GFC_INTEGER_8 * t, char * date, gfc_charlen_type date_len)
{
  time_t now = *t;
  char *s = xmalloc (date_len + 1);
  size_t n = strctime (s, date_len + 1, &now);
  fstrcpy (date, date_len, s, n);
  free (s);
}
