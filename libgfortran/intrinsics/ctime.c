/* Implementation of the CTIME and FDATE g77 intrinsics.
   Copyright (C) 2005, 2007, 2009 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

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

#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  if HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    ifdef HAVE_TIME_H
#      include <time.h>
#    endif
#  endif
#endif

#include <string.h>


extern void fdate (char **, gfc_charlen_type *);
export_proto(fdate);

void
fdate (char ** date, gfc_charlen_type * date_len)
{
#if defined(HAVE_TIME) && defined(HAVE_CTIME)
  int i;
  time_t now = time(NULL);
  *date = ctime (&now);
  if (*date != NULL)
    {
      *date = strdup (*date);
      *date_len = strlen (*date);

      i = 0;
      while ((*date)[i])
       {
         if ((*date)[i] == '\n')
           (*date)[i] = ' ';
         i++;
       }
      return;
    }
#endif

  *date = NULL;
  *date_len = 0;
}


extern void fdate_sub (char *, gfc_charlen_type);
export_proto(fdate_sub);

void
fdate_sub (char * date, gfc_charlen_type date_len)
{
#if defined(HAVE_TIME) && defined(HAVE_CTIME)
  int i;
  char *d;
  time_t now = time(NULL);
#endif
  
  memset (date, ' ', date_len);
#if defined(HAVE_TIME) && defined(HAVE_CTIME)
  d = ctime (&now);
  if (d != NULL)
    {
      i = 0;
      while (*d && *d != '\n' && i < date_len)
       date[i++] = *(d++);
    }
#endif
}



extern void PREFIX(ctime) (char **, gfc_charlen_type *, GFC_INTEGER_8);
export_proto_np(PREFIX(ctime));

void
PREFIX(ctime) (char ** date, gfc_charlen_type * date_len, GFC_INTEGER_8 t)
{
#if defined(HAVE_CTIME)
  time_t now = t;
  int i;
  *date = ctime (&now);
  if (*date != NULL)
    {
      *date = strdup (*date);
      *date_len = strlen (*date);

      i = 0;
      while ((*date)[i])
       {
         if ((*date)[i] == '\n')
           (*date)[i] = ' ';
         i++;
       }
      return;
    }
#endif

  *date = NULL;
  *date_len = 0;
}


extern void ctime_sub (GFC_INTEGER_8 *, char *, gfc_charlen_type);
export_proto(ctime_sub);

void
ctime_sub (GFC_INTEGER_8 * t, char * date, gfc_charlen_type date_len)
{
#if defined(HAVE_CTIME)
  int i;
  char *d;
  time_t now = *t;
#endif
  
  memset (date, ' ', date_len);
#if defined(HAVE_CTIME)
  d = ctime (&now);
  if (d != NULL)
    {
      i = 0;
      while (*d && *d != '\n' && i < date_len)
       date[i++] = *(d++);
    }
#endif
}
