/* Copyright (C) 1995, 1996, 2001 Free Software Foundation, Inc.
This file is part of GNU Fortran libU77 library.

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with GNU Fortran; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <sys/types.h>
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
#if HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif
#include "f2c.h"

/* may need sys/time.h & long arg for stime (bsd, svr1-3) */

#ifdef KR_headers
/* Character */ void G77_ctime_0 (chtime, Lchtime, xstime)
     char *chtime;
     longint * xstime;
     ftnlen Lchtime;
#else
/* Character */ void G77_ctime_0 (char *chtime, const ftnlen Lchtime, longint * xstime)
#endif
{
  int i, l;
  int s_copy ();
  time_t stime = *xstime;

  /* Allow a length other than 24 for compatibility with what other
     systems do, despite it being documented as 24. */
  s_copy (chtime, ctime (&stime), Lchtime, 24);
}
