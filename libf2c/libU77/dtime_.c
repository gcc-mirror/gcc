/* Copyright (C) 1995, 1996 Free Software Foundation, Inc.
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
#if HAVE_UNISTD_H
#  include <unistd.h>
#endif
#include <sys/types.h>
#if HAVE_SYS_TIMES_H
#  include <sys/times.h>
#endif
#if HAVE_SYS_PARAM_H
#  include <sys/param.h>
#endif
#if HAVE_GETRUSAGE
#  include <sys/time.h>
#  include <sys/resource.h>
#endif
#include <errno.h>		/* for ENOSYS */
#include "f2c.h"

/* For dtime, etime we store the clock tick parameter (clk_tck) the
   first time either of them is invoked rather than each time.  This
   approach probably speeds up each invocation by avoiding a system
   call each time, but means that the overhead of the first call is
   different to all others. */
static long clk_tck = 0;

#ifdef KR_headers
double G77_dtime_0 (tarray)
     real tarray[2];
#else
double G77_dtime_0 (real tarray[2])
#endif
{
#if defined (HAVE_GETRUSAGE) || defined (HAVE_TIMES)
  /* The getrusage version is only the default for convenience. */
#ifdef HAVE_GETRUSAGE
  float utime, stime;
  static float old_utime = 0.0, old_stime = 0.0;
  struct rusage rbuff;

   if (getrusage (RUSAGE_SELF, &rbuff) != 0)
     abort ();
   utime = (float) (rbuff.ru_utime).tv_sec +
	   (float) (rbuff.ru_utime).tv_usec/1000000.0;
   tarray[0] = utime - (float) old_utime;
   stime = (float) (rbuff.ru_stime).tv_sec +
	   (float) (rbuff.ru_stime).tv_usec/1000000.0;
  tarray[1] = stime - old_stime;
#else  /* HAVE_GETRUSAGE */
  time_t utime, stime;
  static time_t old_utime = 0, old_stime = 0;
  struct tms buffer;

/* NeXTStep seems to define _SC_CLK_TCK but not to have sysconf;
   fixme: does using _POSIX_VERSION help? */
#  if defined _SC_CLK_TCK && defined _POSIX_VERSION
  if (! clk_tck) clk_tck = sysconf(_SC_CLK_TCK);
#  elif defined CLOCKS_PER_SECOND
  if (! clk_tck) clk_tck = CLOCKS_PER_SECOND;
#  elif defined CLK_TCK
  if (! clk_tck) clk_tck = CLK_TCK;
#  elif defined HZ
  if (! clk_tck) clk_tck = HZ;
#  elif defined HAVE_GETRUSAGE
#  else
  #error Dont know clock tick length
#  endif
  if (times(&buffer) < 0) return -1.0;
  utime = buffer.tms_utime; stime = buffer.tms_stime;
  tarray[0] = ((float)(utime - old_utime)) / (float)clk_tck;
  tarray[1] = ((float)(stime - old_stime)) / (float)clk_tck;
#endif /* HAVE_GETRUSAGE */
  old_utime = utime; old_stime = stime;
  return (tarray[0]+tarray[1]);
#else /* ! HAVE_GETRUSAGE && ! HAVE_TIMES */
  errno = ENOSYS;
  return 0.0;
#endif /* ! HAVE_GETRUSAGE && ! HAVE_TIMES */
}
