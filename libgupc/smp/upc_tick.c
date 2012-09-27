/* Copyright (C) 2012
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
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

#include "upc_config.h"
#include "upc_sysdep.h"
#include "upc_defs.h"
#include "upc_lib.h"

#if HAVE_CLOCK_GETTIME
#ifdef CLOCK_MONOTONIC_RAW
/* System clock id passed to clock_gettime. CLOCK_MONOTONIC_RAW
   is preferred.  It has been available in the Linux kernel
   since version 2.6.28 */
#define SYS_RT_CLOCK_ID CLOCK_MONOTONIC_RAW
#else
#define SYS_RT_CLOCK_ID CLOCK_MONOTONIC
#endif

upc_tick_t
upc_ticks_now (void)
{
  struct timespec ts;
  upc_tick_t t;
  if (clock_gettime (SYS_RT_CLOCK_ID, &ts) != 0)
    {
      perror ("clock_gettime");
      abort ();
    }
  t = (upc_tick_t) ts.tv_sec * 1000000000LL + (upc_tick_t) ts.tv_nsec;
  return t;
}

#else /* !HAVE_CLOCK_GETTIME */

upc_tick_t
upc_ticks_now (void)
{
  struct timeval tv;
  upc_tick_t t;
  if (gettimeofday (&tv, NULL) != 0)
    {
      perror ("gettimeofday");
      abort ();
    }
  t = tv.tv_sec * 1000000000 + tv.tv_usec * 1000;
  return t;
}

#endif

uint64_t
upc_ticks_to_ns (upc_tick_t ticks)
{
  return ticks;
}
