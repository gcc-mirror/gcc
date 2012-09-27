/* Copyright (c) 2012
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime library test suite.
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

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/time.h>
#include <unistd.h>
#include <upc.h>
#include <upc_tick.h>


/* Generic timing function.  */
static
uint64_t
ref_get_ns (void)
{
  struct timeval tv;
  uint64_t t;
  if (gettimeofday (&tv, NULL) != 0)
    {
      perror ("gettimeofday");
      abort ();
    }
  t = (uint64_t) tv.tv_sec * 1000000000LL
      + (uint64_t) tv.tv_usec * 1000LL;
  return t;
}

void
test31 (void)
{
  upc_tick_t start, stop, elapsed;
  uint64_t ref_start_ns, ref_stop_ns, ref_elapsed_ns;
  uint64_t elapsed_ns;
  double elapsed_sec, total_elapsed_sec;
  double ref_elapsed_sec, ref_pct_err;
  upc_barrier;
  ref_start_ns = ref_get_ns();
  total_elapsed_sec = 0.0;
  for (int n = 0; n < 100; ++n)
    {
      start = upc_ticks_now ();
      /* Sleep 0.01 seconds */
      usleep (10000L);
      stop = upc_ticks_now ();
      elapsed = (stop - start);
      elapsed_ns = upc_ticks_to_ns (elapsed);
      elapsed_sec = (double) elapsed_ns * 1.0e-9;
      total_elapsed_sec += elapsed_sec;
    }
  ref_stop_ns = ref_get_ns();
  ref_elapsed_ns = (ref_stop_ns - ref_start_ns);
  ref_elapsed_sec = (double) ref_elapsed_ns * 1.0e-9;
  ref_pct_err = 100.0 * (1.0 - total_elapsed_sec / ref_elapsed_sec);
  if (ref_pct_err < 0.0)
   ref_pct_err = -ref_pct_err;
#ifdef DEBUG
  printf ("elapsed: %0.5g ref: %0.5g error: %0.5g\n",
          total_elapsed_sec, ref_elapsed_sec, ref_pct_err);
#endif
  if (ref_pct_err > 3.0)
    {
      fprintf (stderr, "upc_tick differs from reference by "
               "more than 3%% on thread %d\n"
	       "measured error is: %0.1f%%\n",
	       MYTHREAD, ref_pct_err);
      abort ();
    }
  upc_barrier;
  if (!MYTHREAD)
    printf ("test31: upc_tick wall clock timer library test - passed.\n");
}

int
main ()
{
  test31 ();
  return 0;
}
