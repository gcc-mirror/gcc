/* Copyright (C) 2005-2020 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file contains system specific routines related to counting
   online processors and dynamic load balancing.  It is expected that
   a system may well want to write special versions of each of these.

   The following implementation uses a mix of POSIX and BSD routines.  */

#include "libgomp.h"
#include <unistd.h>
#include <stdlib.h>
#ifdef HAVE_GETLOADAVG
# ifdef HAVE_SYS_LOADAVG_H
#  include <sys/loadavg.h>
# endif
#endif


/* At startup, determine the default number of threads.  It would seem
   this should be related to the number of cpus online.  */

void
gomp_init_num_threads (void)
{
#ifdef _SC_NPROCESSORS_ONLN
  gomp_global_icv.nthreads_var = sysconf (_SC_NPROCESSORS_ONLN);
#endif
}

/* When OMP_DYNAMIC is set, at thread launch determine the number of
   threads we should spawn for this team.  */
/* ??? I have no idea what best practice for this is.  Surely some
   function of the number of processors that are *still* online and
   the load average.  Here I use the number of processors online
   minus the 15 minute load average.  */

unsigned
gomp_dynamic_max_threads (void)
{
  unsigned n_onln, loadavg;
  unsigned nthreads_var = gomp_icv (false)->nthreads_var;

#ifdef _SC_NPROCESSORS_ONLN
  n_onln = sysconf (_SC_NPROCESSORS_ONLN);
  if (n_onln > nthreads_var)
    n_onln = nthreads_var;
#else
  n_onln = nthreads_var;
#endif

  loadavg = 0;
#ifdef HAVE_GETLOADAVG
  {
    double dloadavg[3];
    if (getloadavg (dloadavg, 3) == 3)
      {
	/* Add 0.1 to get a kind of biased rounding.  */
	loadavg = dloadavg[2] + 0.1;
      }
  }
#endif

  if (loadavg >= n_onln)
    return 1;
  else
    return n_onln - loadavg;
}

int
omp_get_num_procs (void)
{
#ifdef _SC_NPROCESSORS_ONLN
  return sysconf (_SC_NPROCESSORS_ONLN);
#else
  return gomp_icv (false)->nthreads_var;
#endif
}

ialias (omp_get_num_procs)
