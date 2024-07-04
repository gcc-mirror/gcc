/* Copyright (C) 2007-2024 Free Software Foundation, Inc.
   Contributed by Danny Smith <dannysmith@users.sourceforge.net>

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

   The following implementation uses win32 API routines.  */

#include "libgomp.h"
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

/* Count the CPU's currently available to this process.  */
static unsigned int
count_avail_process_cpus ()
{
  DWORD_PTR process_cpus;
  DWORD_PTR system_cpus;

  if (GetProcessAffinityMask (GetCurrentProcess (),
			      &process_cpus, &system_cpus))
    {
      unsigned int count;
      for (count = 0; process_cpus != 0; process_cpus >>= 1)  
	if (process_cpus & 1)
	  count++;
      return count;
    }
  return 1;
}

/* At startup, determine the default number of threads.  It would seem
   this should be related to the number of cpus available to the process.  */

void
gomp_init_num_threads (void)
{
  gomp_global_icv.nthreads_var = count_avail_process_cpus ();
}

/* When OMP_DYNAMIC is set, at thread launch determine the number of
   threads we should spawn for this team.  FIXME:  How do we adjust for
   load average on MS Windows?  */

unsigned
gomp_dynamic_max_threads (void)
{
  unsigned int n_onln = count_avail_process_cpus ();
  unsigned int nthreads_var = gomp_icv (false)->nthreads_var;
  return n_onln > nthreads_var ? nthreads_var : n_onln;
}

int
omp_get_num_procs (void)
{
  return count_avail_process_cpus ();
}

ialias (omp_get_num_procs)
