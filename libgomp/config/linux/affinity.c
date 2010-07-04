/* Copyright (C) 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

   This file is part of the GNU OpenMP Library (libgomp).

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

/* This is a Linux specific implementation of a CPU affinity setting.  */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#include "libgomp.h"
#include <sched.h>
#include <stdlib.h>
#include <unistd.h>

#ifdef HAVE_PTHREAD_AFFINITY_NP

static unsigned int affinity_counter;

void
gomp_init_affinity (void)
{
  cpu_set_t cpuset, cpusetnew;
  size_t idx, widx;
  unsigned long cpus = 0;

  if (pthread_getaffinity_np (pthread_self (), sizeof (cpuset), &cpuset))
    {
      gomp_error ("could not get CPU affinity set");
      free (gomp_cpu_affinity);
      gomp_cpu_affinity = NULL;
      gomp_cpu_affinity_len = 0;
      return;
    }

  CPU_ZERO (&cpusetnew);
  for (widx = idx = 0; idx < gomp_cpu_affinity_len; idx++)
    if (gomp_cpu_affinity[idx] < CPU_SETSIZE
        && CPU_ISSET (gomp_cpu_affinity[idx], &cpuset))
      {
	if (! CPU_ISSET (gomp_cpu_affinity[idx], &cpusetnew))
	  {
	    cpus++;
	    CPU_SET (gomp_cpu_affinity[idx], &cpusetnew);
	  }
	gomp_cpu_affinity[widx++] = gomp_cpu_affinity[idx];
      }

  if (widx == 0)
    {
      gomp_error ("no CPUs left for affinity setting");
      free (gomp_cpu_affinity);
      gomp_cpu_affinity = NULL;
      gomp_cpu_affinity_len = 0;
      return;
    }

  gomp_cpu_affinity_len = widx;
  if (cpus < gomp_available_cpus)
    gomp_available_cpus = cpus;
  CPU_ZERO (&cpuset);
  CPU_SET (gomp_cpu_affinity[0], &cpuset);
  pthread_setaffinity_np (pthread_self (), sizeof (cpuset), &cpuset);
  affinity_counter = 1;
}

void
gomp_init_thread_affinity (pthread_attr_t *attr)
{
  unsigned int cpu;
  cpu_set_t cpuset;

  cpu = __sync_fetch_and_add (&affinity_counter, 1);
  cpu %= gomp_cpu_affinity_len;
  CPU_ZERO (&cpuset);
  CPU_SET (gomp_cpu_affinity[cpu], &cpuset);
  pthread_attr_setaffinity_np (attr, sizeof (cpu_set_t), &cpuset);
}

#else

#include "../posix/affinity.c"

#endif
