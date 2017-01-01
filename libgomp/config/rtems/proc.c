/* Copyright (C) 2015-2017 Free Software Foundation, Inc.
   Contributed by Sebastian Huber <sebastian.huber@embedded-brains.de>.

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

/* This file contains RTEMS specific routines related to counting
   online processors and dynamic load balancing.  */

#include "libgomp.h"
#include "pool.h"
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

struct gomp_thread_pool_reservoir **gomp_thread_pool_reservoirs;

__thread struct gomp_tls_rtems_data gomp_tls_rtems_data;

static void
allocate_thread_pool_reservoirs (void)
{
  struct gomp_thread_pool_reservoir **reservoirs;
  size_t size = _Sched_Count () * sizeof (*reservoirs);
  reservoirs = gomp_malloc (size);
  gomp_thread_pool_reservoirs = reservoirs;
  memset (reservoirs, 0, size);
}

static void
allocate_thread_pool_reservoir (unsigned long count, unsigned long priority,
				unsigned long scheduler)
{
  struct gomp_thread_pool_reservoir *res;
  struct gomp_thread_pool *pools;
  unsigned long i;
  size_t size;

  res = gomp_thread_pool_reservoirs[scheduler];
  if (res != NULL)
    gomp_fatal ("Multiple thread pool reservoir initialization");
  size = sizeof (*res) + count * (sizeof(pools) + sizeof(*pools));
  pools = gomp_malloc (size);
  memset (pools, 0, size);
  res = (struct gomp_thread_pool_reservoir *) (pools + count);
  res->index = count;
  res->priority = priority;
  gomp_sem_init (&res->available, count);
  pthread_spin_init (&res->lock, PTHREAD_PROCESS_PRIVATE);
  for (i = 0; i < count; ++i)
    res->pools[i] = &pools[i];
  gomp_thread_pool_reservoirs[scheduler] = res;
}

static char *
parse_thread_pools (char *env, unsigned long *count, unsigned long *priority,
		    unsigned long *scheduler)
{
  size_t len;
  int i;

  if (*env == ':')
    ++env;

  errno = 0;
  *count = strtoul (env, &env, 10);
  if (errno != 0)
    gomp_fatal ("Invalid thread pool count");

  if (*env == '$')
    {
      ++env;
      errno = 0;
      *priority = strtoul (env, &env, 10);
      if (errno != 0)
	gomp_fatal ("Invalid thread pool priority");
    }
  else
    *priority = -1;

  if (*env != '@')
    gomp_fatal ("Invalid thread pool scheduler prefix");
  ++env;

  len = 0;
  while (env[len] != '\0' && env[len] != ':')
    ++len;
  i = _Sched_Name_to_index (env, len);
  if (i < 0)
    gomp_fatal ("Invalid thread pool scheduler");
  *scheduler = i;
  env += len;

  return env;
}

static void
init_thread_pool_reservoirs (void)
{
  char *env = getenv ("GOMP_RTEMS_THREAD_POOLS");
  if (env != NULL)
    {
      allocate_thread_pool_reservoirs ();
      while (*env != '\0')
	{
	  unsigned long count;
	  unsigned long priority;
	  unsigned long scheduler;
	  env = parse_thread_pools (env, &count, &priority, &scheduler);
	  allocate_thread_pool_reservoir (count, priority, scheduler);
	}
    }
}

void
gomp_init_num_threads (void)
{
  gomp_global_icv.nthreads_var = omp_get_num_procs();
  init_thread_pool_reservoirs ();
}

unsigned
gomp_dynamic_max_threads (void)
{
  unsigned n_onln = (unsigned) omp_get_num_procs();
  unsigned nthreads_var = gomp_icv (false)->nthreads_var;

  if (n_onln > nthreads_var)
    return nthreads_var;
  else
    return n_onln;
}

int
omp_get_num_procs (void)
{
  return sysconf (_SC_NPROCESSORS_ONLN);
}

ialias (omp_get_num_procs)
