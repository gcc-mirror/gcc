/* Copyright (c) 2008, 2009, 2010, 2011
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime library.
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
#include "upc_sup.h"
#include "upc_affinity.h"
#include "upc_numa.h"

/* The opaque type upc_cpu_avoid_t, forward references this type.  */
struct upc_cpu_avoid_struct
{
  cpu_set_t cpu_set;
};

int
__upc_affinity_supported ()
{
  return 1;
}

/* Calculate the right affinity for the thread based on 
   the currnet scheduling and NUMA policies.  Executed by
   the monitoring thread.  AVOID is a pointer to a data
   structure that lists cpu's that aren't eligible for
   allocation.  */

int
__upc_affinity_init (upc_info_p u, upc_cpu_avoid_p avoid,
                     const char **err_msg)
{
  const upc_sched_policy_t sched_policy = u->sched_policy;
  const int num_cpus = u->num_cpus;
  const int num_nodes = u->num_nodes;
  int t, next_sched_cpu;
  if (!__upc_numa_init (u, err_msg))
    return 0;
  /* Calculate affinity for each thread. */
  for (t = 0, next_sched_cpu = 0; t < THREADS; ++t)
    {
      const upc_thread_info_p tinfo = &u->thread_info[t];
      int pelem, melem;
      int sched_affinity, mem_affinity;
      switch (sched_policy)
	{
	case GUPCR_SCHED_POLICY_CPU:
	  /* One cpu = multiple threads */
	  pelem = t % num_cpus;
	  melem = pelem % num_nodes;
	  sched_affinity = pelem;
	  mem_affinity = melem;
	  break;
	case GUPCR_SCHED_POLICY_CPU_STRICT:
	  /* One cpu = one thread */
	  while (CPU_ISSET (next_sched_cpu, &avoid->cpu_set)
		 && next_sched_cpu < num_cpus)
	    {
	      next_sched_cpu += 1;
	    }
	  if (next_sched_cpu >= num_cpus)
	    {
	      *err_msg = "UPC error: unable to allocate CPU for all threads.";
	      return 0;
	    }
	  pelem = next_sched_cpu;
	  melem = pelem % num_nodes;
	  sched_affinity = pelem;
	  mem_affinity = melem;
	  next_sched_cpu += 1;
	  break;
	case GUPCR_SCHED_POLICY_NODE:
	  /* Use NUMA for node scheduling */
	  if (!__upc_numa_allocate (u, t, &sched_affinity, &mem_affinity,
                                    err_msg))
	    return 0;
	  break;
	default:
	  /* Auto scheduling */
	  sched_affinity = -1;
	  mem_affinity = -1;
	}
      tinfo->sched_affinity = sched_affinity;
      tinfo->mem_affinity = mem_affinity;
    }
  return 1;
}

/* Allocate, and return a pointer to the data structure used to record
   the list of CPU's that are unavailable.  */

upc_cpu_avoid_p
__upc_affinity_cpu_avoid_new ()
{
  upc_cpu_avoid_p avoid;
  avoid = (upc_cpu_avoid_p) calloc (1, sizeof (upc_cpu_avoid_t));
  if (!avoid)
    {
      perror ("calloc");
      abort ();
    }
  return avoid;
}

/* Free the previously allocated data structure that is used
   to record list of CPU's that are unavailable.  */

void
__upc_affinity_cpu_avoid_free (const upc_cpu_avoid_p avoid)
{
  if (avoid)
    free ((void *) avoid);
}

/* Mark CPU as being unavailable for allocation.  */

void
__upc_affinity_cpu_avoid_set (const int cpu, const upc_cpu_avoid_p avoid)
{
  CPU_SET (cpu, &avoid->cpu_set);
}

#ifdef DEBUG_AFFINITY
static const char *
upc_sched_policy_to_string (const upc_sched_policy_t sched_policy)
{
  switch (sched_policy)
    {
    case GUPCR_SCHED_POLICY_AUTO:
      return "sched auto";
    case GUPCR_SCHED_POLICY_NODE:
      return "sched node";
    case GUPCR_SCHED_POLICY_CPU:
      return "sched cpu";
    case GUPCR_SCHED_POLICY_CPU_STRICT:
      return "sched strict";
    }
  return "sched <unknown>";
}

static const char *
upc_mem_policy_to_string (const upc_mem_policy_t mem_policy)
{
  switch (mem_policy)
    {
    case GUPCR_MEM_POLICY_AUTO:
      return "mem auto";
    case GUPCR_MEM_POLICY_NODE:
      return "mem node";
    case GUPCR_MEM_POLICY_STRICT:
      return "mem strict";
    }
  return "mem <unknown>";
}
#endif /* DEBUG_AFFINITY */

/* Set thread's affinity based on the pre-calculated
   policies. Executed by each thread as the first thing after thread
   is created.  */

void
__upc_affinity_set (upc_info_p u, int thread_id)
{
  const upc_thread_info_p tinfo = &u->thread_info[thread_id];
  switch (u->sched_policy)
    {
    case GUPCR_SCHED_POLICY_CPU:
    case GUPCR_SCHED_POLICY_CPU_STRICT:
      {
	const int sched_affinity = tinfo->sched_affinity;
	cpu_set_t set;
	CPU_ZERO (&set);
	CPU_SET (sched_affinity, &set);
	if (sched_setaffinity (0, sizeof (set), &set))
	  {
	    __upc_fatal ("Scheduling cannot be set");
	  }
      }
      break;
    case GUPCR_SCHED_POLICY_NODE:
      __upc_numa_sched_set (u, thread_id);
      break;
    default:
      /* auto - no scheduling support */
      break;
    }
  /* set memory policy only if we are not AUTO scheduling */
  if ((u->sched_policy != GUPCR_SCHED_POLICY_AUTO) &&
      (u->mem_policy != GUPCR_MEM_POLICY_AUTO))
    __upc_numa_memory_affinity_set (u, thread_id);
#ifdef DEBUG_AFFINITY
  printf ("affinity: %d (%s,%s) scheduling (%d,%d)\n", thread_id,
	  upc_sched_policy_to_string (u->sched_policy),
	  upc_mem_policy_to_string (u->mem_policy),
	  tinfo->sched_affinity, tinfo->mem_affinity);
#endif /* DEBUG_AFFINITY */
}
