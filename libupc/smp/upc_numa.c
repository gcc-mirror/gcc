/* Copyright (C) 2008 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

#include <numa.h>
#include "upc_config.h"
#include "upc_sysdep.h"
#include "upc_defs.h"
#include "upc_sup.h"
#include "upc_affinity.h"

int
__upc_numa_supported (void)
{
  return 1;
}

int
__upc_numa_init (const upc_info_p u, const char **ARG_UNUSED (err_msg))
{
  u->num_nodes = numa_max_node () + 1;
  return 1;
}

int
__upc_numa_allocate (const upc_info_p u, const int thread_id,
		     int *sched_affinity, int *mem_affinity,
		     const char **ARG_UNUSED (err_msg))
{
  int pelem;
  /* schedule threads over nodes */
  pelem = thread_id % u->num_nodes;
  *sched_affinity = pelem;
  *mem_affinity = pelem;
  return 1;
}

/* Set node scheduling policy. */

void
__upc_numa_sched_set (const upc_info_p u, const int thread_id)
{
#if defined(LIBNUMA_API_VERSION) && (LIBNUMA_API_VERSION==2)
  struct bitmask *set = numa_allocate_cpumask(); 
  numa_node_to_cpus (u->thread_info[thread_id].sched_affinity, set);
  if (numa_sched_setaffinity (0, set))
    {
      __upc_fatal (GUPCR_SCHED_ERROR_MSG);
    }
#else
  cpu_set_t set;
  CPU_ZERO (&set);
  numa_node_to_cpus (u->thread_info[thread_id].sched_affinity,
		     (unsigned long *) &set, sizeof (set));
  if (sched_setaffinity (0, sizeof (set), &set))
    {
      __upc_fatal (GUPCR_SCHED_ERROR_MSG);
    }
#endif
}

/* Set memory allocation policy */

void
__upc_numa_memory_affinity_set (const upc_info_p u, const int thread_id)
{
  if (u->mem_policy == GUPCR_MEM_POLICY_NODE)
    {
      numa_set_preferred (u->thread_info[thread_id].mem_affinity);
    }
  else if (u->mem_policy == GUPCR_MEM_POLICY_STRICT)
    {
#if defined(LIBNUMA_API_VERSION) && (LIBNUMA_API_VERSION==2)
      struct bitmask *nodemask = numa_bitmask_alloc(u->num_nodes);
      numa_bitmask_setbit (nodemask, u->thread_info[thread_id].mem_affinity);
      numa_set_membind (nodemask);
      numa_bitmask_free (nodemask);
#else
      nodemask_t nodemask;
      nodemask_zero (&nodemask);
      nodemask_set (&nodemask, u->thread_info[thread_id].mem_affinity);
      numa_set_membind (&nodemask);
#endif
    }
}

/* Set affinity for memory region */

void
__upc_numa_memory_region_affinity_set (const upc_info_p u,
				       const int thread_id,
				       const void *region, const size_t size)
{
  if ((u->sched_policy != GUPCR_SCHED_POLICY_AUTO) &&
         (u->mem_policy != GUPCR_MEM_POLICY_AUTO))
    {
      /* memory is being allocated with affinity to "thread" */
      if (thread_id == MYTHREAD)
	numa_tonode_memory ((void *) region, size,
			    u->thread_info[thread_id].mem_affinity);
    }
}
