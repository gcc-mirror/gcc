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
#include "upc_numa.h"

int
__upc_numa_supported (void)
{
  return 0;
}

int
__upc_numa_init (const upc_info_p ARG_UNUSED (u),
                 const char **ARG_UNUSED (err_msg))
{
  return 1;
}

int
__upc_numa_allocate (const upc_info_p ARG_UNUSED (u),
                     const int ARG_UNUSED (thread_id),
		     int *ARG_UNUSED (sched_affinity),
		     int *ARG_UNUSED (mem_affinity),
		     const char **err_msg)
{
  *err_msg = "UPC error: unable to schedule over nodes - NUMA not available.";
  return 0;
}

void
__upc_numa_sched_set (const upc_info_p ARG_UNUSED (u),
                      const int ARG_UNUSED (thread_id))
{
}

void
__upc_numa_memory_affinity_set (const upc_info_p ARG_UNUSED (u),
                                const int ARG_UNUSED (thread_id))
{
}

void
__upc_numa_memory_region_affinity_set (const upc_info_p ARG_UNUSED (u),
				       const int ARG_UNUSED (thread_id),
				       const void *ARG_UNUSED (region),
				       const size_t ARG_UNUSED (size))
{
}
