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
