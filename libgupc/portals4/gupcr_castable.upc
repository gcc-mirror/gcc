/* Copyright (C) 2013 Free Software Foundation, Inc.
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

#include <upc.h>
#include <upc_castable.h>
#include "gupcr_portals.h"
#include "gupcr_pts.h"
#include "gupcr_gmem.h"
#include "gupcr_node.h"

void *
upc_cast (const shared void *ptr)
{
  const upc_shared_ptr_t sptr = GUPCR_PTS_TO_REP (ptr);
  void *local_ptr = NULL;
  if (!GUPCR_PTS_IS_NULL (sptr))
    {
      const size_t thread = GUPCR_PTS_THREAD (sptr);
      const int thread_as_int = (int) thread;
      if (thread_as_int >= THREADS)
	gupcr_fatal_error ("thread number %d in shared address "
	                   "is out of range", thread_as_int);
      if (GUPCR_GMEM_IS_LOCAL (thread))
	{
	  size_t offset = GUPCR_PTS_OFFSET (sptr);
	  local_ptr = GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
	}
    }
  return local_ptr;
}

upc_thread_info_t
upc_thread_info (size_t thread)
{
  const int thread_as_int = (int) thread;
  upc_thread_info_t cast_info = { 0, 0 };
  if (thread_as_int >= THREADS)
    gupcr_fatal_error ("thread number %d in shared address "
		       "is out of range", thread_as_int);
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      cast_info.guaranteedCastable = UPC_CASTABLE_ALL;
      cast_info.probablyCastable = UPC_CASTABLE_ALL;
    }
  return cast_info;
}
