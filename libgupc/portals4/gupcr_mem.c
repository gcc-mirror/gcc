/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
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

#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_sup.h"
#include "gupcr_access.h"
#include "gupcr_portals.h"
#include "gupcr_node.h"
#include "gupcr_gmem.h"
#include "gupcr_utils.h"

/**
 * @file gupcr_mem.c
 * GUPC Shared string handling functions.
 */

/**
 * @addtogroup UPCSTR UPC Shared string handling functions
 * @{
 */

/**
 * Copy shared memory block.
 *
 * The upc_memcpy function copies n characters from a shared object having
 * affinity with one thread to a shared object having affinity with the same or
 * another thread.
 *
 * @param [in] dest Pointer-to-shared of the destination
 * @param [in] src Pointer-to-shared of the source
 * @param [in] n Number of bytes to transfer
 */
void
upc_memcpy (upc_shared_ptr_t dest, upc_shared_ptr_t src, size_t n)
{
  int sthread = GUPCR_PTS_THREAD (src);
  size_t soffset = GUPCR_PTS_OFFSET (src);
  int dthread = GUPCR_PTS_THREAD (dest);
  size_t doffset = GUPCR_PTS_OFFSET (dest);
  int dthread_local, sthread_local;
  gupcr_trace (FC_MEM, "MEM MEMCPY ENTER %d:0x%lx %d:0x%lx %lu",
	       sthread, (long unsigned) soffset,
	       dthread, (long unsigned) doffset, (long unsigned) n);
  gupcr_assert (dthread < THREADS);
  gupcr_assert (doffset != 0);
  gupcr_assert (sthread < THREADS);
  gupcr_assert (soffset != 0);
  dthread_local = GUPCR_GMEM_IS_LOCAL (dthread);
  sthread_local = GUPCR_GMEM_IS_LOCAL (sthread);
  if (dthread_local && sthread_local)
    memcpy (GUPCR_GMEM_OFF_TO_LOCAL (dthread, doffset),
	    GUPCR_GMEM_OFF_TO_LOCAL (sthread, soffset), n);
  else if (dthread_local)
    {
      gupcr_gmem_get (GUPCR_GMEM_OFF_TO_LOCAL (dthread, doffset),
		      sthread, soffset, n);
      gupcr_gmem_sync_gets ();
    }
  else if (sthread_local)
    {
      if (n > (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (dthread, doffset,
			  GUPCR_GMEM_OFF_TO_LOCAL (sthread, soffset), n);
	  gupcr_pending_strict_put = 1;
	}
      else
	gupcr_gmem_put (dthread, doffset,
			GUPCR_GMEM_OFF_TO_LOCAL (sthread, soffset), n);
    }
  else
    {
      if (n > (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
          gupcr_gmem_sync_puts ();
          gupcr_gmem_copy (dthread, doffset, sthread, soffset, n);
          gupcr_pending_strict_put = 1;
	}
      else
        gupcr_gmem_copy (dthread, doffset, sthread, soffset, n);
    }
  gupcr_trace (FC_MEM, "MEM MEMCPY EXIT");
}

/**
 * Get shared memory block.
 *
 * The upc_memget function copies n characters from a shared object with
 * affinity to any single thread to an object on the calling thread.
 *
 * @param [in] dest Local destination address
 * @param [in] src Pointer-to-shared of the source
 * @param [in] n Number of bytes to transfer
 */
void
upc_memget (void *dest, upc_shared_ptr_t src, size_t n)
{
  int sthread = GUPCR_PTS_THREAD (src);
  size_t soffset = GUPCR_PTS_OFFSET (src);
  gupcr_trace (FC_MEM, "MEM MEMGET ENTER %d:0x%lx 0x%lx %lu",
	       sthread, (long unsigned) soffset,
	       (long unsigned) dest, (long unsigned) n);
  gupcr_assert (sthread < THREADS);
  gupcr_assert (soffset != 0);
  if (GUPCR_GMEM_IS_LOCAL (sthread))
    memcpy (dest, GUPCR_GMEM_OFF_TO_LOCAL (sthread, soffset), n);
  else
    gupcr_gmem_get (dest, sthread, soffset, n);
  gupcr_gmem_sync_gets ();
  gupcr_trace (FC_MEM, "MEM MEMGET EXIT");
}

/**
 * Put shared memory block.
 *
 * The upc_memput function copies n characters from an object on the calling
 * thread to a shared object with affinity to any single thread.
 *
 * @param [in] dest Pointer-to-shared of the destination
 * @param [in] src Local source address
 * @param [in] n Number of bytes to transfer
 */
void
upc_memput (upc_shared_ptr_t dest, const void *src, size_t n)
{
  int dthread = GUPCR_PTS_THREAD (dest);
  size_t doffset = GUPCR_PTS_OFFSET (dest);
  gupcr_trace (FC_MEM, "MEM MEMPUT ENTER 0x%lx %d:0x%lx %lu",
	       (long unsigned) src, dthread, (long unsigned) doffset,
	       (long unsigned) n);
  gupcr_assert (dthread < THREADS);
  gupcr_assert (doffset != 0);
  if (GUPCR_GMEM_IS_LOCAL (dthread))
    memcpy (GUPCR_GMEM_OFF_TO_LOCAL (dthread, doffset), src, n);
  else
    {
      if (n > (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (dthread, doffset, src, n);
	  gupcr_pending_strict_put = 1;
	}
      else
	gupcr_gmem_put (dthread, doffset, src, n);
    }
  gupcr_trace (FC_MEM, "MEM MEMPUT EXIT");
}

/**
 * Set shared memory block.
 *
 * The upc_memset function copies the value of c, converted to an unsigned
 * char, to a shared object with affinity to any single thread.
 *
 * @param [in] dest Pointer-to-shared of the destination
 * @param [in] c Value to set the remote memory
 * @param [in] n Number of bytes to set
 */
void
upc_memset (upc_shared_ptr_t dest, int c, size_t n)
{
  int dthread = GUPCR_PTS_THREAD (dest);
  size_t doffset = GUPCR_PTS_OFFSET (dest);
  gupcr_trace (FC_MEM, "MEM MEMSET ENTER 0x%x %d:0x%lx %lu",
	       c, dthread, (long unsigned) doffset, (long unsigned) n);
  gupcr_assert (dthread < THREADS);
  gupcr_assert (doffset != 0);
  if (GUPCR_GMEM_IS_LOCAL (dthread))
    memset (GUPCR_GMEM_OFF_TO_LOCAL (dthread, doffset), c, n);
  else
    {
      if (n > (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_set (dthread, doffset, c, n);
	  gupcr_pending_strict_put = 1;
	}
      else
	gupcr_gmem_set (dthread, doffset, c, n);
    }
  gupcr_trace (FC_MEM, "MEM MEMSET EXIT");
}

/** @} */
