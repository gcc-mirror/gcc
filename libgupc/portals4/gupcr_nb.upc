/* Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

/**
 * @file gupcr_nb.upc
 * GUPC Portals4 Non-Blocking Transfers Operations.
 *
 * @addtogroup UPC-NON-BLOCKING UPC Non-Blocking Transfer Operations
 * @{
 */

#include <upc.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <upc_nb.h>
#include <portals4.h>
#include "gupcr_gmem.h"
#include "gupcr_utils.h"
#include "gupcr_portals.h"
#include "gupcr_nb_sup.h"

/**
 * Copy memory with non-blocking explicit handle transfer
 *
 * @param[in] dst Destination shared memory pointer
 * @param[in] src Source shared memory pointer
 * @param[in] n Number of bytes to transfer
 * @retval UPC non-blocking transfer handle
 */
upc_handle_t
upc_memcpy_nb (shared void *restrict dst,
	       shared const void *restrict src, size_t n)
{
  size_t sthread, soffset;
  size_t dthread, doffset;
  upc_handle_t handle;
  gupcr_trace (FC_NB, "NB MEMCOPY_NB ENTER");
  sthread = upc_threadof ((shared void *) src);
  dthread = upc_threadof ((shared void *) dst);
  if ((int) sthread == MYTHREAD)
    {
      doffset = upc_addrfield (dst);
      gupcr_nb_put (dthread, doffset, (char *) src, n, &handle);
    }
  else if ((int) dthread == MYTHREAD)
    {
      soffset = upc_addrfield ((shared void *) src);
      gupcr_nb_get (sthread, soffset, (char *) dst, n, &handle);
    }
  else
    {
      /* Third party copying.  Just use upc_memcpy().  */
      upc_memcpy (dst, src, n);
      handle = UPC_COMPLETE_HANDLE; 
    }
  gupcr_trace (FC_NB, "NB MEMCOPY_NB EXIT");
  return handle;
}

/**
 * Get memory with non-blocking explicit handle transfer
 *
 * @param[in] dst Destination local memory pointer
 * @param[in] src Source remote memory pointer
 * @param[in] n Number of bytes to transfer
 * @retval UPC non-blocking transfer handle
 */
upc_handle_t
upc_memget_nb (void *restrict dst,
	       shared const void *restrict src, size_t n)
{
  gupcr_trace (FC_NB, "NB MEMGET_NB ENTER");
  upc_handle_t handle = UPC_COMPLETE_HANDLE;
  if (n)
    {
      size_t sthread, soffset;
      sthread = upc_threadof ((shared void *) src);
      soffset = upc_addrfield ((shared void *) src);
      gupcr_nb_get (sthread, soffset, dst, n, &handle);
    }
  gupcr_trace (FC_NB, "NB MEMGET_NB EXIT");
  return handle;
}

/**
 * Put memory with non-blocking explicit handle transfer
 *
 * @param[in] dst Destination remote memory pointer
 * @param[in] src Source local memory pointer
 * @param[in] n Number of bytes to transfer
 * @retval UPC non-blocking transfer handle
 */
upc_handle_t
upc_memput_nb (shared void *restrict dst,
	       const void *restrict src, size_t n)
{
  gupcr_trace (FC_NB, "NB MEMPUT_NB ENTER");
  upc_handle_t handle = UPC_COMPLETE_HANDLE;
  if (n)
    {
      size_t dthread, doffset;
      dthread = upc_threadof ((shared void *) dst);
      doffset = upc_addrfield ((shared void *) dst);
      gupcr_nb_put (dthread, doffset, src, n, &handle);
    }
  gupcr_trace (FC_NB, "NB MEMPUT_NB EXIT");
  return handle;
}

/**
 * Set memory with non-blocking implicit handle transfer
 *
 * @param[in] dst Shared remote pointer
 * @param[in] c Value for set operation
 * @param[in] n Number of bytes to set
 * @retval UPC non-blocking transfer handle
 *
 * This call completes the transfer before returning.
 */
upc_handle_t
upc_memset_nb (shared void *dst, int c, size_t n)
{
  gupcr_trace (FC_NB, "NB MEMSET_NB ENTER");
  if (n)
    {
      upc_memset (dst, c, n);
      upc_fence;
    }
  gupcr_trace (FC_NB, "NB MEMSET_NB EXIT");
  return UPC_COMPLETE_HANDLE;
}

/**
 * Explicit handle non-blocking transfer sync attempt
 *
 * @param[in] handle Transfer explicit handle
 * @retval UPC_NB_COMPLETED returned if transfer completed,
 *	   otherwise UPC_NB_NOT_COMPLETED
 */
int
upc_sync_attempt (upc_handle_t handle)
{
  int comp;
  gupcr_trace (FC_NB, "NB SYNC_ATTEMPT ENTER");
  if (handle == UPC_COMPLETE_HANDLE
    || gupcr_nb_completed (handle))
    comp = UPC_NB_COMPLETED;
  else
    comp = UPC_NB_NOT_COMPLETED;
  gupcr_trace (FC_NB, "NB SYNC_ATTEMPT EXIT");
  return comp;
}

/**
 * Explicit handle non-blocking transfer sync
 *
 * @param[in] handle Non-blocking transfer explicit handle
 */
void
upc_sync (upc_handle_t handle)
{
  gupcr_trace (FC_NB, "NB SYNC ENTER");
  if (handle != UPC_COMPLETE_HANDLE)
    gupcr_sync (handle);
  gupcr_trace (FC_NB, "NB SYNC EXIT");
}

/**
 * Copy memory with non-blocking implicit handle transfer
 *
 * @param[in] dst Shared remote memory pointer
 * @param[in] src Shared remote memory pointer
 * @param[in] n Number of bytes to transfer
 */
void
upc_memcpy_nbi (shared void *restrict dst,
		shared const void *restrict src, size_t n)
{
  size_t sthread, soffset;
  size_t dthread, doffset;
  gupcr_trace (FC_NB, "NB MEMCOPY_NBI ENTER");
  sthread = upc_threadof ((shared void *) src);
  dthread = upc_threadof ((shared void *) dst);
  if ((int) sthread == MYTHREAD)
    {
      doffset = upc_addrfield (dst);
      gupcr_nb_put (dthread, doffset, (char *) src, n, NULL);
    }
  else if ((int) dthread == MYTHREAD)
    {
      soffset = upc_addrfield ((shared void *) src);
      gupcr_nb_get (sthread, soffset, (char *) dst, n, NULL);
    }
  else
    {
      /* Third party copying.  Just use upc_memcpy().  */
      upc_memcpy (dst, src, n);
    }
  gupcr_trace (FC_NB, "NB MEMCOPY_NBI EXIT");
}

/**
 * Get memory with non-blocking implicit handle transfer
 *
 * @param[in] dst Local memory pointer
 * @param[in] src Shared remote memory pointer
 * @param[in] n Number of bytes to transfer
 */
void
upc_memget_nbi (void *restrict dst,
		shared const void *restrict src, size_t n)
{
  gupcr_trace (FC_NB, "NB MEMGET_NBI ENTER");
  if (n)
    {
      size_t sthread, soffset;
      sthread = upc_threadof ((shared void *) src);
      soffset = upc_addrfield ((shared void *) src);
      gupcr_nb_get (sthread, soffset, dst, n, NULL);
    }
  gupcr_trace (FC_NB, "NB MEMGET_NBI EXIT");
}

/**
 * Put memory with non-blocking implicit handle transfer
 *
 * @param[in] dst Shared remote memory pointer
 * @param[in] src Local memory pointer
 * @param[in] n Number of bytes to transfer
 */
void
upc_memput_nbi (shared void *restrict dst,
		const void *restrict src, size_t n)
{
  gupcr_trace (FC_NB, "NB MEMPUT_NBI ENTER");
  if (n)
    {
      size_t dthread, doffset;
      dthread = upc_threadof ((shared void *) dst);
      doffset = upc_addrfield ((shared void *) dst);
      gupcr_nb_put (dthread, doffset, src, n, NULL);
    }
  gupcr_trace (FC_NB, "NB MEMPUT_NBI EXIT");
}

/**
 * Set memory with non-blocking implicit handle transfer
 *
 * @param[in] dst Shared remote pointer
 * @param[in] c Value for set operation
 * @param[in] n Number of bytes to set
 */
void
upc_memset_nbi (shared void *dst, int c, size_t n)
{
  gupcr_trace (FC_NB, "NB MEMSET_NBI ENTER");
  if (n)
    {
      upc_memset (dst, c, n);
      upc_fence;
    }
  gupcr_trace (FC_NB, "NB MEMSET_NBI EXIT");
}

/**
 * Check on implicit handle non-blocking transfers
 *
 * @retval UPC_NB_COMPLETED if no transfers pending, otherwise
 *         UPC_NB_NOT_COMPLETED is returned
 */
int
upc_synci_attempt (void)
{
  int result;
  gupcr_trace (FC_NB, "NB SYNCI ATTEMPT ENTER");
  if (gupcr_nbi_outstanding ())
    result = UPC_NB_NOT_COMPLETED;
  else
    result = UPC_NB_COMPLETED;
  gupcr_trace (FC_NB, "NB SYNCI ATTEMPT EXIT");
  return result;
}

/**
 * Complete implicit handle non-blocking transfers
 */
void
upc_synci (void)
{
  gupcr_trace (FC_NB, "NB SYNCI ENTER");
  gupcr_synci ();
  gupcr_trace (FC_NB, "NB SYNCI EXIT");
}

/** @} */
