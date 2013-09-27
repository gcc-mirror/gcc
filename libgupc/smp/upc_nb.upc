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
#include <upc_nb.h>

/**
 * Copy memory with non-blocking explicit handle transfer.
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
  upc_memcpy (dst, src, n);
  return UPC_COMPLETE_HANDLE;
}

/**
 * Get memory with non-blocking explicit handle transfer.
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
  upc_memget (dst, src, n);
  return UPC_COMPLETE_HANDLE;
}

/**
 * Put memory with non-blocking explicit handle transfer.
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
  upc_memput (dst, src, n);
  return UPC_COMPLETE_HANDLE;
}

/**
 * Set memory with non-blocking implicit handle transfer.
 *
 * @param[in] dst Shared remote pointer
 * @param[in] c Value for set operation
 * @param[in] n Number of bytes to set
 * @retval UPC non-blocking transfer handle
 */
upc_handle_t
upc_memset_nb (shared void *dst, int c, size_t n)
{
  upc_memset (dst, c, n);
  return UPC_COMPLETE_HANDLE;
}

/**
 * Explicit handle non-blocking transfer sync attempt.
 *
 * @param[in] handle Transfer explicit handle
 * @retval UPC_NB_COMPLETED returned if transfer completed,
 *	   otherwise UPC_NB_NOT_COMPLETED
 */
int
upc_sync_attempt (upc_handle_t ARG_UNUSED(handle))
{
  return UPC_NB_COMPLETED;
}

/**
 * Explicit handle non-blocking transfer sync.
 *
 * @param[in] handle Non-blocking transfer explicit handle
 */
void
upc_sync (upc_handle_t ARG_UNUSED(handle))
{
}

/**
 * Copy memory with non-blocking implicit handle transfer.
 *
 * @param[in] dst Shared remote memory pointer
 * @param[in] src Shared remote memory pointer
 * @param[in] n Number of bytes to transfer
 */
void
upc_memcpy_nbi (shared void *restrict dst,
		shared const void *restrict src, size_t n)
{
  upc_memcpy (dst, src, n);
}

/**
 * Get memory with non-blocking implicit handle transfer.
 *
 * @param[in] dst Local memory pointer
 * @param[in] src Shared remote memory pointer
 * @param[in] n Number of bytes to transfer
 */
void
upc_memget_nbi (void *restrict dst,
		shared const void *restrict src, size_t n)
{
  upc_memget (dst, src, n);
}

/**
 * Put memory with non-blocking implicit handle transfer.
 *
 * @param[in] dst Shared remote memory pointer
 * @param[in] src Local memory pointer
 * @param[in] n Number of bytes to transfer
 */
void
upc_memput_nbi (shared void *restrict dst,
		const void *restrict src, size_t n)
{
  upc_memput (dst, src, n);
}

/**
 * Set memory with non-blocking implicit handle transfer.
 *
 * @param[in] dst Shared remote pointer
 * @param[in] c Value for set operation
 * @param[in] n Number of bytes to set
 */
void
upc_memset_nbi (shared void *dst, int c, size_t n)
{
  upc_memset (dst, c, n);
}

/**
 * Check on implicit handle non-blocking transfers.
 *
 * @retval UPC_NB_COMPLETED if no transfers pending, otherwise
 *         UPC_NB_NOT_COMPLETED is returned
 */
int
upc_synci_attempt (void)
{
  return UPC_NB_COMPLETED;
}

/**
 * Complete implicit handle non-blocking transfers.
 */
void
upc_synci (void)
{
}
