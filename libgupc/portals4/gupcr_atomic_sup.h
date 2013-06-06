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

#ifndef _GUPCR_ATOMIC_SUP_H_
#define _GUPCR_ATOMIC_SUP_H_ 1

/**
 * @file gupcr_atomic_sup.h
 * GUPC Portals4 atomics implementation support routines.
 *
 * @addtogroup ATOMIC GUPCR Atomics Support Functions
 * @{
 */

/** Maximum size of atomic types */
#define GUPC_MAX_ATOMIC_SIZE 16

/** Convert from UPC atomics int to Portals atomic type */
#if __SIZEOF_INT__ == 4
#define UPC_ATOMIC_TO_PTL_INT PTL_INT32_T
#define UPC_ATOMIC_TO_PTL_UINT PTL_UINT32_T
#elif __SIZEOF_INT__ == 8
#define UPC_ATOMIC_TO_PTL_INT PTL_INT64_T
#define UPC_ATOMIC_TO_PTL_UINT PTL_UINT64_T
#else
#error "Size of int not supported"
#endif
/** Convert from UPC atomics long to Portals atomic type */
#if __SIZEOF_LONG__ == 4
#define UPC_ATOMIC_TO_PTL_LONG PTL_INT32_T
#define UPC_ATOMIC_TO_PTL_ULONG PTL_UINT32_T
#elif __SIZEOF_LONG__ == 8
#define UPC_ATOMIC_TO_PTL_LONG PTL_INT64_T
#define UPC_ATOMIC_TO_PTL_ULONG PTL_UINT64_T
#else
#error "Size of long not supported"
#endif
/** Convert from UPC atomic int32 to Portals atomic type */
#define UPC_ATOMIC_TO_PTL_INT32 PTL_INT32_T
#define UPC_ATOMIC_TO_PTL_UINT32 PTL_UINT32_T
/** Convert from UPC atomic int64 to Portals atomic type */
#define UPC_ATOMIC_TO_PTL_INT64 PTL_INT64_T
#define UPC_ATOMIC_TO_PTL_UINT64 PTL_UINT64_T
/** Convert from UPC atomic float to Portals atomic type */
#define UPC_ATOMIC_TO_PTL_FLOAT PTL_FLOAT
/** Convert from UPC atomic double to Portals atomic type */
#define UPC_ATOMIC_TO_PTL_DOUBLE PTL_DOUBLE

/** @} */

void gupcr_atomic_put (size_t, size_t, size_t, ptl_op_t op, ptl_datatype_t);
void gupcr_atomic_get (size_t, size_t, void *, ptl_datatype_t);
void gupcr_atomic_set (size_t, size_t, void *, const void *, ptl_datatype_t);
void gupcr_atomic_cswap (size_t, size_t, void *, const void *,
			 const void *, ptl_datatype_t);
void gupcr_atomic_op (size_t, size_t, void *, const void *,
		      ptl_op_t, ptl_datatype_t);
void gupcr_atomic_init (void);
void gupcr_atomic_fini (void);

#endif /* gupcr_atomic_sup.h */
