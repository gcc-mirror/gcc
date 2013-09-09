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

/* UPC Non-Blocking Transfer Operations */

#ifndef _UPC_NB_H_
#define _UPC_NB_H_

/* Sync attempt return values.  */
#define UPC_NB_NOT_COMPLETED 0
#define UPC_NB_COMPLETED 1

/* UPC non-blocking handle.  */
typedef unsigned long upc_handle_t;
#define UPC_COMPLETE_HANDLE (unsigned long) 0

/* Non-blocking memory transfers with explicit handle.  */
extern upc_handle_t upc_memcpy_nb (
	shared void *restrict dst, shared const void *restrict src, size_t n);
extern upc_handle_t upc_memget_nb (
	void *restrict dst, shared const void *restrict src, size_t n);
extern upc_handle_t upc_memput_nb (
	shared void *restrict dst, const void *restrict src, size_t n);
extern upc_handle_t upc_memset_nb (shared void *dst, int c, size_t n);
extern int upc_sync_attempt (upc_handle_t handle);
extern void upc_sync (upc_handle_t handle);

/* Non-blocking memory transfers with implicit handle.  */
extern void
upc_memcpy_nbi (shared void *restrict dst,
		shared const void *restrict src, size_t n);
extern void
upc_memget_nbi (void *restrict dst,
		shared const void *restrict src, size_t n);
extern void
upc_memput_nbi (shared void *restrict dst,
		const void *restrict src, size_t n);
extern void upc_memset_nbi (shared void *dst, int c, size_t n);
extern int upc_synci_attempt (void);
extern void upc_synci (void);

#endif /* !_UPC_NB_H_ */
