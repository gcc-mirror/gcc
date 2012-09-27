/* Copyright (c) 2006, 2007, 2008, 2009, 2010, 2011
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


#ifndef _UPC_LIB_H_
#define _UPC_LIB_H_

/* Definition of user-visible UPC library routines,
   in a form that they can be called from the
   "C"-based runtime.  */

extern size_t upc_threadof (upc_shared_ptr_t);
extern size_t upc_phaseof (upc_shared_ptr_t);
extern upc_shared_ptr_t upc_resetphase (upc_shared_ptr_t);
extern size_t upc_addrfield (upc_shared_ptr_t);
extern size_t upc_affinitysize (size_t, size_t, size_t);

extern void upc_global_exit (int);

extern void upc_memcpy (upc_shared_ptr_t dest, upc_shared_ptr_t src,
			size_t n);
extern void upc_memget (void *dest, upc_shared_ptr_t src, size_t n);
extern void upc_memput (upc_shared_ptr_t dest, const void *src, size_t n);
extern void upc_memset (upc_shared_ptr_t dest, int c, size_t n);

extern upc_shared_ptr_t upc_global_alloc (size_t, size_t);
extern upc_shared_ptr_t upc_all_alloc (size_t, size_t);
extern upc_shared_ptr_t upc_alloc (size_t);
extern void upc_free (upc_shared_ptr_t);
extern void upc_all_free (upc_shared_ptr_t);

extern upc_shared_ptr_t upc_lock_alloc (void);
extern void upc_lock_free (upc_shared_ptr_t);
extern void upc_all_lock_free (upc_shared_ptr_t);
extern upc_shared_ptr_t upc_all_lock_alloc (void);
extern upc_shared_ptr_t upc_global_lock_alloc (void);
extern void upc_lock (upc_shared_ptr_t);
extern int upc_lock_attempt (upc_shared_ptr_t);
extern void upc_unlock (upc_shared_ptr_t);

typedef uint64_t upc_tick_t;
extern upc_tick_t upc_ticks_now(void);
extern uint64_t upc_ticks_to_ns(upc_tick_t ticks);

#endif /* _UPC_LIB_H_ */
