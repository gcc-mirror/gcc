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

#ifndef _GUPCR_LOCK_SUP_H_
#define _GUPCR_LOCK_SUP_H_

/**
 * @file gupcr_lock_sup.h
 * GUPC Portals4 lock implementation support routines.
 */

extern void gupcr_lock_init (void);
extern void gupcr_lock_fini (void);
extern int gupcr_lock_cswap (size_t, size_t, void *, void *, size_t);
extern void gupcr_lock_swap (size_t, size_t, void *, void *, size_t);
extern void gupcr_lock_put (size_t, size_t, void *, size_t);
extern void gupcr_lock_get (size_t, size_t, void *, size_t);
extern void gupcr_lock_wait (void);

/* See: gupcr_alloc.upc */
extern void gupcr_lock_link_init (void);
extern void gupcr_lock_free_init (void);
extern void gupcr_lock_heap_sup_init (void);

#endif /* gupcr_lock_sup.h */
