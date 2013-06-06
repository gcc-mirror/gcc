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

#ifndef _GUPCR_BROADCAST_H_
#define _GUPCR_BROADCAST_H_

/**
 * @file gupcr_broadcast.h
 * GUPC Portals4 broadcast implementation.
 */

/**
 * @addtogroup BROADCAST GUPCR Broadcast Functions
 * @{
 */

/** Maximum message size that can be sent via broadcast.  */
#define GUPCR_MAX_BROADCAST_SIZE 32

/** @} */

extern void gupcr_broadcast_get (void *value, size_t nbytes);
extern void gupcr_broadcast_put (void *value, size_t nbytes);
extern void gupcr_broadcast_init (void);
extern void gupcr_broadcast_fini (void);

#endif /* gupcr_broadcast.h */
