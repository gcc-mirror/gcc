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

/**
 * @file gupcr_nb_sup.h
 * GUPC Portals4 non-blocking transfer implementation support routines.
 *
 * @addtogroup NON-BLOCKING GUPCR Non-Blocking Transfer Support Functions
 * @{
 */

#ifndef _GUPCR_NB_SUP_H_
#define _GUPCR_NB_SUP_H_ 1

/** Maximum number of outstanding non-blocking transfers */
#define GUPCR_NB_MAX_OUTSTANDING 128

extern void gupcr_nb_put (size_t, size_t, const void *,
			  size_t, unsigned long *);
extern void gupcr_nb_get (size_t, size_t, char *, size_t,
			  unsigned long *);
extern int gupcr_nb_completed (unsigned long);
extern void gupcr_sync (unsigned long);
extern int gupcr_nbi_outstanding (void);
extern void gupcr_synci (void);
extern void gupcr_nb_init (void);
extern void gupcr_nb_fini (void);

#endif /* gupcr_nb_sup.h */

/** }@ */
