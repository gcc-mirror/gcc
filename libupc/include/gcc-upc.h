/* Copyright (c) 2005, 2006, 2007, 2008, 2009, 2010, 2011
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


#ifndef _GCC_UPC_H_
#define _GCC_UPC_H_

#ifndef NO_GCC_UPC_LIB
/* Include the runtime API.  */
#include <gcc-upc-lib.h>
#endif

#pragma upc upc_code

/* upc_lock_t is an opaque shared type */
typedef shared struct upc_lock_struct upc_lock_t;

#ifndef upc_fence
#define upc_fence { static strict shared int x; x = x; }
#endif

#ifndef upc_poll
/* for now upc_poll is a no-op */
#define upc_poll()
#endif

#ifdef __BERKELEY_UPC_RUNTIME__
#undef upc_fence
#undef upc_poll
#define upc_fence upcr_poll()
#define upc_poll() upcr_poll()
#endif

#ifdef __UPC_USES_PTHREADS__
/* Pthreads implementation uses per thread random seed */
#define rand __upc_rand
#define srand __upc_srand
extern int __upc_rand (void);
extern void __upc_srand (unsigned int seed);
#endif

#endif /* !_GCC_UPC_H_ */
