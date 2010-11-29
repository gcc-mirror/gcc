/* Copyright (C) 2001-2004 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

#ifndef _GCC_UPC_H_
#define _GCC_UPC_H_

#ifndef NO_GCC_UPC_LIB
/* Include the runtime API.  */
#include <gcc-upc-lib.h>
#endif

#pragma upc upc_code

/* upc_lock_t is an opaque shared type */
typedef shared struct upc_lock_struct upc_lock_t;

/* The following pre-processor definitions are required by section 7.1
   in the UPC specification. */
#define barrier upc_barrier
#define barrier_notify upc_notify
#define barrier_wait upc_wait
#define forall upc_forall
#define fence upc_fence

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
