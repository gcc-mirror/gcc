/* Copyright (C) 2002-2021 Free Software Foundation, Inc.
   Contributed by Zack Weinberg <zack@codesourcery.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Threads compatibility routines for libgcc2 for VxWorks.

   This file implements the init-once service exposed by gthr-vxworks.h.  */

#include "tconfig.h"
#include "tsystem.h"
#include "gthr.h"

#if defined(__GTHREADS)

#include <vxWorks.h>
#include <taskLib.h>

#ifndef __RTP__
# include <vxLib.h>
# include <taskHookLib.h>
#else /* __RTP__ */
# include <errno.h>
#endif /* __RTP__ */

/* ----------------------------- Init-once ------------------------------- */

static void
__release (__gthread_once_t ** __guard)
{
  (*__guard)->busy = 0;
}

int
__gthread_once (__gthread_once_t * __guard, void (*__func) (void))
{
  if (__guard->done)
    return 0;

  /* Busy-wait until we have exclusive access to the state.  Check if
     another thread managed to perform the init call in the interim.  */
  
  while (!__TAS(&__guard->busy))
    {
      if (__guard->done)
	return 0;
      taskDelay (1);
    }

  if (!__guard->done)
    {
#ifndef __USING_SJLJ_EXCEPTIONS__
      /* Setup a cleanup to release the guard when __func() throws an
	 exception.  We cannot use this with SJLJ exceptions as
	 Unwind_Register calls __gthread_once, leading to an infinite
	 recursion.  */
      __attribute__ ((cleanup (__release)))
	__gthread_once_t *__temp = __guard;
#endif

      __func ();
      __guard->done = 1;
    }

  __release(&__guard);
  return 0;
}

#endif /* __GTHREADS */
