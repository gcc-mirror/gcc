/* Copyright (C) 2002-2019 Free Software Foundation, Inc.
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
   These are out-of-line routines called from gthr-vxworks.h.  */

#include "tconfig.h"
#include "tsystem.h"
#include "gthr.h"

#if defined(__GTHREADS)
#include <vxWorks.h>
#ifndef __RTP__
#include <vxLib.h>
#endif
#include <taskLib.h>
#ifndef __RTP__
#include <taskHookLib.h>
#else
# include <errno.h>
#endif

/* Init-once operation.

   This would be a clone of the implementation from gthr-solaris.h,
   except that we have a bootstrap problem - the whole point of this
   exercise is to prevent double initialization, but if two threads
   are racing with each other, once->mutex is liable to be initialized
   by both.  Then each thread will lock its own mutex, and proceed to
   call the initialization routine.

   So instead we use a bare atomic primitive (vxTas()) to handle
   mutual exclusion.  Threads losing the race then busy-wait, calling
   taskDelay() to yield the processor, until the initialization is
   completed.  Inefficient, but reliable.  */

int
__gthread_once (__gthread_once_t *guard, void (*func)(void))
{
  if (guard->done)
    return 0;

#ifdef __RTP__
  __gthread_lock_library ();
#else
  while (!vxTas ((void *)&guard->busy))
    {
#ifdef __PPC__
      /* This can happen on powerpc, which is using all 32 bits
	 of the gthread_once_t structure.  */
      if (guard->done)
	return 0;
#endif
      taskDelay (1);
    }
#endif

  /* Only one thread at a time gets here.  Check ->done again, then
     go ahead and call func() if no one has done it yet.  */
  if (!guard->done)
    {
      func ();
      guard->done = 1;
    }

#ifdef __RTP__
  __gthread_unlock_library ();
#else
  guard->busy = 0;
#endif
  return 0;
}

#endif /* __GTHREADS */
