/* Copyright (C) 2005-2023 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file handles the CRITICAL construct.  */

#include "libgomp.h"
#include <stdlib.h>


static gomp_mutex_t default_lock;

void
GOMP_critical_start (void)
{
  /* There is an implicit flush on entry to a critical region. */
  __atomic_thread_fence (MEMMODEL_RELEASE);
  gomp_mutex_lock (&default_lock);
}

void
GOMP_critical_end (void)
{
  gomp_mutex_unlock (&default_lock);
}

#ifndef HAVE_SYNC_BUILTINS
static gomp_mutex_t create_lock_lock;
#endif

void
GOMP_critical_name_start (void **pptr)
{
  gomp_mutex_t *plock;

  /* If a mutex fits within the space for a pointer, and is zero initialized,
     then use the pointer space directly.  */
  if (GOMP_MUTEX_INIT_0
      && sizeof (gomp_mutex_t) <= sizeof (void *)
      && __alignof (gomp_mutex_t) <= sizeof (void *))
    plock = (gomp_mutex_t *)pptr;

  /* Otherwise we have to be prepared to malloc storage.  */
  else
    {
      plock = *pptr;

      if (plock == NULL)
	{
#ifdef HAVE_SYNC_BUILTINS
	  gomp_mutex_t *nlock = gomp_malloc (sizeof (gomp_mutex_t));
	  gomp_mutex_init (nlock);

	  plock = __sync_val_compare_and_swap (pptr, NULL, nlock);
	  if (plock != NULL)
	    {
	      gomp_mutex_destroy (nlock);
	      free (nlock);
	    }
	  else
	    plock = nlock;
#else
	  gomp_mutex_lock (&create_lock_lock);
	  plock = *pptr;
	  if (plock == NULL)
	    {
	      plock = gomp_malloc (sizeof (gomp_mutex_t));
	      gomp_mutex_init (plock);
	      __sync_synchronize ();
	      *pptr = plock;
	    }
	  gomp_mutex_unlock (&create_lock_lock);
#endif
	}
    }

  gomp_mutex_lock (plock);
}

void
GOMP_critical_name_end (void **pptr)
{
  gomp_mutex_t *plock;

  /* If a mutex fits within the space for a pointer, and is zero initialized,
     then use the pointer space directly.  */
  if (GOMP_MUTEX_INIT_0
      && sizeof (gomp_mutex_t) <= sizeof (void *)
      && __alignof (gomp_mutex_t) <= sizeof (void *))
    plock = (gomp_mutex_t *)pptr;
  else
    plock = *pptr;

  gomp_mutex_unlock (plock);
}

#if !GOMP_MUTEX_INIT_0
static void __attribute__((constructor))
initialize_critical (void)
{
  gomp_mutex_init (&default_lock);
#ifndef HAVE_SYNC_BUILTINS
  gomp_mutex_init (&create_lock_lock);
#endif
}
#endif
