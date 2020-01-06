/* Copyright (C) 2005-2020 Free Software Foundation, Inc.
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

/* This file contains wrappers for the system allocation routines.  Most
   places in the OpenMP API do not make any provision for failure, so in
   general we cannot allow memory allocation to fail.  */

#define _GNU_SOURCE
#include "libgomp.h"
#include <stdlib.h>


void *
gomp_malloc (size_t size)
{
  void *ret = malloc (size);
  if (ret == NULL)
    gomp_fatal ("Out of memory allocating %lu bytes", (unsigned long) size);
  return ret;
}

void *
gomp_malloc_cleared (size_t size)
{
  void *ret = calloc (1, size);
  if (ret == NULL)
    gomp_fatal ("Out of memory allocating %lu bytes", (unsigned long) size);
  return ret;
}

void *
gomp_realloc (void *old, size_t size)
{
  void *ret = realloc (old, size);
  if (ret == NULL)
    gomp_fatal ("Out of memory allocating %lu bytes", (unsigned long) size);
  return ret;
}

void *
gomp_aligned_alloc (size_t al, size_t size)
{
  void *ret;
  if (al < sizeof (void *))
    al = sizeof (void *);
#ifdef HAVE_ALIGNED_ALLOC
  ret = aligned_alloc (al, size);
#elif defined(HAVE__ALIGNED_MALLOC)
  ret = _aligned_malloc (size, al);
#elif defined(HAVE_POSIX_MEMALIGN)
  if (posix_memalign (&ret, al, size) != 0)
    ret = NULL;
#elif defined(HAVE_MEMALIGN)
  {
    extern void *memalign (size_t, size_t);
    ret = memalign (al, size);
  }
#else
  ret = NULL;
  if ((al & (al - 1)) == 0 && size)
    {
      void *p = malloc (size + al);
      if (p)
	{
	  void *ap = (void *) (((uintptr_t) p + al) & -al);
	  ((void **) ap)[-1] = p;
	  ret = ap;
	}
    }
#endif
  if (ret == NULL)
    gomp_fatal ("Out of memory allocating %lu bytes", (unsigned long) size);
  return ret;
}

void
gomp_aligned_free (void *ptr)
{
#ifdef GOMP_HAVE_EFFICIENT_ALIGNED_ALLOC
  free (ptr);
#else
  if (ptr)
    free (((void **) ptr)[-1]);
#endif
}
