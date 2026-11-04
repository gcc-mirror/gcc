/* Copyright (C) 2005-2026 Free Software Foundation, Inc.
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
#ifdef HAVE_MEMALIGN
  {
    extern void *memalign (size_t, size_t);
    ret = memalign (al, size);
  }
#elif defined(HAVE_POSIX_MEMALIGN)
  if (posix_memalign (&ret, al, size) != 0)
    ret = NULL;
#elif defined(HAVE_ALIGNED_ALLOC)
  {
    size_t sz = (size + al - 1) & ~(al - 1);
    if (__builtin_expect (sz >= size, 1))
      ret = aligned_alloc (al, sz);
    else
      ret = NULL;
  }
#elif defined(HAVE__ALIGNED_MALLOC)
  /* Mingw64 doesn't perform overflow checking, so do it ourselves.
     It does power of two checking though.  */
  if (__builtin_add_overflow_p (size, al + 2 * sizeof (void *), (size_t) 0))
    ret = NULL;
  else
    ret = _aligned_malloc (size, al);
#else
  ret = NULL;
  if ((al & (al - 1)) == 0 && size)
    {
      void *p;
      size_t sz;
      if (__builtin_add_overflow (size, al, &sz))
	p = NULL;
      else
	p = malloc (sz);
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
#elif defined(HAVE__ALIGNED_MALLOC)
  _aligned_free (ptr);
#else
  if (ptr)
    free (((void **) ptr)[-1]);
#endif
}

#ifdef LIBGOMP_SIMT
/* Return the largest X of the active lanes, in every lane.  */

static size_t
gomp_simt_max (size_t x)
{
  /* Usually every lane has the same X.  */
  size_t max = SIMT_BROADCAST_FIRST_LANE (x);
  if (!SIMT_VOTE_ANY (x != max))
    return max;

  max = 0;
  for (int bit = sizeof (size_t) * 8 - 1; bit >= 0; bit--)
    {
      size_t c = max | ((size_t) 1 << bit);
      if (SIMT_VOTE_ANY (x >= c))
	max = c;
    }
  return max;
}

/* Allocate SIZE bytes, aligned to AL, for each active lane of the wave, with
   one allocation for all of them.  The first active lane allocates a slot for
   itself and each higher lane, large enough for any of the lanes, and its own
   slot is the start of the block.  Every active lane must call this at once,
   and the same lanes must later free the memory at once, with
   gomp_aligned_free_lanes, which frees the block from the first lane.  */

void *
gomp_aligned_alloc_lanes (size_t al, size_t size)
{
  /* There is nothing to share with a single lane.  */
  if (!SIMT_VOTE_ANY (!FIRST_LANE_P))
    return gomp_aligned_alloc (al, size);

  al = gomp_simt_max (al);
  size_t slot = (gomp_simt_max (size) + al - 1) & -al;
  int first = SIMT_BROADCAST_FIRST_LANE (SIMT_LANE_ID);
  uintptr_t block = 0;
  if (FIRST_LANE_P)
    block = (uintptr_t) gomp_aligned_alloc (al, slot * (SIMT_LANES - first));
  block = SIMT_BROADCAST_FIRST_LANE (block);
  return (void *) (block + (SIMT_LANE_ID - first) * slot);
}

void
gomp_aligned_free_lanes (void *ptr)
{
  if (FIRST_LANE_P)
    gomp_aligned_free (ptr);
}
#endif
