/* Copyright (C) 2014-2025 Free Software Foundation, Inc.
   Contributed by Kai Tietz <ktietz@redhat.com>.

   This file is part of the GNU Atomic Library (libatomic).

   Libatomic is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libatomic is distributed in the hope that it will be useful, but WITHOUT ANY
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

#define UWORD __shadow_UWORD
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef UWORD
#include "libatomic_i.h"

/* The target page size.  Must be no larger than the runtime page size,
   lest locking fail with virtual address aliasing (i.e. a page mmaped
   at two locations).  */
#ifndef PAGE_SIZE
#define PAGE_SIZE 4096
#endif

/* The target cacheline size.  This is an optimization; the padding that
   should be applied to the locks to keep them from interfering.  */
#ifndef CACHLINE_SIZE
#define CACHLINE_SIZE 64
#endif

/* The granularity at which locks are applied.  Almost certainly the
   cachline size is the right thing to use here.  */
#ifndef WATCH_SIZE
#define WATCH_SIZE CACHLINE_SIZE
#endif

struct lock
{
  HANDLE mutex;
  char pad[sizeof (HANDLE) < CACHLINE_SIZE
	   ? CACHLINE_SIZE - sizeof (HANDLE)
	   : 0];
};

#define NLOCKS (PAGE_SIZE / WATCH_SIZE)

static struct lock locks[NLOCKS] = {
  [0 ... NLOCKS-1].mutex = NULL
};

static inline uintptr_t 
addr_hash (void *ptr)
{
  return ((uintptr_t)ptr / WATCH_SIZE) % NLOCKS;
}

void
libat_lock_1 (void *ptr)
{
  if (!locks[addr_hash (ptr)].mutex)
    locks[addr_hash (ptr)].mutex = CreateMutex  (NULL, FALSE, NULL);
  WaitForSingleObject (locks[addr_hash (ptr)].mutex, INFINITE);
}

void
libat_unlock_1 (void *ptr)
{
  if (locks[addr_hash (ptr)].mutex)
    ReleaseMutex (locks[addr_hash (ptr)].mutex);
}

void
libat_lock_n (void *ptr, size_t n)
{
  uintptr_t h = addr_hash (ptr);
  size_t i = 0;
  size_t nlocks
    = (n + ((uintptr_t)ptr % WATCH_SIZE) + WATCH_SIZE - 1) / WATCH_SIZE;

  /* Don't lock more than all the locks we have.  */
  if (nlocks > NLOCKS)
    nlocks = NLOCKS;

  if (__builtin_expect (h + nlocks > NLOCKS, 0))
    {
      size_t j = h + nlocks - NLOCKS;
      for (; i < j; ++i)
	{
	  if (!locks[i].mutex)
	    locks[i].mutex = CreateMutex (NULL, FALSE, NULL);
	  WaitForSingleObject (locks[i].mutex, INFINITE);
	}
    }

  for (; i < nlocks; ++i)
    {
      if (!locks[h].mutex)
	locks[h].mutex = CreateMutex (NULL, FALSE, NULL);
      WaitForSingleObject (locks[h++].mutex, INFINITE);
    }
}

void
libat_unlock_n (void *ptr, size_t n)
{
  uintptr_t h = addr_hash (ptr);
  size_t i = 0;
  size_t nlocks
    = (n + ((uintptr_t)ptr % WATCH_SIZE) + WATCH_SIZE - 1) / WATCH_SIZE;

  /* Don't lock more than all the locks we have.  */
  if (nlocks > NLOCKS)
    nlocks = NLOCKS;

  if (__builtin_expect (h + nlocks > NLOCKS, 0))
    {
      size_t j = h + nlocks - NLOCKS;
      for (; i < j; ++i)
	if (locks[i].mutex)
	  ReleaseMutex (locks[i].mutex);
    }

  for (; i < nlocks; ++i, ++h)
    if (locks[h].mutex)
      ReleaseMutex (locks[h].mutex);
}
