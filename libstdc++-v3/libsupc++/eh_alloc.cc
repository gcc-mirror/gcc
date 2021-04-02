// -*- C++ -*- Allocate exception objects.
// Copyright (C) 2001-2021 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

// This is derived from the C++ ABI for IA-64.  Where we diverge
// for cross-architecture compatibility are noted with "@@@".

#include <bits/c++config.h>
#include <cstdlib>
#if _GLIBCXX_HOSTED
#include <cstring>
#endif
#include <climits>
#include <exception>
#include "unwind-cxx.h"
#include <ext/concurrence.h>
#include <new>

#if _GLIBCXX_HOSTED
using std::free;
using std::malloc;
using std::memset;
#else
// In a freestanding environment, these functions may not be available
// -- but for now, we assume that they are.
extern "C" void *malloc (std::size_t);
extern "C" void free(void *);
extern "C" void *memset (void *, int, std::size_t);
#endif

using namespace __cxxabiv1;

// ??? How to control these parameters.

// Guess from the size of basic types how large a buffer is reasonable.
// Note that the basic c++ exception header has 13 pointers and 2 ints,
// so on a system with PSImode pointers we're talking about 56 bytes
// just for overhead.

#if INT_MAX == 32767
# define EMERGENCY_OBJ_SIZE	128
# define EMERGENCY_OBJ_COUNT	16
#elif !defined (_GLIBCXX_LLP64) && LONG_MAX == 2147483647
# define EMERGENCY_OBJ_SIZE	512
# define EMERGENCY_OBJ_COUNT	32
#else
# define EMERGENCY_OBJ_SIZE	1024
# define EMERGENCY_OBJ_COUNT	64
#endif

#ifndef __GTHREADS
# undef EMERGENCY_OBJ_COUNT
# define EMERGENCY_OBJ_COUNT	4
#endif

namespace __gnu_cxx
{
  void __freeres();
}

namespace
{
  // A fixed-size heap, variable size object allocator
  class pool
    {
    public:
      pool();

      _GLIBCXX_NODISCARD void *allocate (std::size_t);
      void free (void *);

      bool in_pool (void *);

    private:
      struct free_entry {
	std::size_t size;
	free_entry *next;
      };
      struct allocated_entry {
	std::size_t size;
	char data[] __attribute__((aligned));
      };

      // A single mutex controlling emergency allocations.
      __gnu_cxx::__mutex emergency_mutex;

      // The free-list
      free_entry *first_free_entry;
      // The arena itself - we need to keep track of these only
      // to implement in_pool.
      char *arena;
      std::size_t arena_size;

      friend void __gnu_cxx::__freeres();
    };

  pool::pool()
    {
      // Allocate the arena - we could add a GLIBCXX_EH_ARENA_SIZE environment
      // to make this tunable.
      arena_size = (EMERGENCY_OBJ_SIZE * EMERGENCY_OBJ_COUNT
		    + EMERGENCY_OBJ_COUNT * sizeof (__cxa_dependent_exception));
      arena = (char *)malloc (arena_size);
      if (!arena)
	{
	  // If the allocation failed go without an emergency pool.
	  arena_size = 0;
	  first_free_entry = NULL;
	  return;
	}

      // Populate the free-list with a single entry covering the whole arena
      first_free_entry = reinterpret_cast <free_entry *> (arena);
      new (first_free_entry) free_entry;
      first_free_entry->size = arena_size;
      first_free_entry->next = NULL;
    }

  void *pool::allocate (std::size_t size)
    {
      __gnu_cxx::__scoped_lock sentry(emergency_mutex);
      // We need an additional size_t member plus the padding to
      // ensure proper alignment of data.
      size += offsetof (allocated_entry, data);
      // And we need to at least hand out objects of the size of
      // a freelist entry.
      if (size < sizeof (free_entry))
	size = sizeof (free_entry);
      // And we need to align objects we hand out to the maximum
      // alignment required on the target (this really aligns the
      // tail which will become a new freelist entry).
      size = ((size + __alignof__ (allocated_entry::data) - 1)
	      & ~(__alignof__ (allocated_entry::data) - 1));
      // Search for an entry of proper size on the freelist.
      free_entry **e;
      for (e = &first_free_entry;
	   *e && (*e)->size < size;
	   e = &(*e)->next)
	;
      if (!*e)
	return NULL;
      allocated_entry *x;
      if ((*e)->size - size >= sizeof (free_entry))
	{
	  // Split block if it is too large.
	  free_entry *f = reinterpret_cast <free_entry *>
	      (reinterpret_cast <char *> (*e) + size);
	  std::size_t sz = (*e)->size;
	  free_entry *next = (*e)->next;
	  new (f) free_entry;
	  f->next = next;
	  f->size = sz - size;
	  x = reinterpret_cast <allocated_entry *> (*e);
	  new (x) allocated_entry;
	  x->size = size;
	  *e = f;
	}
      else
	{
	  // Exact size match or too small overhead for a free entry.
	  std::size_t sz = (*e)->size;
	  free_entry *next = (*e)->next;
	  x = reinterpret_cast <allocated_entry *> (*e);
	  new (x) allocated_entry;
	  x->size = sz;
	  *e = next;
	}
      return &x->data;
    }

  void pool::free (void *data)
    {
      __gnu_cxx::__scoped_lock sentry(emergency_mutex);
      allocated_entry *e = reinterpret_cast <allocated_entry *>
	(reinterpret_cast <char *> (data) - offsetof (allocated_entry, data));
      std::size_t sz = e->size;
      if (!first_free_entry
	  || (reinterpret_cast <char *> (e) + sz
	      < reinterpret_cast <char *> (first_free_entry)))
	{
	  // If the free list is empty or the entry is before the
	  // first element and cannot be merged with it add it as
	  // the first free entry.
	  free_entry *f = reinterpret_cast <free_entry *> (e);
	  new (f) free_entry;
	  f->size = sz;
	  f->next = first_free_entry;
	  first_free_entry = f;
	}
      else if (reinterpret_cast <char *> (e) + sz
	       == reinterpret_cast <char *> (first_free_entry))
	{
	  // Check if we can merge with the first free entry being right
	  // after us.
	  free_entry *f = reinterpret_cast <free_entry *> (e);
	  new (f) free_entry;
	  f->size = sz + first_free_entry->size;
	  f->next = first_free_entry->next;
	  first_free_entry = f;
	}
      else
	{
	  // Else search for a free item we can merge with at its end.
	  free_entry **fe;
	  for (fe = &first_free_entry;
	       (*fe)->next
	       && (reinterpret_cast <char *> ((*fe)->next)
		   > reinterpret_cast <char *> (e) + sz);
	       fe = &(*fe)->next)
	    ;
	  // If we can merge the next block into us do so and continue
	  // with the cases below.
	  if (reinterpret_cast <char *> (e) + sz
	      == reinterpret_cast <char *> ((*fe)->next))
	    {
	      sz += (*fe)->next->size;
	      (*fe)->next = (*fe)->next->next;
	    }
	  if (reinterpret_cast <char *> (*fe) + (*fe)->size
	      == reinterpret_cast <char *> (e))
	    // Merge with the freelist entry.
	    (*fe)->size += sz;
	  else
	    {
	      // Else put it after it which keeps the freelist sorted.
	      free_entry *f = reinterpret_cast <free_entry *> (e);
	      new (f) free_entry;
	      f->size = sz;
	      f->next = (*fe)->next;
	      (*fe)->next = f;
	    }
	}
    }

  bool pool::in_pool (void *ptr)
    {
      char *p = reinterpret_cast <char *> (ptr);
      return (p > arena
	      && p < arena + arena_size);
    }

  pool emergency_pool;
}

namespace __gnu_cxx
{
  void
  __freeres()
  {
    if (emergency_pool.arena)
      {
	::free(emergency_pool.arena);
	emergency_pool.arena = 0;
      }
  }
}

extern "C" void *
__cxxabiv1::__cxa_allocate_exception(std::size_t thrown_size) _GLIBCXX_NOTHROW
{
  void *ret;

  thrown_size += sizeof (__cxa_refcounted_exception);
  ret = malloc (thrown_size);

  if (!ret)
    ret = emergency_pool.allocate (thrown_size);

  if (!ret)
    std::terminate ();

  memset (ret, 0, sizeof (__cxa_refcounted_exception));

  return (void *)((char *)ret + sizeof (__cxa_refcounted_exception));
}


extern "C" void
__cxxabiv1::__cxa_free_exception(void *vptr) _GLIBCXX_NOTHROW
{
  char *ptr = (char *) vptr - sizeof (__cxa_refcounted_exception);
  if (emergency_pool.in_pool (ptr))
    emergency_pool.free (ptr);
  else
    free (ptr);
}


extern "C" __cxa_dependent_exception*
__cxxabiv1::__cxa_allocate_dependent_exception() _GLIBCXX_NOTHROW
{
  __cxa_dependent_exception *ret;

  ret = static_cast<__cxa_dependent_exception*>
    (malloc (sizeof (__cxa_dependent_exception)));

  if (!ret)
    ret = static_cast <__cxa_dependent_exception*>
      (emergency_pool.allocate (sizeof (__cxa_dependent_exception)));

  if (!ret)
    std::terminate ();

  memset (ret, 0, sizeof (__cxa_dependent_exception));

  return ret;
}


extern "C" void
__cxxabiv1::__cxa_free_dependent_exception
  (__cxa_dependent_exception *vptr) _GLIBCXX_NOTHROW
{
  if (emergency_pool.in_pool (vptr))
    emergency_pool.free (vptr);
  else
    free (vptr);
}
