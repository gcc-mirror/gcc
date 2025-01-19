// -*- C++ -*- Allocate exception objects.
// Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

#ifndef _GNU_SOURCE
// Cygwin needs this for secure_getenv
# define _GNU_SOURCE 1
#endif

#include <exception>           // std::exception
#include <new>                 // std::terminate
#include <cstdlib>             // std::malloc, std::free, std::strtoul
#include <climits>             // INT_MAX
#include <bits/stl_function.h> // std::less
#include "unwind-cxx.h"
#if _GLIBCXX_HOSTED
# include <string_view>        // std::string_view
# include <cstring>            // std::strchr, std::memset
# include <ext/concurrence.h>  // __gnu_cxx::__mutex, __gnu_cxx::__scoped_lock
#endif

// We use an emergency buffer used for exceptions when malloc fails.
// If _GLIBCXX_EH_POOL_STATIC is defined (e.g. by configure) then we use
// a fixed-size static buffer. Otherwise, allocate on startup using malloc.
//
// The size of the buffer is N * (S * P + R + D), where:
// N == The number of objects to reserve space for.
//      Defaults to EMERGENCY_OBJ_COUNT, defined below.
// S == Estimated size of exception objects to account for.
//      This size is in units of sizeof(void*) not bytes.
//      Defaults to EMERGENCY_OBJ_SIZE, defined below.
// P == sizeof(void*).
// R == sizeof(__cxa_refcounted_exception).
// D == sizeof(__cxa_dependent_exception).
//
// This provides space for N thrown exceptions of S words each, and an
// additional N dependent exceptions from std::rethrow_exception.
//
// The calculation allows values of N and S to be target-independent,
// as the size will be scaled by the size of basic types on the target,
// and space for the C++ exception header (__cxa_refcounted_exception)
// is added automatically.
//
// For a dynamically allocated buffer, N and S can be set from the environment.
// Setting N=0 will disable the emergency buffer.
// The GLIBCXX_TUNABLES environment variable will be checked for the following:
// - Tunable glibcxx.eh_pool.obj_count overrides EMERGENCY_OBJ_COUNT.
// - Tunable glibcxx.eh_pool.obj_size overrides EMERGENCY_OBJ_SIZE.

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

// Assume that 6 * sizeof(void*) is a reasonable exception object size.
// Throwing very many large objects will exhaust the pool more quickly.
// N.B. sizeof(std::bad_alloc) == sizeof(void*)
// and sizeof(std::runtime_error) == 2 * sizeof(void*)
// and sizeof(std::system_error) == 4 * sizeof(void*).
#define EMERGENCY_OBJ_SIZE	6

#ifdef __GTHREADS
// Assume that the number of concurrent exception objects scales with the
// processor word size, i.e., 16-bit systems are not likely to have hundreds
// of threads all simultaneously throwing on OOM conditions.
# define EMERGENCY_OBJ_COUNT	(4 * __SIZEOF_POINTER__ * __SIZEOF_POINTER__)
# define MAX_OBJ_COUNT          (16 << __SIZEOF_POINTER__)
#else
# define EMERGENCY_OBJ_COUNT	4
# define MAX_OBJ_COUNT          64
#endif

// This can be set by configure.
#ifdef _GLIBCXX_EH_POOL_NOBJS
# if _GLIBCXX_EH_POOL_NOBJS > MAX_OBJ_COUNT
#  warning "_GLIBCXX_EH_POOL_NOBJS value is too large; ignoring it"
# elif _GLIBCXX_EH_POOL_NOBJS < 0
#  warning "_GLIBCXX_EH_POOL_NOBJS value is negative; ignoring it"
# else
#  undef EMERGENCY_OBJ_COUNT
#  define EMERGENCY_OBJ_COUNT _GLIBCXX_EH_POOL_NOBJS
# endif
#endif

#if defined _GLIBCXX_EH_POOL_STATIC && EMERGENCY_OBJ_COUNT == 0
# define USE_POOL 0
#else
# define USE_POOL 1
#endif

#if USE_POOL
namespace __gnu_cxx
{
  void __freeres() noexcept;
}

namespace
{
  static constexpr std::size_t
  buffer_size_in_bytes(std::size_t obj_count, std::size_t obj_size) noexcept
  {
    // N * (S * P + R + D)
    constexpr std::size_t P = sizeof(void*);
    constexpr std::size_t R = sizeof(__cxa_refcounted_exception);
    constexpr std::size_t D = sizeof(__cxa_dependent_exception);
    return obj_count * (obj_size * P + R + D);
  }

  // A fixed-size heap, variable size object allocator
  class pool
    {
    public:
      pool() noexcept;

      _GLIBCXX_NODISCARD void *allocate (std::size_t) noexcept;
      void free (void *) noexcept;

      bool in_pool (void *) const noexcept;

    private:
      struct free_entry {
	std::size_t size;
	free_entry *next;
      };
      struct allocated_entry {
	std::size_t size;
	char data[] __attribute__((aligned));
      };

#if _GLIBCXX_HOSTED
      // A single mutex controlling emergency allocations.
      __gnu_cxx::__mutex emergency_mutex;
      using __scoped_lock = __gnu_cxx::__scoped_lock;
#else
      int emergency_mutex = 0;
      struct __scoped_lock { explicit __scoped_lock(int) { } };
#endif

      // The free-list
      free_entry *first_free_entry = nullptr;
      // The arena itself - we need to keep track of these only
      // to implement in_pool.
#ifdef _GLIBCXX_EH_POOL_STATIC
      static constexpr std::size_t arena_size
	= buffer_size_in_bytes(EMERGENCY_OBJ_COUNT, EMERGENCY_OBJ_SIZE);
      alignas(void*) char arena[arena_size];
#else
      char *arena = nullptr;
      std::size_t arena_size = 0;
#endif

      friend void __gnu_cxx::__freeres() noexcept;
    };

  pool::pool() noexcept
    {
#ifndef _GLIBCXX_EH_POOL_STATIC
      int obj_size = EMERGENCY_OBJ_SIZE;
      int obj_count = EMERGENCY_OBJ_COUNT;

#if _GLIBCXX_HOSTED
#if _GLIBCXX_HAVE_SECURE_GETENV
      const char* str = ::secure_getenv("GLIBCXX_TUNABLES");
#else
      const char* str = std::getenv("GLIBCXX_TUNABLES");
#endif
      const std::string_view ns_name = "glibcxx.eh_pool";
      std::pair<std::string_view, int> tunables[]{
	{"obj_size", 0}, {"obj_count", obj_count}
      };
      while (str)
	{
	  if (*str == ':')
	    ++str;

	  if (!ns_name.compare(0, ns_name.size(), str, ns_name.size())
		&& str[ns_name.size()] == '.')
	  {
	    str += ns_name.size() + 1;
	    for (auto& t : tunables)
	      if (!t.first.compare(0, t.first.size(), str, t.first.size())
		  && str[t.first.size()] == '=')
	      {
		str += t.first.size() + 1;
		char* end;
		unsigned long val = strtoul(str, &end, 0);
		if ((*end == ':' || *end == '\0') && val <= INT_MAX)
		  t.second = val;
		str = end;
		break;
	      }
	  }
	  str = strchr(str, ':');
	}
      obj_count = std::min(tunables[1].second, MAX_OBJ_COUNT); // Can be zero.
      if (tunables[0].second != 0)
	obj_size = tunables[0].second;
#endif // HOSTED

      arena_size = buffer_size_in_bytes(obj_count, obj_size);
      if (arena_size == 0)
	return;
      arena = (char *)malloc (arena_size);
      if (!arena)
	{
	  // If the allocation failed go without an emergency pool.
	  arena_size = 0;
	  return;
	}
#endif // STATIC

      // Populate the free-list with a single entry covering the whole arena
      first_free_entry = reinterpret_cast <free_entry *> (arena);
      new (first_free_entry) free_entry;
      first_free_entry->size = arena_size;
      first_free_entry->next = NULL;
    }

  void *pool::allocate (std::size_t size) noexcept
    {
      __scoped_lock sentry(emergency_mutex);
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

  void pool::free (void *data) noexcept
    {
      __scoped_lock sentry(emergency_mutex);
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
	       && (reinterpret_cast <char *> (e) + sz
		   > reinterpret_cast <char *> ((*fe)->next));
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

  inline bool pool::in_pool (void *ptr) const noexcept
    {
      std::less<const void*> less;
      return less(ptr, arena + arena_size) && less(arena, ptr);
    }

  pool emergency_pool;
}

namespace __gnu_cxx
{
  __attribute__((cold))
  void
  __freeres() noexcept
  {
#ifndef _GLIBCXX_EH_POOL_STATIC
    if (emergency_pool.arena)
      {
	::free(emergency_pool.arena);
	emergency_pool.arena = 0;
      }
#endif
  }
}
#endif // USE_POOL

extern "C" void *
__cxxabiv1::__cxa_allocate_exception(std::size_t thrown_size) noexcept
{
  thrown_size += sizeof (__cxa_refcounted_exception);

  void *ret = malloc (thrown_size);

#if USE_POOL
  if (!ret)
    ret = emergency_pool.allocate (thrown_size);
#endif

  if (!ret)
    std::terminate ();

  memset (ret, 0, sizeof (__cxa_refcounted_exception));

  return (void *)((char *)ret + sizeof (__cxa_refcounted_exception));
}


extern "C" void
__cxxabiv1::__cxa_free_exception(void *vptr) noexcept
{
  char *ptr = (char *) vptr - sizeof (__cxa_refcounted_exception);
#if USE_POOL
  if (emergency_pool.in_pool (ptr)) [[__unlikely__]]
    emergency_pool.free (ptr);
  else
#endif
    free (ptr);
}


extern "C" __cxa_dependent_exception*
__cxxabiv1::__cxa_allocate_dependent_exception() noexcept
{
  void *ret = malloc (sizeof (__cxa_dependent_exception));

#if USE_POOL
  if (!ret)
    ret = emergency_pool.allocate (sizeof (__cxa_dependent_exception));
#endif

  if (!ret)
    std::terminate ();

  memset (ret, 0, sizeof (__cxa_dependent_exception));

  return static_cast<__cxa_dependent_exception*>(ret);
}


extern "C" void
__cxxabiv1::__cxa_free_dependent_exception
  (__cxa_dependent_exception *vptr) noexcept
{
#if USE_POOL
  if (emergency_pool.in_pool (vptr)) [[__unlikely__]]
    emergency_pool.free (vptr);
  else
#endif
    free (vptr);
}
