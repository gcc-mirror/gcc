// Support routines for the -*- C++ -*- dynamic memory management.

// Copyright (C) 1997-2018 Free Software Foundation, Inc.
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

#include <bits/c++config.h>
#include <stdlib.h>
#include <bits/exception_defines.h>
#include "new"

using std::new_handler;
using std::bad_alloc;

#if !_GLIBCXX_HAVE_ALIGNED_ALLOC
#if _GLIBCXX_HAVE__ALIGNED_MALLOC
#define aligned_alloc(al,sz) _aligned_malloc(sz,al)
#elif _GLIBCXX_HAVE_POSIX_MEMALIGN
static inline void*
aligned_alloc (std::size_t al, std::size_t sz)
{
  void *ptr;
  // The value of alignment shall be a power of two multiple of sizeof(void *).
  if (al < sizeof(void*))
    al = sizeof(void*);
  int ret = posix_memalign (&ptr, al, sz);
  if (ret == 0)
    return ptr;
  return nullptr;
}
#elif _GLIBCXX_HAVE_MEMALIGN
#if _GLIBCXX_HOSTED
#include <malloc.h>
#else
extern "C" void *memalign(std::size_t boundary, std::size_t size);
#endif
#define aligned_alloc memalign
#else
#include <stdint.h>
// The C library doesn't provide any aligned allocation functions, define one.
// This is a modified version of code from gcc/config/i386/gmm_malloc.h
static inline void*
aligned_alloc (std::size_t al, std::size_t sz)
{
  // Alignment must be a power of two.
  if (al & (al - 1))
    return nullptr;
  else if (!sz)
    return nullptr;

  // We need extra bytes to store the original value returned by malloc.
  if (al < sizeof(void*))
    al = sizeof(void*);
  void* const malloc_ptr = malloc(sz + al);
  if (!malloc_ptr)
    return nullptr;
  // Align to the requested value, leaving room for the original malloc value.
  void* const aligned_ptr = (void *) (((uintptr_t) malloc_ptr + al) & -al);

  // Store the original malloc value where it can be found by operator delete.
  ((void **) aligned_ptr)[-1] = malloc_ptr;

  return aligned_ptr;
}
#endif
#endif

_GLIBCXX_WEAK_DEFINITION void *
operator new (std::size_t sz, std::align_val_t al)
{
  void *p;
  std::size_t align = (std::size_t)al;

  /* malloc (0) is unpredictable; avoid it.  */
  if (sz == 0)
    sz = 1;

#if _GLIBCXX_HAVE_ALIGNED_ALLOC
# ifdef _AIX
  /* AIX 7.2.0.0 aligned_alloc incorrectly has posix_memalign's requirement
   * that alignment is a multiple of sizeof(void*).  */
  if (align < sizeof(void*))
    align = sizeof(void*);
# endif
  /* C11: the value of size shall be an integral multiple of alignment.  */
  if (std::size_t rem = sz & (align - 1))
    sz += align - rem;
#endif

  while (__builtin_expect ((p = aligned_alloc (align, sz)) == 0, false))
    {
      new_handler handler = std::get_new_handler ();
      if (! handler)
	_GLIBCXX_THROW_OR_ABORT(bad_alloc());
      handler ();
    }

  return p;
}
