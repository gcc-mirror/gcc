// Allocators -*- C++ -*-

// Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#ifndef _NEW_ALLOCATOR_H
#define _NEW_ALLOCATOR_H 1

#include <new>

namespace __gnu_cxx
{
  /**
   *  @if maint
   *  A new-based allocator, as required by the standard.  Allocation and
   *  deallocation forward to global new and delete.  "SGI" style, minus
   *  reallocate().
   *  @endif
   *  (See @link Allocators allocators info @endlink for more.)
   */
  class __new_alloc
  {
  public:
    static void*
    allocate(size_t __n)
    { return ::operator new(__n); }

    static void
    deallocate(void* __p, size_t)
    { ::operator delete(__p); }
  };
} // namespace __gnu_cxx

#endif
