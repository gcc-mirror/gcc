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

/*
 * Copyright (c) 1996-1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/** @file ext/debug_allocator.h
 *  This file is a GNU extension to the Standard C++ Library.
 *  You should only include this header if you are using GCC 3 or later.
 */

#ifndef _DEBUG_ALLOCATOR_H
#define _DEBUG_ALLOCATOR_H 1

#include <memory>

namespace __gnu_cxx
{
  /**
   *  @brief  A meta-allocator with debugging bits, as per [20.4].
   *
   *  This is precisely the allocator defined in the C++ Standard. 
   *    - all allocation calls operator new
   *    - all deallocation calls operator delete
   *
   *  (See @link Allocators allocators info @endlink for more.)
   */
  template<typename _Alloc>
    class debug_allocator
    {
    public:
      typedef typename _Alloc::size_type       	size_type;
      typedef typename _Alloc::difference_type	difference_type;
      typedef typename _Alloc::pointer       	pointer;
      typedef typename _Alloc::const_pointer    const_pointer;
      typedef typename _Alloc::reference       	reference;
      typedef typename _Alloc::const_reference  const_reference;
      typedef typename _Alloc::value_type       value_type;

    private:
      // Size of space used to store size.  Note that this must be
      // large enough to preserve alignment.
      const size_t 		_M_extra;
      
      _Alloc			_M_allocator;

    public:
      debug_allocator() : _M_extra(8) { }

      pointer
      allocate(size_type __n, std::allocator<void>::const_pointer = 0)
      {
        pointer __result = _M_allocator.allocate(__n + _M_extra);
        *__result = __n;
        return __result + _M_extra;
      }

      void
      deallocate(pointer __p, size_type __n)
      {
        pointer __real_p = __p - _M_extra;
        if (*__real_p != __n)
          abort();
        _M_allocator.deallocate(__real_p, __n + _M_extra);
      }
    };
} // namespace __gnu_cxx

#endif
