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

#include <bits/allocator_traits.h>

namespace __gnu_cxx
{
  /**
   *  @if maint
   *  An adaptor for an underlying allocator (_Alloc) to check the size
   *  arguments for debugging.
   *
   *  "There is some evidence that this can confuse Purify." - SGI comment
   *
   *  This adaptor is "SGI" style.  The _Alloc parameter must also be "SGI".
   *  @endif
   *  (See @link Allocators allocators info @endlink for more.)
   */
  template<typename _Alloc>
    class __debug_alloc
    {
    private:
      // Size of space used to store size.  Note that this must be
      // large enough to preserve alignment.
      enum {_S_extra = 8};

    public:
      static void*
      allocate(size_t __n)
      {
        char* __result = (char*)_Alloc::allocate(__n + (int) _S_extra);
        *(size_t*)__result = __n;
        return __result + (int) _S_extra;
      }

      static void
      deallocate(void* __p, size_t __n)
      {
        char* __real_p = (char*)__p - (int) _S_extra;
        if (*(size_t*)__real_p != __n)
          abort();
        _Alloc::deallocate(__real_p, __n + (int) _S_extra);
      }
    };

  //@{
  /** Comparison operators for all of the predifined SGI-style allocators.
   *  This ensures that __allocator<malloc_alloc> (for example) will work
   *  correctly.  As required, all allocators compare equal.
   */
  template<typename _Alloc>
    inline bool
    operator==(const __debug_alloc<_Alloc>&, const __debug_alloc<_Alloc>&)
    { return true; }

  template<typename _Alloc>
    inline bool
    operator!=(const __debug_alloc<_Alloc>&, const __debug_alloc<_Alloc>&)
    { return false; }
  //@}
} // namespace __gnu_cxx

namespace std
{
  //@{
  /// Versions for the predefined "SGI" style allocators.
  template<typename _Tp, typename _Alloc>
    struct _Alloc_traits<_Tp, __gnu_cxx::__debug_alloc<_Alloc> >
    {
      static const bool _S_instanceless = true;
      typedef __gnu_cxx::__debug_alloc<_Alloc>		base_alloc_type;
      typedef __simple_alloc<_Tp, base_alloc_type>	_Alloc_type;
      typedef __allocator<_Tp, base_alloc_type>		allocator_type;
    };
  //@}

  //@{
  /// Versions for the __allocator adaptor used with the predefined
  /// "SGI" style allocators.
  template<typename _Tp, typename _Tp1, typename _Alloc>
    struct _Alloc_traits<_Tp, __allocator<_Tp1,
					  __gnu_cxx::__debug_alloc<_Alloc> > >
    {
      static const bool _S_instanceless = true;
      typedef __gnu_cxx::__debug_alloc<_Alloc>		base_alloc_type;
      typedef __simple_alloc<_Tp, base_alloc_type>	_Alloc_type;
      typedef __allocator<_Tp, base_alloc_type>		allocator_type;
    };
  //@}
} // namespace std

#endif
