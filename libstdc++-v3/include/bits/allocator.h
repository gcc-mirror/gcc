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

/** @file allocator.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

/**
 *  @defgroup Allocators Memory Allocators
 *  @if maint
 *  allocator.h implements some node allocators.  These are NOT the same as
 *  allocators in the C++ standard, nor in the original H-P STL.  They do not
 *  encapsulate different pointer types; we assume that there is only one
 *  pointer type.  The C++ standard allocators are intended to allocate
 *  individual objects, not pools or arenas.
 *
 *  In this file allocators are of two different styles:  "standard" and
 *  "SGI" (quotes included).  "Standard" allocators conform to 20.4.  "SGI"
 *  allocators differ in AT LEAST the following ways (add to this list as you
 *  discover them):
 *
 *   - "Standard" allocate() takes two parameters (n_count,hint=0) but "SGI"
 *     allocate() takes one paramter (n_size).
 *   - Likewise, "standard" deallocate()'s argument is a count, but in "SGI"
 *     is a byte size.
 *   - max_size(), construct(), and destroy() are missing in "SGI" allocators.
 *   - reallocate(p,oldsz,newsz) is added in "SGI", and behaves as
 *     if p=realloc(p,newsz).
 *
 *  "SGI" allocators may be wrapped in __allocator to convert the interface
 *  into a "standard" one.
 *  @endif
 *
 *  The canonical description of these classes is in docs/html/ext/howto.html
 *  or online at http://gcc.gnu.org/onlinedocs/libstdc++/ext/howto.html#3
*/

#ifndef _ALLOCATOR_H
#define _ALLOCATOR_H 1

#include <bits/functexcept.h>   // For __throw_bad_alloc
#include <bits/allocator_traits.h>

// Pick a default underlying allocator.
#include <ext/pool_allocator.h>

namespace std
{
  typedef __gnu_cxx::__pool_alloc<true, 0>    __alloc;

  /// The version for the default allocator.
  template<typename _Tp, typename _Tp1>
    struct _Alloc_traits<_Tp, allocator<_Tp1> >
    {
      static const bool _S_instanceless = true;
      typedef __simple_alloc<_Tp, __alloc> _Alloc_type;
      typedef allocator<_Tp> allocator_type;
    };
  //@}
}

namespace std
{
  /**
   *  @brief  The "standard" allocator, as per [20.4].
   *
   *  The private _Alloc is "SGI" style.  (See comments at the top
   *  of allocator.h.)
   *
   *  The underlying allocator behaves as follows.
   *    - __pool_alloc is used via two typedefs
   *    - "__alloc" typedef is threadsafe via the locks
   *    - __new_alloc is used for memory requests
   *
   *  (See @link Allocators allocators info @endlink for more.)
   */
  template<typename _Tp>
    class allocator
    {
      // The underlying allocator.
      typedef __alloc _Alloc;     
     
    public:
      typedef size_t     size_type;
      typedef ptrdiff_t  difference_type;
      typedef _Tp*       pointer;
      typedef const _Tp* const_pointer;
      typedef _Tp&       reference;
      typedef const _Tp& const_reference;
      typedef _Tp        value_type;

      template<typename _Tp1>
        struct rebind
        { typedef allocator<_Tp1> other; };

      allocator() throw() { }

      allocator(const allocator&) throw() { }

      template<typename _Tp1>
        allocator(const allocator<_Tp1>&) throw() { }

      ~allocator() throw() { }

      pointer
      address(reference __x) const { return &__x; }

      const_pointer
      address(const_reference __x) const { return &__x; }

      // NB: __n is permitted to be 0.  The C++ standard says nothing
      // about what the return value is when __n == 0.
      _Tp*
      allocate(size_type __n, const void* = 0)
      {
	_Tp* __ret = 0;
	if (__n)
	  {
	    if (__n <= this->max_size())
	      __ret = static_cast<_Tp*>(_Alloc::allocate(__n * sizeof(_Tp)));
	    else
	      __throw_bad_alloc();
	  }
	return __ret;
      }

      // __p is not permitted to be a null pointer.
      void
      deallocate(pointer __p, size_type __n)
      { _Alloc::deallocate(__p, __n * sizeof(_Tp)); }

      size_type
      max_size() const throw() { return size_t(-1) / sizeof(_Tp); }

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 402. wrong new expression in [some_]allocator::construct
      void 
      construct(pointer __p, const _Tp& __val) { ::new(__p) _Tp(__val); }

      void 
      destroy(pointer __p) { __p->~_Tp(); }
    };

  template<>
    class allocator<void>
    {
    public:
      typedef size_t      size_type;
      typedef ptrdiff_t   difference_type;
      typedef void*       pointer;
      typedef const void* const_pointer;
      typedef void        value_type;

      template<typename _Tp1>
        struct rebind
        { typedef allocator<_Tp1> other; };
    };


  template<typename _T1, typename _T2>
    inline bool
    operator==(const allocator<_T1>&, const allocator<_T2>&)
    { return true; }

  template<typename _T1, typename _T2>
    inline bool
    operator!=(const allocator<_T1>&, const allocator<_T2>&)
    { return false; }

  // Inhibit implicit instantiations for required instantiations,
  // which are defined via explicit instantiations elsewhere.
  // NB: This syntax is a GNU extension.
#if _GLIBCXX_EXTERN_TEMPLATE
  extern template class allocator<char>;
  extern template class allocator<wchar_t>;
#endif
} // namespace std

#endif
