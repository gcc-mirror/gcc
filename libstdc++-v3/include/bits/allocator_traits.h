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

#ifndef _ALLOCATOR_TRAITS_H
#define _ALLOCATOR_TRAITS_H 1

#include <cstddef>

namespace std
{
  /**
   *  @if maint
   *  This is used primarily (only?) in _Alloc_traits and other places to
   *  help provide the _Alloc_type typedef.  All it does is forward the
   *  requests after some minimal checking.
   *
   *  This is neither "standard"-conforming nor "SGI".  The _Alloc parameter
   *  must be "SGI" style.
   *  @endif
   *  (See @link Allocators allocators info @endlink for more.)
   */
  template<typename _Tp, typename _Alloc>
    class __simple_alloc
    {
    public:
      static _Tp*
      allocate(size_t __n)
      {
	_Tp* __ret = 0;
	if (__n)
	  __ret = static_cast<_Tp*>(_Alloc::allocate(__n * sizeof(_Tp)));
	return __ret;
      }
  
      static _Tp*
      allocate()
      { return (_Tp*) _Alloc::allocate(sizeof (_Tp)); }
  
      static void
      deallocate(_Tp* __p, size_t __n)
      { if (0 != __n) _Alloc::deallocate(__p, __n * sizeof (_Tp)); }
  
      static void
      deallocate(_Tp* __p)
      { _Alloc::deallocate(__p, sizeof (_Tp)); }
    };


  /**
   *  @if maint
   *  Allocator adaptor to turn an "SGI" style allocator (e.g.,
   *  __alloc, __malloc_alloc) into a "standard" conforming
   *  allocator.  Note that this adaptor does *not* assume that all
   *  objects of the underlying alloc class are identical, nor does it
   *  assume that all of the underlying alloc's member functions are
   *  static member functions.  Note, also, that __allocator<_Tp,
   *  __alloc> is essentially the same thing as allocator<_Tp>.
   *  @endif
   *  (See @link Allocators allocators info @endlink for more.)
   */
  template<typename _Tp, typename _Alloc>
    struct __allocator
    {
      _Alloc __underlying_alloc;
      
      typedef size_t    size_type;
      typedef ptrdiff_t difference_type;
      typedef _Tp*       pointer;
      typedef const _Tp* const_pointer;
      typedef _Tp&       reference;
      typedef const _Tp& const_reference;
      typedef _Tp        value_type;

      template<typename _Tp1>
        struct rebind
        { typedef __allocator<_Tp1, _Alloc> other; };

      __allocator() throw() { }

      __allocator(const __allocator& __a) throw()
      : __underlying_alloc(__a.__underlying_alloc) { }

      template<typename _Tp1>
        __allocator(const __allocator<_Tp1, _Alloc>& __a) throw()
        : __underlying_alloc(__a.__underlying_alloc) { }

      ~__allocator() throw() { }

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
	  __ret = static_cast<_Tp*>(_Alloc::allocate(__n * sizeof(_Tp)));
	return __ret;
      }

      // __p is not permitted to be a null pointer.
      void
      deallocate(pointer __p, size_type __n)
      { __underlying_alloc.deallocate(__p, __n * sizeof(_Tp)); }
      
      size_type
      max_size() const throw() { return size_t(-1) / sizeof(_Tp); }

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 402. wrong new expression in [some_]allocator::construct
      void
      construct(pointer __p, const _Tp& __val) { ::new(__p) _Tp(__val); }
      
      void
      destroy(pointer __p) { __p->~_Tp(); }
    };

  template<typename _Alloc>
    struct __allocator<void, _Alloc>
    {
      typedef size_t      size_type;
      typedef ptrdiff_t   difference_type;
      typedef void*       pointer;
      typedef const void* const_pointer;
      typedef void        value_type;

      template<typename _Tp1>
        struct rebind
        { typedef __allocator<_Tp1, _Alloc> other; };
    };

  template<typename _Tp, typename _Alloc>
    inline bool
    operator==(const __allocator<_Tp,_Alloc>& __a1, 
	       const __allocator<_Tp,_Alloc>& __a2)
    { return __a1.__underlying_alloc == __a2.__underlying_alloc; }

  template<typename _Tp, typename _Alloc>
    inline bool
    operator!=(const __allocator<_Tp, _Alloc>& __a1,
               const __allocator<_Tp, _Alloc>& __a2)
    { return __a1.__underlying_alloc != __a2.__underlying_alloc; }


  /**
   *  @if maint
   *  Another allocator adaptor:  _Alloc_traits.  This serves two purposes.
   *  First, make it possible to write containers that can use either "SGI"
   *  style allocators or "standard" allocators.  Second, provide a mechanism
   *  so that containers can query whether or not the allocator has distinct
   *  instances.  If not, the container can avoid wasting a word of memory to
   *  store an empty object.  For examples of use, see stl_vector.h, etc, or
   *  any of the other classes derived from this one.
   *
   *  This adaptor uses partial specialization.  The general case of
   *  _Alloc_traits<_Tp, _Alloc> assumes that _Alloc is a
   *  standard-conforming allocator, possibly with non-equal instances and
   *  non-static members.  (It still behaves correctly even if _Alloc has
   *  static member and if all instances are equal.  Refinements affect
   *  performance, not correctness.)
   *
   *  There are always two members:  allocator_type, which is a standard-
   *  conforming allocator type for allocating objects of type _Tp, and
   *  _S_instanceless, a static const member of type bool.  If
   *  _S_instanceless is true, this means that there is no difference
   *  between any two instances of type allocator_type.  Furthermore, if
   *  _S_instanceless is true, then _Alloc_traits has one additional
   *  member:  _Alloc_type.  This type encapsulates allocation and
   *  deallocation of objects of type _Tp through a static interface; it
   *  has two member functions, whose signatures are
   *
   *  -  static _Tp* allocate(size_t)
   *  -  static void deallocate(_Tp*, size_t)
   *
   *  The size_t parameters are "standard" style (see top of
   *  allocator.h) in that they take counts, not sizes.
   *
   *  @endif
   *  (See @link Allocators allocators info @endlink for more.)
   */
  // The fully general version.
  template<typename _Tp, typename _Allocator>
    struct _Alloc_traits
    {
      static const bool _S_instanceless = false;
      typedef typename _Allocator::template rebind<_Tp>::other allocator_type;
    };

  template<typename _Tp, typename _Allocator>
    const bool _Alloc_traits<_Tp, _Allocator>::_S_instanceless;
} // namespace std

#endif
