// <extptr_allocator.h> -*- C++ -*-

// Copyright (C) 2008 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/**
 * @file ext/extptr_allocator.h
 * @author Bob Walters
 *
 * An example allocator which uses an alternative pointer type from
 * bits/pointer.h.  Supports test cases which confirm container support
 * for alternative pointers.
 */

#ifndef _EXTPTR_ALLOCATOR_H
#define _EXTPTR_ALLOCATOR_H 1

#include <memory>
#include <limits>
#include <ext/pointer.h>

using __gnu_cxx::_Pointer_adapter;
using __gnu_cxx::_Relative_pointer_impl;

_GLIBCXX_BEGIN_NAMESPACE(__gnu_cxx)

  // forward declaration
  template<typename _Tp>
    class _ExtPtr_allocator;

  // _ExtPtr_allocator<void> specialization.
  template<>
    class _ExtPtr_allocator<void>
    {
    public:
      typedef size_t      size_type;
      typedef ptrdiff_t   difference_type;
      typedef void        value_type;

      // Note the non-standard pointer types
      typedef _Pointer_adapter<_Relative_pointer_impl<void> >       pointer;
      typedef _Pointer_adapter<_Relative_pointer_impl<const void> >
                                                              const_pointer;

      template<typename _Up>
        struct rebind
        { typedef _ExtPtr_allocator<_Up> other; };
    };

  /**
   * @brief An example allocator which uses a non-standard pointer type.
   *
   * This allocator specifies that containers use a 'relative pointer' as it's
   * pointer type.  (See bits/pointer.h)  Memory allocation in this example
   * is still performed using std::allocator.
   */
  template<typename _Tp>
    class _ExtPtr_allocator
    {
    public:
      typedef size_t     size_type;
      typedef ptrdiff_t  difference_type;

      // Note the non-standard pointer types.
      typedef _Pointer_adapter<_Relative_pointer_impl<_Tp> >       pointer;
      typedef _Pointer_adapter<_Relative_pointer_impl<const _Tp> > const_pointer;

      typedef _Tp&       reference;
      typedef const _Tp& const_reference;
      typedef _Tp        value_type;

      template<typename _Up>
        struct rebind
        { typedef _ExtPtr_allocator<_Up> other; };

      _ExtPtr_allocator() throw() 
      : _M_real_alloc() { }

      _ExtPtr_allocator(const _ExtPtr_allocator &__rarg) throw()
      : _M_real_alloc(__rarg._M_real_alloc) { }

      template<class _Up>
        _ExtPtr_allocator(const _ExtPtr_allocator<_Up>& __rarg) throw()
        : _M_real_alloc(__rarg._M_getUnderlyingImp()) { }

      ~_ExtPtr_allocator() throw()
      { }

      pointer address(reference __x) const
      { return &__x; }

      const_pointer address(const_reference __x) const
      { return &__x; }

      pointer allocate(size_type __n, void* __hint = 0)
      { return _M_real_alloc.allocate(__n,__hint); }

      void deallocate(pointer __p, size_type __n)
      { _M_real_alloc.deallocate(__p.get(), __n); }

      size_type max_size() const throw()
      { return std::numeric_limits<size_type>::max() / sizeof(_Tp); }

      void construct(pointer __p, const _Tp& __val)
      { ::new(__p.get()) _Tp(__val); }

#ifdef __GXX_EXPERIMENTAL_CXX0X__
      template<typename... _Args>
        void
        construct(pointer __p, _Args&&... __args)
        { ::new(__p.get()) _Tp(std::forward<_Args>(__args)...); }
#endif

      void destroy(pointer __p)
      { __p->~_Tp(); }

      template<typename _Up>
        inline bool
        operator==(const _ExtPtr_allocator<_Up>& __rarg)
        { return _M_real_alloc == __rarg._M_getUnderlyingImp(); }

      inline bool
      operator==(const _ExtPtr_allocator& __rarg)
      { return _M_real_alloc == __rarg._M_real_alloc; }

      template<typename _Up>
        inline bool
        operator!=(const _ExtPtr_allocator<_Up>& __rarg)
        { return _M_real_alloc != __rarg._M_getUnderlyingImp(); }

      inline bool
      operator!=(const _ExtPtr_allocator& __rarg)
      { return _M_real_alloc != __rarg._M_real_alloc; }

      template<typename _Up>
        inline friend void
        swap(_ExtPtr_allocator<_Up>& __larg, _ExtPtr_allocator<_Up>& __rarg);

      // A method specific to this implementation.
      const std::allocator<_Tp>&
      _M_getUnderlyingImp() const
      { return _M_real_alloc; }

    private:
      // simlated state data.
      std::allocator<_Tp>  _M_real_alloc;
    };

  template<typename _Tp>
    inline void
    swap(_ExtPtr_allocator<_Tp>& __larg, _ExtPtr_allocator<_Tp>& __rarg)
    {
      std::allocator<_Tp> temp( __rarg._M_real_alloc );
      __rarg._M_real_alloc = __larg._M_real_alloc;
      __larg._M_real_alloc = temp;
    }

_GLIBCXX_END_NAMESPACE

#endif /* _EXTPTR_ALLOCATOR_H */
