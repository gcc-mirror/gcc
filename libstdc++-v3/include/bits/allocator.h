// Allocators -*- C++ -*-

// Copyright (C) 2001-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

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

/** @file bits/allocator.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{memory}
 */

#ifndef _ALLOCATOR_H
#define _ALLOCATOR_H 1

#include <bits/c++allocator.h> // Define the base class to std::allocator.
#include <bits/memoryfwd.h>
#if __cplusplus >= 201103L
#include <type_traits>
#endif

#define __cpp_lib_incomplete_container_elements 201505L

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @addtogroup allocators
   *  @{
   */

  // Since C++20 the primary template should be used for allocator<void>,
  // but then it would have a non-trivial default ctor and dtor for C++20,
  // but trivial for C++98-17, which would be an ABI incompatibility between
  // different standard dialects. So C++20 still uses the allocator<void>
  // explicit specialization, with the historical ABI properties, but with
  // the same members that are present in the primary template.

  /** std::allocator<void> specialization.
   *
   * @headerfile memory
   */
  template<>
    class allocator<void>
    {
    public:
      typedef void        value_type;
      typedef size_t      size_type;
      typedef ptrdiff_t   difference_type;

#if __cplusplus <= 201703L
      // These were removed for C++20, allocator_traits does the right thing.
      typedef void*       pointer;
      typedef const void* const_pointer;

      template<typename _Tp1>
	struct rebind
	{ typedef allocator<_Tp1> other; };
#endif

#if __cplusplus >= 201103L
      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 2103. std::allocator propagate_on_container_move_assignment
      using propagate_on_container_move_assignment = true_type;

      using is_always_equal
	_GLIBCXX20_DEPRECATED_SUGGEST("std::allocator_traits::is_always_equal")
	= true_type;

#if __cplusplus >= 202002L
      // As noted above, these members are present for C++20 to provide the
      // same API as the primary template, but still trivial as in pre-C++20.
      allocator() = default;
      ~allocator() = default;

      template<typename _Up>
	__attribute__((__always_inline__))
	constexpr
	allocator(const allocator<_Up>&) noexcept { }

      // No allocate member because it's ill-formed by LWG 3307.
      // No deallocate member because it would be undefined to call it
      // with any pointer which wasn't obtained from allocate.
#endif // C++20
#endif // C++11
    };

  /**
   * @brief  The @a standard allocator, as per C++03 [20.4.1].
   *
   *  See https://gcc.gnu.org/onlinedocs/libstdc++/manual/memory.html#std.util.memory.allocator
   *  for further details.
   *
   *  @tparam  _Tp  Type of allocated object.
   *
   *  @headerfile memory
   */
  template<typename _Tp>
    class allocator : public __allocator_base<_Tp>
    {
    public:
      typedef _Tp        value_type;
      typedef size_t     size_type;
      typedef ptrdiff_t  difference_type;

#if __cplusplus <= 201703L
      // These were removed for C++20.
      typedef _Tp*       pointer;
      typedef const _Tp* const_pointer;
      typedef _Tp&       reference;
      typedef const _Tp& const_reference;

      template<typename _Tp1>
	struct rebind
	{ typedef allocator<_Tp1> other; };
#endif

#if __cplusplus >= 201103L
      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 2103. std::allocator propagate_on_container_move_assignment
      using propagate_on_container_move_assignment = true_type;

      using is_always_equal
	_GLIBCXX20_DEPRECATED_SUGGEST("std::allocator_traits::is_always_equal")
	= true_type;
#endif

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 3035. std::allocator's constructors should be constexpr
      __attribute__((__always_inline__))
      _GLIBCXX20_CONSTEXPR
      allocator() _GLIBCXX_NOTHROW { }

      __attribute__((__always_inline__))
      _GLIBCXX20_CONSTEXPR
      allocator(const allocator& __a) _GLIBCXX_NOTHROW
      : __allocator_base<_Tp>(__a) { }

#if __cplusplus >= 201103L
      // Avoid implicit deprecation.
      allocator& operator=(const allocator&) = default;
#endif

      template<typename _Tp1>
	__attribute__((__always_inline__))
	_GLIBCXX20_CONSTEXPR
	allocator(const allocator<_Tp1>&) _GLIBCXX_NOTHROW { }

      __attribute__((__always_inline__))
#if __cpp_constexpr_dynamic_alloc
      constexpr
#endif
      ~allocator() _GLIBCXX_NOTHROW { }

#if __cplusplus > 201703L
      [[nodiscard,__gnu__::__always_inline__]]
      constexpr _Tp*
      allocate(size_t __n)
      {
	if (std::__is_constant_evaluated())
	  {
	    if (__builtin_mul_overflow(__n, sizeof(_Tp), &__n))
	      std::__throw_bad_array_new_length();
	    return static_cast<_Tp*>(::operator new(__n));
	  }

	return __allocator_base<_Tp>::allocate(__n, 0);
      }

      [[__gnu__::__always_inline__]]
      constexpr void
      deallocate(_Tp* __p, size_t __n)
      {
	if (std::__is_constant_evaluated())
	  {
	    ::operator delete(__p);
	    return;
	  }
	__allocator_base<_Tp>::deallocate(__p, __n);
      }
#endif // C++20

      friend __attribute__((__always_inline__)) _GLIBCXX20_CONSTEXPR
      bool
      operator==(const allocator&, const allocator&) _GLIBCXX_NOTHROW
      { return true; }

#if __cpp_impl_three_way_comparison < 201907L
      friend __attribute__((__always_inline__)) _GLIBCXX20_CONSTEXPR
      bool
      operator!=(const allocator&, const allocator&) _GLIBCXX_NOTHROW
      { return false; }
#endif

      // Inherit everything else.
    };

  /** Equality comparison for std::allocator objects
   *
   * @return true, for all std::allocator objects.
   * @relates std::allocator
   */
  template<typename _T1, typename _T2>
    __attribute__((__always_inline__))
    inline _GLIBCXX20_CONSTEXPR bool
    operator==(const allocator<_T1>&, const allocator<_T2>&)
    _GLIBCXX_NOTHROW
    { return true; }

#if __cpp_impl_three_way_comparison < 201907L
  template<typename _T1, typename _T2>
    __attribute__((__always_inline__))
    inline _GLIBCXX20_CONSTEXPR bool
    operator!=(const allocator<_T1>&, const allocator<_T2>&)
    _GLIBCXX_NOTHROW
    { return false; }
#endif

  /// @cond undocumented

  // Invalid allocator<cv T> partial specializations.
  // allocator_traits::rebind_alloc can be used to form a valid allocator type.
  template<typename _Tp>
    class allocator<const _Tp>
    {
    public:
      typedef _Tp value_type;
      template<typename _Up> allocator(const allocator<_Up>&) { }
    };

  template<typename _Tp>
    class allocator<volatile _Tp>
    {
    public:
      typedef _Tp value_type;
      template<typename _Up> allocator(const allocator<_Up>&) { }
    };

  template<typename _Tp>
    class allocator<const volatile _Tp>
    {
    public:
      typedef _Tp value_type;
      template<typename _Up> allocator(const allocator<_Up>&) { }
    };
  /// @endcond

  /// @} group allocator

  // Inhibit implicit instantiations for required instantiations,
  // which are defined via explicit instantiations elsewhere.
#if _GLIBCXX_EXTERN_TEMPLATE
  extern template class allocator<char>;
  extern template class allocator<wchar_t>;
#endif

  // Undefine.
#undef __allocator_base

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif
