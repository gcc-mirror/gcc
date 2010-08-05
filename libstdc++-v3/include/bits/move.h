// Move, forward and identity for C++0x + swap -*- C++ -*-

// Copyright (C) 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
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

/** @file move.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _MOVE_H
#define _MOVE_H 1

#include <bits/c++config.h>
#include <bits/concept_check.h>

_GLIBCXX_BEGIN_NAMESPACE(std)

  // Used, in C++03 mode too, by allocators, etc.
  template<typename _Tp>
    inline _Tp*
    __addressof(_Tp& __r)
    {
      return reinterpret_cast<_Tp*>
	(&const_cast<char&>(reinterpret_cast<const volatile char&>(__r)));
    }

_GLIBCXX_END_NAMESPACE

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#include <type_traits> // Brings in std::declval too.

_GLIBCXX_BEGIN_NAMESPACE(std)
  
  /// forward
  template<typename _Tp, typename _Up>
    inline typename
    enable_if<((std::is_convertible<
		typename std::remove_reference<_Up>::type*,
		typename std::remove_reference<_Tp>::type*>::value)
	       && (!std::is_lvalue_reference<_Tp>::value
		   || std::is_lvalue_reference<_Up>::value)), _Tp&&>::type
    forward(_Up&& __u)
    { return static_cast<_Tp&&>(__u); }

  /**
   *  @brief Move a value.
   *  @ingroup mutating_algorithms
   *  @param  __t  A thing of arbitrary type.
   *  @return Same, moved.
  */
  template<typename _Tp>
    inline typename std::remove_reference<_Tp>::type&&
    move(_Tp&& __t)
    { return static_cast<typename std::remove_reference<_Tp>::type&&>(__t); }

  /// declval, from type_traits.

  /**
   *  @brief Returns the actual address of the object or function
   *         referenced by r, even in the presence of an overloaded
   *         operator&.
   *  @param  __r  Reference to an object or function.
   *  @return   The actual address.
  */
  template<typename _Tp>
    inline _Tp*
    addressof(_Tp& __r)
    { return std::__addressof(__r); }

_GLIBCXX_END_NAMESPACE

#define _GLIBCXX_MOVE(__val) std::move(__val)
#define _GLIBCXX_FORWARD(_Tp, __val) std::forward<_Tp>(__val)
#else
#define _GLIBCXX_MOVE(__val) (__val)
#define _GLIBCXX_FORWARD(_Tp, __val) (__val)
#endif

_GLIBCXX_BEGIN_NAMESPACE(std)

  /**
   *  @brief Swaps two values.
   *  @ingroup mutating_algorithms
   *  @param  __a  A thing of arbitrary type.
   *  @param  __b  Another thing of arbitrary type.
   *  @return   Nothing.
  */
  template<typename _Tp>
    inline void
    swap(_Tp& __a, _Tp& __b)
    {
      // concept requirements
      __glibcxx_function_requires(_SGIAssignableConcept<_Tp>)

      _Tp __tmp = _GLIBCXX_MOVE(__a);
      __a = _GLIBCXX_MOVE(__b);
      __b = _GLIBCXX_MOVE(__tmp);
    }

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // DR 809. std::swap should be overloaded for array types.
  template<typename _Tp, size_t _Nm>
    inline void
    swap(_Tp (&__a)[_Nm], _Tp (&__b)[_Nm])
    {
      for (size_t __n = 0; __n < _Nm; ++__n)
	swap(__a[__n], __b[__n]);
    }

_GLIBCXX_END_NAMESPACE

#endif /* _MOVE_H */
