// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

#ifndef _EXT_TYPE_TRAITS
#define _EXT_TYPE_TRAITS 1

#pragma GCC system_header

#include <cstddef>
#include <utility>
#include <limits>
#include <iosfwd> // std::streamsize
#include <bits/cpp_type_traits.h>

_GLIBCXX_BEGIN_NAMESPACE(__gnu_cxx)

  // Define a nested type if some predicate holds.
  template<bool, typename>
    struct __enable_if 
    { };

  template<typename _Tp>
    struct __enable_if<true, _Tp>
    { typedef _Tp __type; };

  // XXX What about std::tr1::true_type?
  // Conditional expression for types. If true, first, if false, second.
  template<bool _Cond, typename _Iftrue, typename _Iffalse>
    struct __conditional_type
    { typedef _Iftrue __type; };

  template<typename _Iftrue, typename _Iffalse>
    struct __conditional_type<false, _Iftrue, _Iffalse>
    { typedef _Iffalse __type; };


  // Given a builtin type, return the corresponding unsigned type.
  template<typename _Value>
    struct __add_unsigned
    { typedef _Value __type; };

  template<>
    struct __add_unsigned<char>
    { typedef unsigned char __type; };

  template<>
    struct __add_unsigned<short>
    { typedef unsigned short __type; };

  template<>
    struct __add_unsigned<int>
    { typedef unsigned int __type; };

  template<>
    struct __add_unsigned<long>
    { typedef unsigned long __type; };

#ifdef _GLIBCXX_USE_LONG_LONG
  template<>
    struct __add_unsigned<long long>
    { typedef unsigned long long __type; };
#endif

  // Given an builtin type, return the corresponding signed type.
  template<typename _Value>
    struct __remove_unsigned
    { typedef _Value __type; };

  template<>
    struct __remove_unsigned<unsigned char>
    { typedef char __type; };

  template<>
    struct __remove_unsigned<unsigned short>
    { typedef short __type; };

  template<>
    struct __remove_unsigned<unsigned int>
    { typedef int __type; };

  template<>
    struct __remove_unsigned<unsigned long>
    { typedef long __type; };

#ifdef _GLIBCXX_USE_LONG_LONG
  template<>
    struct __remove_unsigned<unsigned long long>
    { typedef long long __type; };
#endif

  // Compile time constants for builtin types.
  // Sadly std::numeric_limits member functions cannot be used for this.
#define __glibcxx_signed(T) ((T)(-1) < 0)
#define __glibcxx_digits(T) (sizeof(T) * __CHAR_BIT__ - __glibcxx_signed(T))

#define __glibcxx_min(T) \
  (__glibcxx_signed(T) ? (T)1 << __glibcxx_digits(T) : (T)0)

#define __glibcxx_max(T) \
  (__glibcxx_signed(T) ? ((T)1 << __glibcxx_digits(T)) - 1 : ~(T)0)

  template<typename _Value>
    struct __numeric_traits_integer
    {
      // Only integers for initialization of member constant.
      static const _Value __min = __glibcxx_min(_Value);
      static const _Value __max = __glibcxx_max(_Value);
    };

  template<typename _Value>
    const _Value __numeric_traits_integer<_Value>::__min;

  template<typename _Value>
    const _Value __numeric_traits_integer<_Value>::__max;

  template<typename _Value>
    struct __numeric_traits_floating
    {
      // Only floating point types. See N1822. 
      static const std::streamsize __max_digits10 =
	2 + std::numeric_limits<_Value>::digits * 3010/10000;
    };

  template<typename _Value>
    const std::streamsize __numeric_traits_floating<_Value>::__max_digits10;

  template<typename _Value>
    struct __numeric_traits 
    : public __conditional_type<std::__is_integer<_Value>::__value,
				__numeric_traits_integer<_Value>,
				__numeric_traits_floating<_Value> >::__type
    { };

_GLIBCXX_END_NAMESPACE

#endif 
