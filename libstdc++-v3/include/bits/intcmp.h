// Integer comparison functions -*- C++ -*-

// Copyright (C) 2020-2026 Free Software Foundation, Inc.
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

/** @file bits/intcmp.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{utility}
 */

#ifndef _GLIBCXX_INTCMP_H
#define _GLIBCXX_INTCMP_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/version.h>

#ifdef __glibcxx_integer_comparison_functions // C++ >= 20

#include <type_traits>
#include <ext/numeric_traits.h> // __int_traits

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<typename _Tp, typename _Up>
    constexpr bool
    cmp_equal(_Tp __t, _Up __u) noexcept
    {
      static_assert(__is_signed_or_unsigned_integer<_Tp>::value);
      static_assert(__is_signed_or_unsigned_integer<_Up>::value);

      if constexpr (is_signed_v<_Tp> == is_signed_v<_Up>)
	return __t == __u;
      else if constexpr (is_signed_v<_Tp>)
	return __t >= 0 && make_unsigned_t<_Tp>(__t) == __u;
      else
	return __u >= 0 && __t == make_unsigned_t<_Up>(__u);
    }

  template<typename _Tp, typename _Up>
    constexpr bool
    cmp_not_equal(_Tp __t, _Up __u) noexcept
    { return !std::cmp_equal(__t, __u); }

  template<typename _Tp, typename _Up>
    constexpr bool
    cmp_less(_Tp __t, _Up __u) noexcept
    {
      static_assert(__is_signed_or_unsigned_integer<_Tp>::value);
      static_assert(__is_signed_or_unsigned_integer<_Up>::value);

      if constexpr (is_signed_v<_Tp> == is_signed_v<_Up>)
	return __t < __u;
      else if constexpr (is_signed_v<_Tp>)
	return __t < 0 || make_unsigned_t<_Tp>(__t) < __u;
      else
	return __u >= 0 && __t < make_unsigned_t<_Up>(__u);
    }

  template<typename _Tp, typename _Up>
    constexpr bool
    cmp_greater(_Tp __t, _Up __u) noexcept
    { return std::cmp_less(__u, __t); }

  template<typename _Tp, typename _Up>
    constexpr bool
    cmp_less_equal(_Tp __t, _Up __u) noexcept
    { return !std::cmp_less(__u, __t); }

  template<typename _Tp, typename _Up>
    constexpr bool
    cmp_greater_equal(_Tp __t, _Up __u) noexcept
    { return !std::cmp_less(__t, __u); }

  template<typename _Res, typename _Tp>
    constexpr bool
    in_range(_Tp __t) noexcept
    {
      static_assert(__is_signed_or_unsigned_integer<_Res>::value);
      static_assert(__is_signed_or_unsigned_integer<_Tp>::value);
      using __gnu_cxx::__int_traits;

      if constexpr (is_signed_v<_Tp> == is_signed_v<_Res>)
	return __int_traits<_Res>::__min <= __t
	  && __t <= __int_traits<_Res>::__max;
      else if constexpr (is_signed_v<_Tp>)
	return __t >= 0
	  && make_unsigned_t<_Tp>(__t) <= __int_traits<_Res>::__max;
      else
	return __t <= make_unsigned_t<_Res>(__int_traits<_Res>::__max);
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif // __glibcxx_integer_comparison_functions
#endif // _GLIBCXX_INTCMP_H
