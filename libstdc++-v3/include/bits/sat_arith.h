// Saturation arithmetic -*- C++ -*-

// Copyright The GNU Toolchain Authors.
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

/** @file include/bits/sat_arith.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{numeric}
 */

#ifndef _GLIBCXX_SAT_ARITH_H
#define _GLIBCXX_SAT_ARITH_H 1

#pragma GCC system_header

#include <bits/version.h>

#ifdef __glibcxx_saturation_arithmetic // C++ >= 26

#include <concepts>
#include <ext/numeric_traits.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /// Add two integers, with saturation in case of overflow.
  template<typename _Tp> requires __is_standard_integer<_Tp>::value
    constexpr _Tp
    add_sat(_Tp __x, _Tp __y) noexcept
    {
      _Tp __z;
      if (!__builtin_add_overflow(__x, __y, &__z))
	return __z;
      if constexpr (is_unsigned_v<_Tp>)
	return __gnu_cxx::__int_traits<_Tp>::__max;
      else if (__x < 0)
	return __gnu_cxx::__int_traits<_Tp>::__min;
      else
	return __gnu_cxx::__int_traits<_Tp>::__max;
    }

  /// Subtract one integer from another, with saturation in case of overflow.
  template<typename _Tp> requires __is_standard_integer<_Tp>::value
    constexpr _Tp
    sub_sat(_Tp __x, _Tp __y) noexcept
    {
      _Tp __z;
      if (!__builtin_sub_overflow(__x, __y, &__z))
	return __z;
      if constexpr (is_unsigned_v<_Tp>)
	return __gnu_cxx::__int_traits<_Tp>::__min;
      else if (__x < 0)
	return __gnu_cxx::__int_traits<_Tp>::__min;
      else
	return __gnu_cxx::__int_traits<_Tp>::__max;
    }

  /// Multiply two integers, with saturation in case of overflow.
  template<typename _Tp> requires __is_standard_integer<_Tp>::value
    constexpr _Tp
    mul_sat(_Tp __x, _Tp __y) noexcept
    {
      _Tp __z;
      if (!__builtin_mul_overflow(__x, __y, &__z))
	return __z;
      if constexpr (is_unsigned_v<_Tp>)
	return __gnu_cxx::__int_traits<_Tp>::__max;
      else if (__x < 0 != __y < 0)
	return __gnu_cxx::__int_traits<_Tp>::__min;
      else
	return __gnu_cxx::__int_traits<_Tp>::__max;
    }

  /// Divide one integer by another, with saturation in case of overflow.
  template<typename _Tp> requires __is_standard_integer<_Tp>::value
    constexpr _Tp
    div_sat(_Tp __x, _Tp __y) noexcept
    {
      __glibcxx_assert(__y != 0);
      if constexpr (is_signed_v<_Tp>)
	if (__x == __gnu_cxx::__int_traits<_Tp>::__min && __y == _Tp(-1))
	  return __gnu_cxx::__int_traits<_Tp>::__max;
      return __x / __y;
    }

  /// Divide one integer by another, with saturation in case of overflow.
  template<typename _Res, typename _Tp>
    requires __is_standard_integer<_Res>::value
      && __is_standard_integer<_Tp>::value
    constexpr _Res
    saturate_cast(_Tp __x) noexcept
    {
      constexpr int __digits_R = __gnu_cxx::__int_traits<_Res>::__digits;
      constexpr int __digits_T = __gnu_cxx::__int_traits<_Tp>::__digits;
      constexpr _Res __max_Res = __gnu_cxx::__int_traits<_Res>::__max;

      if constexpr (is_signed_v<_Res> == is_signed_v<_Tp>)
	{
	  if constexpr (__digits_R < __digits_T)
	    {
	      constexpr _Res __min_Res = __gnu_cxx::__int_traits<_Res>::__min;

	      if (__x < static_cast<_Tp>(__min_Res))
		return __min_Res;
	      else if (__x > static_cast<_Tp>(__max_Res))
		return __max_Res;
	    }
	}
      else if constexpr (is_signed_v<_Tp>) // Res is unsigned
	{
	  if (__x < 0)
	    return 0;
	  else if (make_unsigned_t<_Tp>(__x) > __max_Res)
	    return __gnu_cxx::__int_traits<_Res>::__max;
	}
      else // Tp is unsigned, Res is signed
	{
	  if (__x > make_unsigned_t<_Res>(__max_Res))
	    return __max_Res;
	}
      return static_cast<_Res>(__x);
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif // __glibcxx_saturation_arithmetic
#endif /* _GLIBCXX_SAT_ARITH_H */
