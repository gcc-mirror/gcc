// C compatibility header <stdckdint.h> -*- C++ -*-

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

/** @file include/stdckdint.h
 *  This is a Standard C++ Library header.
 */

#ifndef _GLIBCXX_STDCKDINT_H
#define _GLIBCXX_STDCKDINT_H

#if __cplusplus > 202302L
#include <type_traits>
#include <concepts>

#define __STDC_VERSION_STDCKDINT_H__ 202311L

#ifndef _GLIBCXX_DOXYGEN
// We define these in our own namespace, but let Doxygen think otherwise.
namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
#endif
/// @cond undocumented
namespace __detail
{
  template<typename _Tp>
    concept __cv_unqual_signed_or_unsigned_integer_type
      = std::same_as<_Tp, std::remove_cv_t<_Tp>>
	  && std::__is_standard_integer<_Tp>::value;
}
/// @endcond

/** Checked integer arithmetic
 *
 * Performs arithmetic on `__a` and `__b` and stores the result in `*__result`,
 * with overflow detection.
 * The arithmetic is performed in infinite signed precision, without overflow,
 * then converted to the result type, `_Tp1`. If the converted result is not
 * equal to the infinite precision result, the stored result is wrapped to the
 * width of `_Tp1` and `true` is returned. Otherwise, the stored result is
 * correct and `false` is returned.
 *
 * @param __result A pointer to a signed or unsigned integer type.
 * @param __a      A signed or unsigned integer type.
 * @param __b      A signed or unsigned integer type.
 * @return True if overflow occurred, false otherwise.
 * @since C++26
 * @{
 */
template<typename _Tp1, typename _Tp2, typename _Tp3>
  inline bool
  ckd_add(_Tp1* __result, _Tp2 __a, _Tp3 __b)
  {
    using __gnu_cxx::__detail::__cv_unqual_signed_or_unsigned_integer_type;
    static_assert(__cv_unqual_signed_or_unsigned_integer_type<_Tp1>);
    static_assert(__cv_unqual_signed_or_unsigned_integer_type<_Tp2>);
    static_assert(__cv_unqual_signed_or_unsigned_integer_type<_Tp3>);
    return __builtin_add_overflow(__a, __b, __result);
  }

template<typename _Tp1, typename _Tp2, typename _Tp3>
  inline bool
  ckd_sub(_Tp1* __result, _Tp2 __a, _Tp3 __b)
  {
    using __gnu_cxx::__detail::__cv_unqual_signed_or_unsigned_integer_type;
    static_assert(__cv_unqual_signed_or_unsigned_integer_type<_Tp1>);
    static_assert(__cv_unqual_signed_or_unsigned_integer_type<_Tp2>);
    static_assert(__cv_unqual_signed_or_unsigned_integer_type<_Tp3>);
    return __builtin_sub_overflow(__a, __b, __result);
  }

template<typename _Tp1, typename _Tp2, typename _Tp3>
  inline bool
  ckd_mul(_Tp1* __result, _Tp2 __a, _Tp3 __b)
  {
    using __gnu_cxx::__detail::__cv_unqual_signed_or_unsigned_integer_type;
    static_assert(__cv_unqual_signed_or_unsigned_integer_type<_Tp1>);
    static_assert(__cv_unqual_signed_or_unsigned_integer_type<_Tp2>);
    static_assert(__cv_unqual_signed_or_unsigned_integer_type<_Tp3>);
    return __builtin_mul_overflow(__a, __b, __result);
  }
/// @}
#ifndef _GLIBCXX_DOXYGEN
}

using __gnu_cxx::ckd_add;
using __gnu_cxx::ckd_sub;
using __gnu_cxx::ckd_mul;
#endif

#endif // C++26

#endif // _GLIBCXX_STDCKDINT_H
