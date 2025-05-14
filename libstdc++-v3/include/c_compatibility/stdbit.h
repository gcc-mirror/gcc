// C compatibility header <stdbit.h> -*- C++ -*-

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

/** @file include/stdbit.h
 *  This is a Standard C++ Library header.
 */

#ifndef _GLIBCXX_STDBIT_H
#define _GLIBCXX_STDBIT_H

#if __cplusplus > 202302L
#include <bit>

#define __STDC_VERSION_STDBIT_H__ 202311L

#define __STDC_ENDIAN_BIG__     __ORDER_BIG_ENDIAN__
#define __STDC_ENDIAN_LITTLE__  __ORDER_LITTLE_ENDIAN__
#define __STDC_ENDIAN_NATIVE__  __BYTE_ORDER__

#ifndef _GLIBCXX_DOXYGEN
// We define these in our own namespace, but let Doxygen think otherwise.
namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
#endif

/** Count the number of leading zero bits
 *
 * @param  __value An unsigned integer.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline unsigned int
stdc_leading_zeros(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  return std::countl_zero(__value);
}

inline unsigned int
stdc_leading_zeros_uc(unsigned char __value)
{ return stdc_leading_zeros(__value); }

inline unsigned int
stdc_leading_zeros_us(unsigned short __value)
{ return stdc_leading_zeros(__value); }

inline unsigned int
stdc_leading_zeros_ui(unsigned int __value)
{ return stdc_leading_zeros(__value); }

inline unsigned int
stdc_leading_zeros_ul(unsigned long int __value)
{ return stdc_leading_zeros(__value); }

inline unsigned int
stdc_leading_zeros_ull(unsigned long long int __value)
{ return stdc_leading_zeros(__value); }
/// @}

/** Count the number of leading one bits
 *
 * @param  __value An unsigned integer.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline unsigned int
stdc_leading_ones(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  return std::countl_one(__value);
}

inline unsigned int
stdc_leading_ones_uc(unsigned char __value)
{ return stdc_leading_ones(__value); }

inline unsigned int
stdc_leading_ones_us(unsigned short __value)
{ return stdc_leading_ones(__value); }

inline unsigned int
stdc_leading_ones_ui(unsigned int __value)
{ return stdc_leading_ones(__value); }

inline unsigned int
stdc_leading_ones_ul(unsigned long int __value)
{ return stdc_leading_ones(__value); }

inline unsigned int
stdc_leading_ones_ull(unsigned long long int __value)
{ return stdc_leading_ones(__value); }
/// @}

/** Count the number of trailing zero bits
 *
 * @param  __value An unsigned integer.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline unsigned int
stdc_trailing_zeros(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  return std::countr_zero(__value);
}

inline unsigned int
stdc_trailing_zeros_uc(unsigned char __value)
{ return stdc_trailing_zeros(__value); }

inline unsigned int
stdc_trailing_zeros_us(unsigned short __value)
{ return stdc_trailing_zeros(__value); }

inline unsigned int
stdc_trailing_zeros_ui(unsigned int __value)
{ return stdc_trailing_zeros(__value); }

inline unsigned int
stdc_trailing_zeros_ul(unsigned long int __value)
{ return stdc_trailing_zeros(__value); }

inline unsigned int
stdc_trailing_zeros_ull(unsigned long long int __value)
{ return stdc_trailing_zeros(__value); }
/// @}

/** Count the number of trailing one bits
 *
 * @param  __value An unsigned integer.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline unsigned int
stdc_trailing_ones(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  return std::countr_one(__value);
}

inline unsigned int
stdc_trailing_ones_uc(unsigned char __value)
{ return stdc_trailing_ones(__value); }

inline unsigned int
stdc_trailing_ones_us(unsigned short __value)
{ return stdc_trailing_ones(__value); }

inline unsigned int
stdc_trailing_ones_ui(unsigned int __value)
{ return stdc_trailing_ones(__value); }

inline unsigned int
stdc_trailing_ones_ul(unsigned long int __value)
{ return stdc_trailing_ones(__value); }

inline unsigned int
stdc_trailing_ones_ull(unsigned long long int __value)
{ return stdc_trailing_ones(__value); }
/// @}

/** Find the leftmost (i.e. most significant) zero bit
 *
 * @param  __value An unsigned integer.
 * @return The one-based index of the first zero bit counting from the left,
 *         or zero if there are no zero bits.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline unsigned int
stdc_first_leading_zero(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  return __value == _Tp(-1) ? 0 : 1 + std::countl_one(__value);
}

inline unsigned int
stdc_first_leading_zero_uc(unsigned char __value)
{ return stdc_first_leading_zero(__value); }

inline unsigned int
stdc_first_leading_zero_us(unsigned short __value)
{ return stdc_first_leading_zero(__value); }

inline unsigned int
stdc_first_leading_zero_ui(unsigned int __value)
{ return stdc_first_leading_zero(__value); }

inline unsigned int
stdc_first_leading_zero_ul(unsigned long int __value)
{ return stdc_first_leading_zero(__value); }

inline unsigned int
stdc_first_leading_zero_ull(unsigned long long int __value)
{ return stdc_first_leading_zero(__value); }
/// @}

/** Find the leftmost (i.e. most significant) one bit
 *
 * @param  __value An unsigned integer.
 * @return The one-based index of the first one bit counting from the left,
 *         or zero if there are no one bits.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline unsigned int
stdc_first_leading_one(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  return __value == 0 ? 0 : 1 + std::countl_zero(__value);
}

inline unsigned int
stdc_first_leading_one_uc(unsigned char __value)
{ return stdc_first_leading_one(__value); }

inline unsigned int
stdc_first_leading_one_us(unsigned short __value)
{ return stdc_first_leading_one(__value); }

inline unsigned int
stdc_first_leading_one_ui(unsigned int __value)
{ return stdc_first_leading_one(__value); }

inline unsigned int
stdc_first_leading_one_ul(unsigned long int __value)
{ return stdc_first_leading_one(__value); }

inline unsigned int
stdc_first_leading_one_ull(unsigned long long int __value)
{ return stdc_first_leading_one(__value); }
/// @}

/** Find the rightmost (i.e. least significant) zero bit
 *
 * @param  __value An unsigned integer.
 * @return The one-based index of the first zero bit counting from the right,
 *         or zero if there are no zero bits.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline unsigned int
stdc_first_trailing_zero(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  return __value == _Tp(-1) ? 0 : 1 + std::countr_one(__value);
}

inline unsigned int
stdc_first_trailing_zero_uc(unsigned char __value)
{ return stdc_first_trailing_zero(__value); }

inline unsigned int
stdc_first_trailing_zero_us(unsigned short __value)
{ return stdc_first_trailing_zero(__value); }

inline unsigned int
stdc_first_trailing_zero_ui(unsigned int __value)
{ return stdc_first_trailing_zero(__value); }

inline unsigned int
stdc_first_trailing_zero_ul(unsigned long int __value)
{ return stdc_first_trailing_zero(__value); }

inline unsigned int
stdc_first_trailing_zero_ull(unsigned long long int __value)
{ return stdc_first_trailing_zero(__value); }
/// @}

/** Find the rightmost (i.e. least significant) one bit
 *
 * @param  __value An unsigned integer.
 * @return The one-based index of the first one bit counting from the right,
 *         or zero if there are no one bits.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline unsigned int
stdc_first_trailing_one(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  return __value == 0 ? 0 : 1 + std::countr_zero(__value);
}

inline unsigned int
stdc_first_trailing_one_uc(unsigned char __value)
{ return stdc_first_trailing_one(__value); }

inline unsigned int
stdc_first_trailing_one_us(unsigned short __value)
{ return stdc_first_trailing_one(__value); }

inline unsigned int
stdc_first_trailing_one_ui(unsigned int __value)
{ return stdc_first_trailing_one(__value); }

inline unsigned int
stdc_first_trailing_one_ul(unsigned long int __value)
{ return stdc_first_trailing_one(__value); }

inline unsigned int
stdc_first_trailing_one_ull(unsigned long long int __value)
{ return stdc_first_trailing_one(__value); }
/// @}

/** Count zeros
 *
 * @param  __value An unsigned integer.
 * @return The total number of zero bits in `__value`.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline unsigned int
stdc_count_zeros(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  return std::popcount(_Tp(~__value));
}

inline unsigned int
stdc_count_zeros_uc(unsigned char __value)
{ return stdc_count_zeros(__value); }

inline unsigned int
stdc_count_zeros_us(unsigned short __value)
{ return stdc_count_zeros(__value); }

inline unsigned int
stdc_count_zeros_ui(unsigned int __value)
{ return stdc_count_zeros(__value); }

inline unsigned int
stdc_count_zeros_ul(unsigned long int __value)
{ return stdc_count_zeros(__value); }

inline unsigned int
stdc_count_zeros_ull(unsigned long long int __value)
{ return stdc_count_zeros(__value); }
/// @}

/** Count ones
 *
 * @param  __value An unsigned integer.
 * @return The total number of one bits in `__value`.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline unsigned int
stdc_count_ones(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  return std::popcount(__value);
}

inline unsigned int
stdc_count_ones_uc(unsigned char __value)
{ return stdc_count_ones(__value); }

inline unsigned int
stdc_count_ones_us(unsigned short __value)
{ return stdc_count_ones(__value); }

inline unsigned int
stdc_count_ones_ui(unsigned int __value)
{ return stdc_count_ones(__value); }

inline unsigned int
stdc_count_ones_ul(unsigned long int __value)
{ return stdc_count_ones(__value); }

inline unsigned int
stdc_count_ones_ull(unsigned long long int __value)
{ return stdc_count_ones(__value); }
/// @}

/** Power of two check
 *
 * @param  __value An unsigned integer.
 * @return True if the value has a single bit set, false otherwise.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline bool
stdc_has_single_bit(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  return std::has_single_bit(__value);
}

inline bool
stdc_has_single_bit_uc(unsigned char __value)
{ return stdc_has_single_bit(__value); }

inline bool
stdc_has_single_bit_us(unsigned short __value)
{ return stdc_has_single_bit(__value); }

inline bool
stdc_has_single_bit_ui(unsigned int __value)
{ return stdc_has_single_bit(__value); }

inline bool
stdc_has_single_bit_ul(unsigned long int __value)
{ return stdc_has_single_bit(__value); }

inline bool
stdc_has_single_bit_ull(unsigned long long int __value)
{ return stdc_has_single_bit(__value); }
/// @}

/** Bit width
 *
 * @param  __value An unsigned integer.
 * @return The minimum number of bits needed to represent `__value`.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline unsigned int
stdc_bit_width(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  return std::bit_width(__value);
}

inline unsigned int
stdc_bit_width_uc(unsigned char __value)
{ return stdc_bit_width(__value); }

inline unsigned int
stdc_bit_width_us(unsigned short __value)
{ return stdc_bit_width(__value); }

inline unsigned int
stdc_bit_width_ui(unsigned int __value)
{ return stdc_bit_width(__value); }

inline unsigned int
stdc_bit_width_ul(unsigned long int __value)
{ return stdc_bit_width(__value); }

inline unsigned int
stdc_bit_width_ull(unsigned long long int __value)
{ return stdc_bit_width(__value); }
/// @}

/** Bit floor
 *
 * @param  __value An unsigned integer.
 * @return The largest power of two that is not greater than `__value`.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline _Tp
stdc_bit_floor(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  return std::bit_floor(__value);
}

inline unsigned char
stdc_bit_floor_uc(unsigned char __value)
{ return stdc_bit_floor(__value); }

inline unsigned short
stdc_bit_floor_us(unsigned short __value)
{ return stdc_bit_floor(__value); }

inline unsigned int
stdc_bit_floor_ui(unsigned int __value)
{ return stdc_bit_floor(__value); }

inline unsigned long int
stdc_bit_floor_ul(unsigned long int __value)
{ return stdc_bit_floor(__value); }

inline unsigned long long int
stdc_bit_floor_ull(unsigned long long int __value)
{ return stdc_bit_floor(__value); }
/// @}

/** Bit ceiling
 *
 * Unlike `std::bit_ceil`, this is defined to return zero for values which
 * are not representable in the return type.
 *
 * @param  __value An unsigned integer.
 * @return The smallest power of two that is not less than `__value`.
 * @since C++26
 * @{
 */
template<typename _Tp>
inline _Tp
stdc_bit_ceil(_Tp __value)
{
  static_assert(std::__unsigned_integer<_Tp>);
  constexpr _Tp __msb = _Tp(1) << (__gnu_cxx::__int_traits<_Tp>::__digits - 1);
  return (__value & __msb) ? 0 : std::bit_ceil(__value);
}

inline unsigned char
stdc_bit_ceil_uc(unsigned char __value)
{ return stdc_bit_ceil(__value); }

inline unsigned short
stdc_bit_ceil_us(unsigned short __value)
{ return stdc_bit_ceil(__value); }

inline unsigned int
stdc_bit_ceil_ui(unsigned int __value)
{ return stdc_bit_ceil(__value); }

inline unsigned long int
stdc_bit_ceil_ul(unsigned long int __value)
{ return stdc_bit_ceil(__value); }

inline unsigned long long int
stdc_bit_ceil_ull(unsigned long long int __value)
{ return stdc_bit_ceil(__value); }
/// @}

#ifndef _GLIBCXX_DOXYGEN
} // namespace __gnu_cxx
#define _GLIBCXX_STDBIT_FUNC(F) \
  using __gnu_cxx::F ## _uc; \
  using __gnu_cxx::F ## _us; \
  using __gnu_cxx::F ## _ui; \
  using __gnu_cxx::F ## _ul; \
  using __gnu_cxx::F ## _ull; \
  using __gnu_cxx::F
_GLIBCXX_STDBIT_FUNC(stdc_leading_zeros);
_GLIBCXX_STDBIT_FUNC(stdc_leading_ones);
_GLIBCXX_STDBIT_FUNC(stdc_trailing_zeros);
_GLIBCXX_STDBIT_FUNC(stdc_trailing_ones);
_GLIBCXX_STDBIT_FUNC(stdc_first_leading_zero);
_GLIBCXX_STDBIT_FUNC(stdc_first_leading_one);
_GLIBCXX_STDBIT_FUNC(stdc_first_trailing_zero);
_GLIBCXX_STDBIT_FUNC(stdc_first_trailing_one);
_GLIBCXX_STDBIT_FUNC(stdc_count_zeros);
_GLIBCXX_STDBIT_FUNC(stdc_count_ones);
_GLIBCXX_STDBIT_FUNC(stdc_has_single_bit);
_GLIBCXX_STDBIT_FUNC(stdc_bit_width);
_GLIBCXX_STDBIT_FUNC(stdc_bit_floor);
_GLIBCXX_STDBIT_FUNC(stdc_bit_ceil);
#undef _GLIBCXX_STDBIT_FUNC
#endif // !DOXYGEN
#endif // C++26

#endif // _GLIBCXX_STDBIT_H
