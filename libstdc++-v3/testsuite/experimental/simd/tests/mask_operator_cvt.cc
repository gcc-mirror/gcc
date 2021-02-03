// Copyright (C) 2020-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// expensive: * [1-9] * *
#include "bits/verify.h"
#include "bits/metahelpers.h"

using schar = signed char;
using uchar = unsigned char;
using ushort = unsigned short;
using uint = unsigned int;
using ulong = unsigned long;
using llong = long long;
using ullong = unsigned long long;
using ldouble = long double;
using wchar = wchar_t;
using char16 = char16_t;
using char32 = char32_t;

template <typename M0, typename M1>
  constexpr bool
  bit_and_is_illformed()
  {
    return is_substitution_failure<M0, M1, std::bit_and<>>;
  }

template <typename M0, typename M1>
  void
  test_binary_op_cvt()
  {
    COMPARE((bit_and_is_illformed<M0, M1>()), !(std::is_same_v<M0, M1>) );
  }

template <typename V>
  void
  test()
  {
    using M = typename V::mask_type;
    // binary ops without conversions work
    COMPARE(typeid(M() & M()), typeid(M));

    // nothing else works: no implicit conv. or ambiguous
    using std::experimental::fixed_size_simd_mask;
    using std::experimental::native_simd_mask;
    using std::experimental::simd_mask;
    test_binary_op_cvt<M, bool>();

    test_binary_op_cvt<M, simd_mask<ldouble>>();
    test_binary_op_cvt<M, simd_mask<double>>();
    test_binary_op_cvt<M, simd_mask<float>>();
    test_binary_op_cvt<M, simd_mask<ullong>>();
    test_binary_op_cvt<M, simd_mask<llong>>();
    test_binary_op_cvt<M, simd_mask<ulong>>();
    test_binary_op_cvt<M, simd_mask<long>>();
    test_binary_op_cvt<M, simd_mask<uint>>();
    test_binary_op_cvt<M, simd_mask<int>>();
    test_binary_op_cvt<M, simd_mask<ushort>>();
    test_binary_op_cvt<M, simd_mask<short>>();
    test_binary_op_cvt<M, simd_mask<uchar>>();
    test_binary_op_cvt<M, simd_mask<schar>>();
    test_binary_op_cvt<M, simd_mask<wchar>>();
    test_binary_op_cvt<M, simd_mask<char16>>();
    test_binary_op_cvt<M, simd_mask<char32>>();

    test_binary_op_cvt<M, native_simd_mask<ldouble>>();
    test_binary_op_cvt<M, native_simd_mask<double>>();
    test_binary_op_cvt<M, native_simd_mask<float>>();
    test_binary_op_cvt<M, native_simd_mask<ullong>>();
    test_binary_op_cvt<M, native_simd_mask<llong>>();
    test_binary_op_cvt<M, native_simd_mask<ulong>>();
    test_binary_op_cvt<M, native_simd_mask<long>>();
    test_binary_op_cvt<M, native_simd_mask<uint>>();
    test_binary_op_cvt<M, native_simd_mask<int>>();
    test_binary_op_cvt<M, native_simd_mask<ushort>>();
    test_binary_op_cvt<M, native_simd_mask<short>>();
    test_binary_op_cvt<M, native_simd_mask<uchar>>();
    test_binary_op_cvt<M, native_simd_mask<schar>>();
    test_binary_op_cvt<M, native_simd_mask<wchar>>();
    test_binary_op_cvt<M, native_simd_mask<char16>>();
    test_binary_op_cvt<M, native_simd_mask<char32>>();

    test_binary_op_cvt<M, fixed_size_simd_mask<ldouble, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<double, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<float, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<ullong, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<llong, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<ulong, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<long, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<uint, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<int, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<ushort, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<short, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<uchar, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<schar, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<wchar, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<char16, 2>>();
    test_binary_op_cvt<M, fixed_size_simd_mask<char32, 2>>();
  }
