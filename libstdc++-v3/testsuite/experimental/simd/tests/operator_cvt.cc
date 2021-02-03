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

// type with sizeof(char) but different signedness
using xchar = std::conditional_t<std::is_unsigned_v<char>, schar, uchar>;

using vschar = std::experimental::native_simd<schar>;
using vuchar = std::experimental::native_simd<uchar>;
using vshort = std::experimental::native_simd<short>;
using vushort = std::experimental::native_simd<ushort>;
using vint = std::experimental::native_simd<int>;
using vuint = std::experimental::native_simd<uint>;
using vlong = std::experimental::native_simd<long>;
using vulong = std::experimental::native_simd<ulong>;
using vllong = std::experimental::native_simd<llong>;
using vullong = std::experimental::native_simd<ullong>;
using vfloat = std::experimental::native_simd<float>;
using vdouble = std::experimental::native_simd<double>;
using vldouble = std::experimental::native_simd<long double>;
using vchar = std::experimental::native_simd<char>;
using vxchar = std::experimental::native_simd<xchar>;

template <typename T>
  using vi8 = std::experimental::fixed_size_simd<T, vschar::size()>;
template <typename T>
  using vi16 = std::experimental::fixed_size_simd<T, vshort::size()>;
template <typename T>
  using vf32 = std::experimental::fixed_size_simd<T, vfloat::size()>;
template <typename T>
  using vi32 = std::experimental::fixed_size_simd<T, vint::size()>;
template <typename T>
  using vf64 = std::experimental::fixed_size_simd<T, vdouble::size()>;
template <typename T>
  using vi64 = std::experimental::fixed_size_simd<T, vllong::size()>;
template <typename T>
  using vl = typename std::conditional<sizeof(long) == sizeof(llong), vi64<T>,
				       vi32<T>>::type;

template <class A, class B, class Expected = A>
  void
  binary_op_return_type()
  {
    using namespace vir::test;
    static_assert(std::is_same<A, Expected>::value, "");
    using AC = std::add_const_t<A>;
    using BC = std::add_const_t<B>;
    COMPARE(typeid(A() + B()), typeid(Expected));
    COMPARE(typeid(B() + A()), typeid(Expected));
    COMPARE(typeid(AC() + BC()), typeid(Expected));
    COMPARE(typeid(BC() + AC()), typeid(Expected));
  }

template <typename V>
  void
  test()
  {
    using T = typename V::value_type;
    namespace simd_abi = std::experimental::simd_abi;
    binary_op_return_type<V, V, V>();
    binary_op_return_type<V, T, V>();
    binary_op_return_type<V, int, V>();

    if constexpr (std::is_same_v<V, vfloat>)
      {
	binary_op_return_type<vfloat, schar>();
	binary_op_return_type<vfloat, uchar>();
	binary_op_return_type<vfloat, short>();
	binary_op_return_type<vfloat, ushort>();

	binary_op_return_type<vf32<float>, schar>();
	binary_op_return_type<vf32<float>, uchar>();
	binary_op_return_type<vf32<float>, short>();
	binary_op_return_type<vf32<float>, ushort>();
	binary_op_return_type<vf32<float>, int>();
	binary_op_return_type<vf32<float>, float>();

	binary_op_return_type<vf32<float>, vf32<schar>>();
	binary_op_return_type<vf32<float>, vf32<uchar>>();
	binary_op_return_type<vf32<float>, vf32<short>>();
	binary_op_return_type<vf32<float>, vf32<ushort>>();
	binary_op_return_type<vf32<float>, vf32<float>>();

	VERIFY((is_substitution_failure<vfloat, uint>));
	VERIFY((is_substitution_failure<vfloat, long>));
	VERIFY((is_substitution_failure<vfloat, ulong>));
	VERIFY((is_substitution_failure<vfloat, llong>));
	VERIFY((is_substitution_failure<vfloat, ullong>));
	VERIFY((is_substitution_failure<vfloat, double>));
	VERIFY((is_substitution_failure<vfloat, vf32<schar>>));
	VERIFY((is_substitution_failure<vfloat, vf32<uchar>>));
	VERIFY((is_substitution_failure<vfloat, vf32<short>>));
	VERIFY((is_substitution_failure<vfloat, vf32<ushort>>));
	VERIFY((is_substitution_failure<vfloat, vf32<int>>));
	VERIFY((is_substitution_failure<vfloat, vf32<uint>>));
	VERIFY((is_substitution_failure<vfloat, vf32<long>>));
	VERIFY((is_substitution_failure<vfloat, vf32<ulong>>));
	VERIFY((is_substitution_failure<vfloat, vf32<llong>>));
	VERIFY((is_substitution_failure<vfloat, vf32<ullong>>));
	VERIFY((is_substitution_failure<vfloat, vf32<float>>));

	VERIFY((is_substitution_failure<vf32<float>, vfloat>));
	VERIFY((is_substitution_failure<vf32<float>, uint>));
	VERIFY((is_substitution_failure<vf32<float>, long>));
	VERIFY((is_substitution_failure<vf32<float>, ulong>));
	VERIFY((is_substitution_failure<vf32<float>, llong>));
	VERIFY((is_substitution_failure<vf32<float>, ullong>));
	VERIFY((is_substitution_failure<vf32<float>, double>));
	VERIFY((is_substitution_failure<vf32<float>, vf32<int>>));
	VERIFY((is_substitution_failure<vf32<float>, vf32<uint>>));
	VERIFY((is_substitution_failure<vf32<float>, vf32<long>>));
	VERIFY((is_substitution_failure<vf32<float>, vf32<ulong>>));
	VERIFY((is_substitution_failure<vf32<float>, vf32<llong>>));
	VERIFY((is_substitution_failure<vf32<float>, vf32<ullong>>));

	VERIFY((is_substitution_failure<vfloat, vf32<double>>));
      }
    else if constexpr (std::is_same_v<V, vdouble>)
      {
	binary_op_return_type<vdouble, float, vdouble>();
	binary_op_return_type<vdouble, schar>();
	binary_op_return_type<vdouble, uchar>();
	binary_op_return_type<vdouble, short>();
	binary_op_return_type<vdouble, ushort>();
	binary_op_return_type<vdouble, uint>();

	binary_op_return_type<vf64<double>, schar>();
	binary_op_return_type<vf64<double>, uchar>();
	binary_op_return_type<vf64<double>, short>();
	binary_op_return_type<vf64<double>, ushort>();
	binary_op_return_type<vf64<double>, uint>();
	binary_op_return_type<vf64<double>, int, vf64<double>>();
	binary_op_return_type<vf64<double>, float, vf64<double>>();
	binary_op_return_type<vf64<double>, double, vf64<double>>();
	binary_op_return_type<vf64<double>, vf64<double>, vf64<double>>();
	binary_op_return_type<vf32<double>, schar>();
	binary_op_return_type<vf32<double>, uchar>();
	binary_op_return_type<vf32<double>, short>();
	binary_op_return_type<vf32<double>, ushort>();
	binary_op_return_type<vf32<double>, uint>();
	binary_op_return_type<vf32<double>, int, vf32<double>>();
	binary_op_return_type<vf32<double>, float, vf32<double>>();
	binary_op_return_type<vf32<double>, double, vf32<double>>();
	binary_op_return_type<vf64<double>, vf64<schar>>();
	binary_op_return_type<vf64<double>, vf64<uchar>>();
	binary_op_return_type<vf64<double>, vf64<short>>();
	binary_op_return_type<vf64<double>, vf64<ushort>>();
	binary_op_return_type<vf64<double>, vf64<int>>();
	binary_op_return_type<vf64<double>, vf64<uint>>();
	binary_op_return_type<vf64<double>, vf64<float>>();

	VERIFY((is_substitution_failure<vdouble, llong>));
	VERIFY((is_substitution_failure<vdouble, ullong>));
	VERIFY((is_substitution_failure<vdouble, vf64<schar>>));
	VERIFY((is_substitution_failure<vdouble, vf64<uchar>>));
	VERIFY((is_substitution_failure<vdouble, vf64<short>>));
	VERIFY((is_substitution_failure<vdouble, vf64<ushort>>));
	VERIFY((is_substitution_failure<vdouble, vf64<int>>));
	VERIFY((is_substitution_failure<vdouble, vf64<uint>>));
	VERIFY((is_substitution_failure<vdouble, vf64<long>>));
	VERIFY((is_substitution_failure<vdouble, vf64<ulong>>));
	VERIFY((is_substitution_failure<vdouble, vf64<llong>>));
	VERIFY((is_substitution_failure<vdouble, vf64<ullong>>));
	VERIFY((is_substitution_failure<vdouble, vf64<float>>));
	VERIFY((is_substitution_failure<vdouble, vf64<double>>));

	VERIFY((is_substitution_failure<vf64<double>, vdouble>));
	VERIFY((is_substitution_failure<vf64<double>, llong>));
	VERIFY((is_substitution_failure<vf64<double>, ullong>));
	VERIFY((is_substitution_failure<vf64<double>, vf64<llong>>));
	VERIFY((is_substitution_failure<vf64<double>, vf64<ullong>>));

	VERIFY((is_substitution_failure<vf32<double>, llong>));
	VERIFY((is_substitution_failure<vf32<double>, ullong>));

	if constexpr (sizeof(long) == sizeof(llong))
	  {
	    VERIFY((is_substitution_failure<vdouble, long>));
	    VERIFY((is_substitution_failure<vdouble, ulong>));
	    VERIFY((is_substitution_failure<vf64<double>, long>));
	    VERIFY((is_substitution_failure<vf64<double>, ulong>));
	    VERIFY((is_substitution_failure<vf64<double>, vf64<long>>));
	    VERIFY((is_substitution_failure<vf64<double>, vf64<ulong>>));
	    VERIFY((is_substitution_failure<vf32<double>, long>));
	    VERIFY((is_substitution_failure<vf32<double>, ulong>));
	  }
	else
	  {
	    binary_op_return_type<vdouble, long>();
	    binary_op_return_type<vdouble, ulong>();
	    binary_op_return_type<vf64<double>, long>();
	    binary_op_return_type<vf64<double>, ulong>();
	    binary_op_return_type<vf64<double>, vf64<long>>();
	    binary_op_return_type<vf64<double>, vf64<ulong>>();
	    binary_op_return_type<vf32<double>, long>();
	    binary_op_return_type<vf32<double>, ulong>();
	  }
      }
    else if constexpr (std::is_same_v<V, vldouble>)
      {
	binary_op_return_type<vldouble, schar>();
	binary_op_return_type<vldouble, uchar>();
	binary_op_return_type<vldouble, short>();
	binary_op_return_type<vldouble, ushort>();
	binary_op_return_type<vldouble, uint>();
	binary_op_return_type<vldouble, long>();
	binary_op_return_type<vldouble, ulong>();
	binary_op_return_type<vldouble, float>();
	binary_op_return_type<vldouble, double>();

	binary_op_return_type<vf64<long double>, schar>();
	binary_op_return_type<vf64<long double>, uchar>();
	binary_op_return_type<vf64<long double>, short>();
	binary_op_return_type<vf64<long double>, ushort>();
	binary_op_return_type<vf64<long double>, int>();
	binary_op_return_type<vf64<long double>, uint>();
	binary_op_return_type<vf64<long double>, long>();
	binary_op_return_type<vf64<long double>, ulong>();
	binary_op_return_type<vf64<long double>, float>();
	binary_op_return_type<vf64<long double>, double>();
	binary_op_return_type<vf64<long double>, vf64<long double>>();

	using std::experimental::simd;
	using A = simd_abi::fixed_size<vldouble::size()>;
	binary_op_return_type<simd<long double, A>, schar>();
	binary_op_return_type<simd<long double, A>, uchar>();
	binary_op_return_type<simd<long double, A>, short>();
	binary_op_return_type<simd<long double, A>, ushort>();
	binary_op_return_type<simd<long double, A>, int>();
	binary_op_return_type<simd<long double, A>, uint>();
	binary_op_return_type<simd<long double, A>, long>();
	binary_op_return_type<simd<long double, A>, ulong>();
	binary_op_return_type<simd<long double, A>, float>();
	binary_op_return_type<simd<long double, A>, double>();

	if constexpr (sizeof(ldouble) == sizeof(double))
	  {
	    VERIFY((is_substitution_failure<vldouble, llong>));
	    VERIFY((is_substitution_failure<vldouble, ullong>));
	    VERIFY((is_substitution_failure<vf64<ldouble>, llong>));
	    VERIFY((is_substitution_failure<vf64<ldouble>, ullong>));
	    VERIFY((is_substitution_failure<simd<ldouble, A>, llong>));
	    VERIFY((is_substitution_failure<simd<ldouble, A>, ullong>));
	  }
	else
	  {
	    binary_op_return_type<vldouble, llong>();
	    binary_op_return_type<vldouble, ullong>();
	    binary_op_return_type<vf64<long double>, llong>();
	    binary_op_return_type<vf64<long double>, ullong>();
	    binary_op_return_type<simd<long double, A>, llong>();
	    binary_op_return_type<simd<long double, A>, ullong>();
	  }

	VERIFY((is_substitution_failure<vf64<long double>, vldouble>));
	COMPARE((is_substitution_failure<simd<long double, A>, vldouble>),
		(!std::is_same<A, vldouble::abi_type>::value));
      }
    else if constexpr (std::is_same_v<V, vlong>)
      {
	VERIFY((is_substitution_failure<vi32<long>, double>));
	VERIFY((is_substitution_failure<vi32<long>, float>));
	VERIFY((is_substitution_failure<vi32<long>, vi32<float>>));
	if constexpr (sizeof(long) == sizeof(llong))
	  {
	    binary_op_return_type<vlong, uint>();
	    binary_op_return_type<vlong, llong>();
	    binary_op_return_type<vi32<long>, uint>();
	    binary_op_return_type<vi32<long>, llong>();
	    binary_op_return_type<vi64<long>, uint>();
	    binary_op_return_type<vi64<long>, llong>();
	    binary_op_return_type<vi32<long>, vi32<uint>>();
	    binary_op_return_type<vi64<long>, vi64<uint>>();
	    VERIFY((is_substitution_failure<vi32<long>, vi32<double>>));
	    VERIFY((is_substitution_failure<vi64<long>, vi64<double>>));
	  }
	else
	  {
	    VERIFY((is_substitution_failure<vlong, uint>));
	    VERIFY((is_substitution_failure<vlong, llong>));
	    VERIFY((is_substitution_failure<vi32<long>, uint>));
	    VERIFY((is_substitution_failure<vi32<long>, llong>));
	    VERIFY((is_substitution_failure<vi64<long>, uint>));
	    VERIFY((is_substitution_failure<vi64<long>, llong>));
	    VERIFY((is_substitution_failure<vi32<long>, vi32<uint>>));
	    VERIFY((is_substitution_failure<vi64<long>, vi64<uint>>));
	    binary_op_return_type<vi32<double>, vi32<long>>();
	    binary_op_return_type<vi64<double>, vi64<long>>();
	  }

	binary_op_return_type<vlong, schar, vlong>();
	binary_op_return_type<vlong, uchar, vlong>();
	binary_op_return_type<vlong, short, vlong>();
	binary_op_return_type<vlong, ushort, vlong>();

	binary_op_return_type<vi32<long>, schar, vi32<long>>();
	binary_op_return_type<vi32<long>, uchar, vi32<long>>();
	binary_op_return_type<vi32<long>, short, vi32<long>>();
	binary_op_return_type<vi32<long>, ushort, vi32<long>>();
	binary_op_return_type<vi32<long>, int, vi32<long>>();
	binary_op_return_type<vi32<long>, long, vi32<long>>();
	binary_op_return_type<vi32<long>, vi32<long>, vi32<long>>();
	binary_op_return_type<vi64<long>, schar, vi64<long>>();
	binary_op_return_type<vi64<long>, uchar, vi64<long>>();
	binary_op_return_type<vi64<long>, short, vi64<long>>();
	binary_op_return_type<vi64<long>, ushort, vi64<long>>();
	binary_op_return_type<vi64<long>, int, vi64<long>>();
	binary_op_return_type<vi64<long>, long, vi64<long>>();
	binary_op_return_type<vi64<long>, vi64<long>, vi64<long>>();

	VERIFY((is_substitution_failure<vlong, vulong>));
	VERIFY((is_substitution_failure<vlong, ulong>));
	VERIFY((is_substitution_failure<vlong, ullong>));
	VERIFY((is_substitution_failure<vlong, float>));
	VERIFY((is_substitution_failure<vlong, double>));
	VERIFY((is_substitution_failure<vlong, vl<schar>>));
	VERIFY((is_substitution_failure<vlong, vl<uchar>>));
	VERIFY((is_substitution_failure<vlong, vl<short>>));
	VERIFY((is_substitution_failure<vlong, vl<ushort>>));
	VERIFY((is_substitution_failure<vlong, vl<int>>));
	VERIFY((is_substitution_failure<vlong, vl<uint>>));
	VERIFY((is_substitution_failure<vlong, vl<long>>));
	VERIFY((is_substitution_failure<vlong, vl<ulong>>));
	VERIFY((is_substitution_failure<vlong, vl<llong>>));
	VERIFY((is_substitution_failure<vlong, vl<ullong>>));
	VERIFY((is_substitution_failure<vlong, vl<float>>));
	VERIFY((is_substitution_failure<vlong, vl<double>>));
	VERIFY((is_substitution_failure<vl<long>, vlong>));
	VERIFY((is_substitution_failure<vl<long>, vulong>));
	VERIFY((is_substitution_failure<vi32<long>, ulong>));
	VERIFY((is_substitution_failure<vi32<long>, ullong>));
	binary_op_return_type<vi32<long>, vi32<schar>>();
	binary_op_return_type<vi32<long>, vi32<uchar>>();
	binary_op_return_type<vi32<long>, vi32<short>>();
	binary_op_return_type<vi32<long>, vi32<ushort>>();
	binary_op_return_type<vi32<long>, vi32<int>>();
	VERIFY((is_substitution_failure<vi32<long>, vi32<ulong>>));
	VERIFY((is_substitution_failure<vi32<long>, vi32<ullong>>));
	VERIFY((is_substitution_failure<vi64<long>, ulong>));
	VERIFY((is_substitution_failure<vi64<long>, ullong>));
	VERIFY((is_substitution_failure<vi64<long>, float>));
	VERIFY((is_substitution_failure<vi64<long>, double>));
	binary_op_return_type<vi64<long>, vi64<schar>>();
	binary_op_return_type<vi64<long>, vi64<uchar>>();
	binary_op_return_type<vi64<long>, vi64<short>>();
	binary_op_return_type<vi64<long>, vi64<ushort>>();
	binary_op_return_type<vi64<long>, vi64<int>>();
	VERIFY((is_substitution_failure<vi64<long>, vi64<ulong>>));
	VERIFY((is_substitution_failure<vi64<long>, vi64<ullong>>));
	VERIFY((is_substitution_failure<vi64<long>, vi64<float>>));

	binary_op_return_type<vi32<llong>, vi32<long>>();
	binary_op_return_type<vi64<llong>, vi64<long>>();
      }
    else if constexpr (std::is_same_v<V, vulong>)
      {
	if constexpr (sizeof(long) == sizeof(llong))
	  {
	    binary_op_return_type<vulong, ullong, vulong>();
	    binary_op_return_type<vi32<ulong>, ullong, vi32<ulong>>();
	    binary_op_return_type<vi64<ulong>, ullong, vi64<ulong>>();
	    VERIFY((is_substitution_failure<vi32<ulong>, vi32<llong>>));
	    VERIFY((is_substitution_failure<vi32<ulong>, vi32<double>>));
	    VERIFY((is_substitution_failure<vi64<ulong>, vi64<llong>>));
	    VERIFY((is_substitution_failure<vi64<ulong>, vi64<double>>));
	  }
	else
	  {
	    VERIFY((is_substitution_failure<vulong, ullong>));
	    VERIFY((is_substitution_failure<vi32<ulong>, ullong>));
	    VERIFY((is_substitution_failure<vi64<ulong>, ullong>));
	    binary_op_return_type<vi32<llong>, vi32<ulong>>();
	    binary_op_return_type<vi32<double>, vi32<ulong>>();
	    binary_op_return_type<vi64<llong>, vi64<ulong>>();
	    binary_op_return_type<vi64<double>, vi64<ulong>>();
	  }

	binary_op_return_type<vulong, uchar, vulong>();
	binary_op_return_type<vulong, ushort, vulong>();
	binary_op_return_type<vulong, uint, vulong>();
	binary_op_return_type<vi32<ulong>, uchar, vi32<ulong>>();
	binary_op_return_type<vi32<ulong>, ushort, vi32<ulong>>();
	binary_op_return_type<vi32<ulong>, int, vi32<ulong>>();
	binary_op_return_type<vi32<ulong>, uint, vi32<ulong>>();
	binary_op_return_type<vi32<ulong>, ulong, vi32<ulong>>();
	binary_op_return_type<vi32<ulong>, vi32<ulong>, vi32<ulong>>();
	binary_op_return_type<vi64<ulong>, uchar, vi64<ulong>>();
	binary_op_return_type<vi64<ulong>, ushort, vi64<ulong>>();
	binary_op_return_type<vi64<ulong>, int, vi64<ulong>>();
	binary_op_return_type<vi64<ulong>, uint, vi64<ulong>>();
	binary_op_return_type<vi64<ulong>, ulong, vi64<ulong>>();
	binary_op_return_type<vi64<ulong>, vi64<ulong>, vi64<ulong>>();

	VERIFY((is_substitution_failure<vi32<ulong>, llong>));
	VERIFY((is_substitution_failure<vi32<ulong>, float>));
	VERIFY((is_substitution_failure<vi32<ulong>, double>));
	VERIFY((is_substitution_failure<vi32<ulong>, vi32<float>>));
	VERIFY((is_substitution_failure<vi64<ulong>, vi64<float>>));
	VERIFY((is_substitution_failure<vulong, schar>));
	VERIFY((is_substitution_failure<vulong, short>));
	VERIFY((is_substitution_failure<vulong, vlong>));
	VERIFY((is_substitution_failure<vulong, long>));
	VERIFY((is_substitution_failure<vulong, llong>));
	VERIFY((is_substitution_failure<vulong, float>));
	VERIFY((is_substitution_failure<vulong, double>));
	VERIFY((is_substitution_failure<vulong, vl<schar>>));
	VERIFY((is_substitution_failure<vulong, vl<uchar>>));
	VERIFY((is_substitution_failure<vulong, vl<short>>));
	VERIFY((is_substitution_failure<vulong, vl<ushort>>));
	VERIFY((is_substitution_failure<vulong, vl<int>>));
	VERIFY((is_substitution_failure<vulong, vl<uint>>));
	VERIFY((is_substitution_failure<vulong, vl<long>>));
	VERIFY((is_substitution_failure<vulong, vl<ulong>>));
	VERIFY((is_substitution_failure<vulong, vl<llong>>));
	VERIFY((is_substitution_failure<vulong, vl<ullong>>));
	VERIFY((is_substitution_failure<vulong, vl<float>>));
	VERIFY((is_substitution_failure<vulong, vl<double>>));
	VERIFY((is_substitution_failure<vl<ulong>, vlong>));
	VERIFY((is_substitution_failure<vl<ulong>, vulong>));
	VERIFY((is_substitution_failure<vi32<ulong>, schar>));
	VERIFY((is_substitution_failure<vi32<ulong>, short>));
	VERIFY((is_substitution_failure<vi32<ulong>, long>));
	VERIFY((is_substitution_failure<vi32<ulong>, vi32<schar>>));
	binary_op_return_type<vi32<ulong>, vi32<uchar>>();
	VERIFY((is_substitution_failure<vi32<ulong>, vi32<short>>));
	binary_op_return_type<vi32<ulong>, vi32<ushort>>();
	VERIFY((is_substitution_failure<vi32<ulong>, vi32<int>>));
	binary_op_return_type<vi32<ulong>, vi32<uint>>();
	VERIFY((is_substitution_failure<vi32<ulong>, vi32<long>>));
	binary_op_return_type<vi32<ullong>, vi32<ulong>>();
	VERIFY((is_substitution_failure<vi64<ulong>, schar>));
	VERIFY((is_substitution_failure<vi64<ulong>, short>));
	VERIFY((is_substitution_failure<vi64<ulong>, long>));
	VERIFY((is_substitution_failure<vi64<ulong>, llong>));
	VERIFY((is_substitution_failure<vi64<ulong>, float>));
	VERIFY((is_substitution_failure<vi64<ulong>, double>));
	VERIFY((is_substitution_failure<vi64<ulong>, vi64<schar>>));
	binary_op_return_type<vi64<ulong>, vi64<uchar>>();
	VERIFY((is_substitution_failure<vi64<ulong>, vi64<short>>));
	binary_op_return_type<vi64<ulong>, vi64<ushort>>();
	VERIFY((is_substitution_failure<vi64<ulong>, vi64<int>>));
	binary_op_return_type<vi64<ulong>, vi64<uint>>();
	VERIFY((is_substitution_failure<vi64<ulong>, vi64<long>>));
	binary_op_return_type<vi64<ullong>, vi64<ulong>>();
      }
    else if constexpr (std::is_same_v<V, vllong>)
      {
	binary_op_return_type<vllong, schar, vllong>();
	binary_op_return_type<vllong, uchar, vllong>();
	binary_op_return_type<vllong, short, vllong>();
	binary_op_return_type<vllong, ushort, vllong>();
	binary_op_return_type<vllong, uint, vllong>();
	binary_op_return_type<vllong, long, vllong>();
	binary_op_return_type<vi32<llong>, schar, vi32<llong>>();
	binary_op_return_type<vi32<llong>, uchar, vi32<llong>>();
	binary_op_return_type<vi32<llong>, short, vi32<llong>>();
	binary_op_return_type<vi32<llong>, ushort, vi32<llong>>();
	binary_op_return_type<vi32<llong>, int, vi32<llong>>();
	binary_op_return_type<vi32<llong>, uint, vi32<llong>>();
	binary_op_return_type<vi32<llong>, long, vi32<llong>>();
	binary_op_return_type<vi32<llong>, llong, vi32<llong>>();
	binary_op_return_type<vi32<llong>, vi32<llong>, vi32<llong>>();
	binary_op_return_type<vi64<llong>, schar, vi64<llong>>();
	binary_op_return_type<vi64<llong>, uchar, vi64<llong>>();
	binary_op_return_type<vi64<llong>, short, vi64<llong>>();
	binary_op_return_type<vi64<llong>, ushort, vi64<llong>>();
	binary_op_return_type<vi64<llong>, int, vi64<llong>>();
	binary_op_return_type<vi64<llong>, uint, vi64<llong>>();
	binary_op_return_type<vi64<llong>, long, vi64<llong>>();
	binary_op_return_type<vi64<llong>, llong, vi64<llong>>();
	binary_op_return_type<vi64<llong>, vi64<llong>>();
	binary_op_return_type<vi32<llong>, vi32<schar>>();
	binary_op_return_type<vi32<llong>, vi32<uchar>>();
	binary_op_return_type<vi32<llong>, vi32<short>>();
	binary_op_return_type<vi32<llong>, vi32<ushort>>();
	binary_op_return_type<vi32<llong>, vi32<int>>();
	binary_op_return_type<vi32<llong>, vi32<uint>>();
	binary_op_return_type<vi32<llong>, vi32<long>>();
	if constexpr (sizeof(long) == sizeof(llong))
	  {
	    VERIFY((is_substitution_failure<vi32<llong>, vi32<ulong>>));
	    VERIFY((is_substitution_failure<vi32<llong>, ulong>));
	    VERIFY((is_substitution_failure<vi64<llong>, ulong>));
	    VERIFY((is_substitution_failure<vllong, ulong>));
	  }
	else
	  {
	    binary_op_return_type<vi32<llong>, vi32<ulong>>();
	    binary_op_return_type<vi32<llong>, ulong>();
	    binary_op_return_type<vi64<llong>, ulong>();
	    binary_op_return_type<vllong, ulong>();
	  }

	VERIFY((is_substitution_failure<vllong, vullong>));
	VERIFY((is_substitution_failure<vllong, ullong>));
	VERIFY((is_substitution_failure<vllong, float>));
	VERIFY((is_substitution_failure<vllong, double>));
	VERIFY((is_substitution_failure<vllong, vi64<schar>>));
	VERIFY((is_substitution_failure<vllong, vi64<uchar>>));
	VERIFY((is_substitution_failure<vllong, vi64<short>>));
	VERIFY((is_substitution_failure<vllong, vi64<ushort>>));
	VERIFY((is_substitution_failure<vllong, vi64<int>>));
	VERIFY((is_substitution_failure<vllong, vi64<uint>>));
	VERIFY((is_substitution_failure<vllong, vi64<long>>));
	VERIFY((is_substitution_failure<vllong, vi64<ulong>>));
	VERIFY((is_substitution_failure<vllong, vi64<llong>>));
	VERIFY((is_substitution_failure<vllong, vi64<ullong>>));
	VERIFY((is_substitution_failure<vllong, vi64<float>>));
	VERIFY((is_substitution_failure<vllong, vi64<double>>));
	VERIFY((is_substitution_failure<vi32<llong>, ullong>));
	VERIFY((is_substitution_failure<vi32<llong>, float>));
	VERIFY((is_substitution_failure<vi32<llong>, double>));
	VERIFY((is_substitution_failure<vi32<llong>, vi32<ullong>>));
	VERIFY((is_substitution_failure<vi32<llong>, vi32<float>>));
	VERIFY((is_substitution_failure<vi32<llong>, vi32<double>>));
	VERIFY((is_substitution_failure<vi64<llong>, vllong>));
	VERIFY((is_substitution_failure<vi64<llong>, vullong>));
	VERIFY((is_substitution_failure<vi64<llong>, ullong>));
	VERIFY((is_substitution_failure<vi64<llong>, float>));
	VERIFY((is_substitution_failure<vi64<llong>, double>));
	binary_op_return_type<vi64<llong>, vi64<schar>>();
	binary_op_return_type<vi64<llong>, vi64<uchar>>();
	binary_op_return_type<vi64<llong>, vi64<short>>();
	binary_op_return_type<vi64<llong>, vi64<ushort>>();
	binary_op_return_type<vi64<llong>, vi64<int>>();
	binary_op_return_type<vi64<llong>, vi64<uint>>();
	binary_op_return_type<vi64<llong>, vi64<long>>();
	if constexpr (sizeof(long) == sizeof(llong))
	  {
	    VERIFY((is_substitution_failure<vi64<llong>, vi64<ulong>>));
	  }
	else
	  {
	    binary_op_return_type<vi64<llong>, vi64<ulong>>();
	  }
	VERIFY((is_substitution_failure<vi64<llong>, vi64<ullong>>));
	VERIFY((is_substitution_failure<vi64<llong>, vi64<float>>));
	VERIFY((is_substitution_failure<vi64<llong>, vi64<double>>));
      }
    else if constexpr (std::is_same_v<V, vullong>)
      {
	binary_op_return_type<vullong, uchar, vullong>();
	binary_op_return_type<vullong, ushort, vullong>();
	binary_op_return_type<vullong, uint, vullong>();
	binary_op_return_type<vullong, ulong, vullong>();
	binary_op_return_type<vi32<ullong>, uchar, vi32<ullong>>();
	binary_op_return_type<vi32<ullong>, ushort, vi32<ullong>>();
	binary_op_return_type<vi32<ullong>, int, vi32<ullong>>();
	binary_op_return_type<vi32<ullong>, uint, vi32<ullong>>();
	binary_op_return_type<vi32<ullong>, ulong, vi32<ullong>>();
	binary_op_return_type<vi32<ullong>, ullong, vi32<ullong>>();
	binary_op_return_type<vi32<ullong>, vi32<ullong>, vi32<ullong>>();
	binary_op_return_type<vi64<ullong>, uchar, vi64<ullong>>();
	binary_op_return_type<vi64<ullong>, ushort, vi64<ullong>>();
	binary_op_return_type<vi64<ullong>, int, vi64<ullong>>();
	binary_op_return_type<vi64<ullong>, uint, vi64<ullong>>();
	binary_op_return_type<vi64<ullong>, ulong, vi64<ullong>>();
	binary_op_return_type<vi64<ullong>, ullong, vi64<ullong>>();
	binary_op_return_type<vi64<ullong>, vi64<ullong>, vi64<ullong>>();

	VERIFY((is_substitution_failure<vullong, schar>));
	VERIFY((is_substitution_failure<vullong, short>));
	VERIFY((is_substitution_failure<vullong, long>));
	VERIFY((is_substitution_failure<vullong, llong>));
	VERIFY((is_substitution_failure<vullong, vllong>));
	VERIFY((is_substitution_failure<vullong, float>));
	VERIFY((is_substitution_failure<vullong, double>));
	VERIFY((is_substitution_failure<vullong, vi64<schar>>));
	VERIFY((is_substitution_failure<vullong, vi64<uchar>>));
	VERIFY((is_substitution_failure<vullong, vi64<short>>));
	VERIFY((is_substitution_failure<vullong, vi64<ushort>>));
	VERIFY((is_substitution_failure<vullong, vi64<int>>));
	VERIFY((is_substitution_failure<vullong, vi64<uint>>));
	VERIFY((is_substitution_failure<vullong, vi64<long>>));
	VERIFY((is_substitution_failure<vullong, vi64<ulong>>));
	VERIFY((is_substitution_failure<vullong, vi64<llong>>));
	VERIFY((is_substitution_failure<vullong, vi64<ullong>>));
	VERIFY((is_substitution_failure<vullong, vi64<float>>));
	VERIFY((is_substitution_failure<vullong, vi64<double>>));
	VERIFY((is_substitution_failure<vi32<ullong>, schar>));
	VERIFY((is_substitution_failure<vi32<ullong>, short>));
	VERIFY((is_substitution_failure<vi32<ullong>, long>));
	VERIFY((is_substitution_failure<vi32<ullong>, llong>));
	VERIFY((is_substitution_failure<vi32<ullong>, float>));
	VERIFY((is_substitution_failure<vi32<ullong>, double>));
	VERIFY((is_substitution_failure<vi32<ullong>, vi32<schar>>));
	binary_op_return_type<vi32<ullong>, vi32<uchar>>();
	VERIFY((is_substitution_failure<vi32<ullong>, vi32<short>>));
	binary_op_return_type<vi32<ullong>, vi32<ushort>>();
	VERIFY((is_substitution_failure<vi32<ullong>, vi32<int>>));
	binary_op_return_type<vi32<ullong>, vi32<uint>>();
	VERIFY((is_substitution_failure<vi32<ullong>, vi32<long>>));
	binary_op_return_type<vi32<ullong>, vi32<ulong>>();
	VERIFY((is_substitution_failure<vi32<ullong>, vi32<llong>>));
	VERIFY((is_substitution_failure<vi32<ullong>, vi32<float>>));
	VERIFY((is_substitution_failure<vi32<ullong>, vi32<double>>));
	VERIFY((is_substitution_failure<vi64<ullong>, schar>));
	VERIFY((is_substitution_failure<vi64<ullong>, short>));
	VERIFY((is_substitution_failure<vi64<ullong>, long>));
	VERIFY((is_substitution_failure<vi64<ullong>, llong>));
	VERIFY((is_substitution_failure<vi64<ullong>, vllong>));
	VERIFY((is_substitution_failure<vi64<ullong>, vullong>));
	VERIFY((is_substitution_failure<vi64<ullong>, float>));
	VERIFY((is_substitution_failure<vi64<ullong>, double>));
	VERIFY((is_substitution_failure<vi64<ullong>, vi64<schar>>));
	binary_op_return_type<vi64<ullong>, vi64<uchar>>();
	VERIFY((is_substitution_failure<vi64<ullong>, vi64<short>>));
	binary_op_return_type<vi64<ullong>, vi64<ushort>>();
	VERIFY((is_substitution_failure<vi64<ullong>, vi64<int>>));
	binary_op_return_type<vi64<ullong>, vi64<uint>>();
	VERIFY((is_substitution_failure<vi64<ullong>, vi64<long>>));
	binary_op_return_type<vi64<ullong>, vi64<ulong>>();
	VERIFY((is_substitution_failure<vi64<ullong>, vi64<llong>>));
	VERIFY((is_substitution_failure<vi64<ullong>, vi64<float>>));
	VERIFY((is_substitution_failure<vi64<ullong>, vi64<double>>));
      }
    else if constexpr (std::is_same_v<V, vint>)
      {
	binary_op_return_type<vint, schar, vint>();
	binary_op_return_type<vint, uchar, vint>();
	binary_op_return_type<vint, short, vint>();
	binary_op_return_type<vint, ushort, vint>();
	binary_op_return_type<vi32<int>, schar, vi32<int>>();
	binary_op_return_type<vi32<int>, uchar, vi32<int>>();
	binary_op_return_type<vi32<int>, short, vi32<int>>();
	binary_op_return_type<vi32<int>, ushort, vi32<int>>();
	binary_op_return_type<vi32<int>, int, vi32<int>>();
	binary_op_return_type<vi32<int>, vi32<int>, vi32<int>>();
	binary_op_return_type<vi32<int>, vi32<schar>>();
	binary_op_return_type<vi32<int>, vi32<uchar>>();
	binary_op_return_type<vi32<int>, vi32<short>>();
	binary_op_return_type<vi32<int>, vi32<ushort>>();

	binary_op_return_type<vi32<llong>, vi32<int>>();
	binary_op_return_type<vi32<double>, vi32<int>>();

	// order is important for MSVC. This compiler is just crazy: It considers
	// operators from unrelated simd template instantiations as candidates -
	// but only after they have been tested. So e.g. vi32<int> + llong will
	// produce a vi32<llong> if a vi32<llong> operator test is done before the
	// vi32<int> + llong test.
	VERIFY((is_substitution_failure<vi32<int>, double>));
	VERIFY((is_substitution_failure<vi32<int>, float>));
	VERIFY((is_substitution_failure<vi32<int>, llong>));
	VERIFY((is_substitution_failure<vi32<int>, vi32<float>>));
	VERIFY((is_substitution_failure<vint, vuint>));
	VERIFY((is_substitution_failure<vint, uint>));
	VERIFY((is_substitution_failure<vint, ulong>));
	VERIFY((is_substitution_failure<vint, llong>));
	VERIFY((is_substitution_failure<vint, ullong>));
	VERIFY((is_substitution_failure<vint, float>));
	VERIFY((is_substitution_failure<vint, double>));
	VERIFY((is_substitution_failure<vint, vi32<schar>>));
	VERIFY((is_substitution_failure<vint, vi32<uchar>>));
	VERIFY((is_substitution_failure<vint, vi32<short>>));
	VERIFY((is_substitution_failure<vint, vi32<ushort>>));
	VERIFY((is_substitution_failure<vint, vi32<int>>));
	VERIFY((is_substitution_failure<vint, vi32<uint>>));
	VERIFY((is_substitution_failure<vint, vi32<long>>));
	VERIFY((is_substitution_failure<vint, vi32<ulong>>));
	VERIFY((is_substitution_failure<vint, vi32<llong>>));
	VERIFY((is_substitution_failure<vint, vi32<ullong>>));
	VERIFY((is_substitution_failure<vint, vi32<float>>));
	VERIFY((is_substitution_failure<vint, vi32<double>>));
	VERIFY((is_substitution_failure<vi32<int>, vint>));
	VERIFY((is_substitution_failure<vi32<int>, vuint>));
	VERIFY((is_substitution_failure<vi32<int>, uint>));
	VERIFY((is_substitution_failure<vi32<int>, ulong>));
	VERIFY((is_substitution_failure<vi32<int>, ullong>));
	VERIFY((is_substitution_failure<vi32<int>, vi32<uint>>));
	VERIFY((is_substitution_failure<vi32<int>, vi32<ulong>>));
	VERIFY((is_substitution_failure<vi32<int>, vi32<ullong>>));

	binary_op_return_type<vi32<long>, vi32<int>>();
	if constexpr (sizeof(long) == sizeof(llong))
	  {
	    VERIFY((is_substitution_failure<vint, long>));
	    VERIFY((is_substitution_failure<vi32<int>, long>));
	  }
	else
	  {
	    binary_op_return_type<vint, long>();
	    binary_op_return_type<vi32<int>, long>();
	  }
      }
    else if constexpr (std::is_same_v<V, vuint>)
      {
	VERIFY((is_substitution_failure<vi32<uint>, llong>));
	VERIFY((is_substitution_failure<vi32<uint>, ullong>));
	VERIFY((is_substitution_failure<vi32<uint>, float>));
	VERIFY((is_substitution_failure<vi32<uint>, double>));
	VERIFY((is_substitution_failure<vi32<uint>, vi32<float>>));

	binary_op_return_type<vuint, uchar, vuint>();
	binary_op_return_type<vuint, ushort, vuint>();
	binary_op_return_type<vi32<uint>, uchar, vi32<uint>>();
	binary_op_return_type<vi32<uint>, ushort, vi32<uint>>();
	binary_op_return_type<vi32<uint>, int, vi32<uint>>();
	binary_op_return_type<vi32<uint>, uint, vi32<uint>>();
	binary_op_return_type<vi32<uint>, vi32<uint>, vi32<uint>>();
	binary_op_return_type<vi32<uint>, vi32<uchar>>();
	binary_op_return_type<vi32<uint>, vi32<ushort>>();

	binary_op_return_type<vi32<llong>, vi32<uint>>();
	binary_op_return_type<vi32<ullong>, vi32<uint>>();
	binary_op_return_type<vi32<double>, vi32<uint>>();

	VERIFY((is_substitution_failure<vuint, schar>));
	VERIFY((is_substitution_failure<vuint, short>));
	VERIFY((is_substitution_failure<vuint, vint>));
	VERIFY((is_substitution_failure<vuint, long>));
	VERIFY((is_substitution_failure<vuint, llong>));
	VERIFY((is_substitution_failure<vuint, ullong>));
	VERIFY((is_substitution_failure<vuint, float>));
	VERIFY((is_substitution_failure<vuint, double>));
	VERIFY((is_substitution_failure<vuint, vi32<schar>>));
	VERIFY((is_substitution_failure<vuint, vi32<uchar>>));
	VERIFY((is_substitution_failure<vuint, vi32<short>>));
	VERIFY((is_substitution_failure<vuint, vi32<ushort>>));
	VERIFY((is_substitution_failure<vuint, vi32<int>>));
	VERIFY((is_substitution_failure<vuint, vi32<uint>>));
	VERIFY((is_substitution_failure<vuint, vi32<long>>));
	VERIFY((is_substitution_failure<vuint, vi32<ulong>>));
	VERIFY((is_substitution_failure<vuint, vi32<llong>>));
	VERIFY((is_substitution_failure<vuint, vi32<ullong>>));
	VERIFY((is_substitution_failure<vuint, vi32<float>>));
	VERIFY((is_substitution_failure<vuint, vi32<double>>));
	VERIFY((is_substitution_failure<vi32<uint>, schar>));
	VERIFY((is_substitution_failure<vi32<uint>, short>));
	VERIFY((is_substitution_failure<vi32<uint>, vint>));
	VERIFY((is_substitution_failure<vi32<uint>, vuint>));
	VERIFY((is_substitution_failure<vi32<uint>, long>));
	VERIFY((is_substitution_failure<vi32<uint>, vi32<schar>>));
	VERIFY((is_substitution_failure<vi32<uint>, vi32<short>>));
	VERIFY((is_substitution_failure<vi32<uint>, vi32<int>>));

	binary_op_return_type<vi32<ulong>, vi32<uint>>();
	if constexpr (sizeof(long) == sizeof(llong))
	  {
	    VERIFY((is_substitution_failure<vuint, ulong>));
	    VERIFY((is_substitution_failure<vi32<uint>, ulong>));
	    binary_op_return_type<vi32<long>, vi32<uint>>();
	  }
	else
	  {
	    binary_op_return_type<vuint, ulong>();
	    binary_op_return_type<vi32<uint>, ulong>();
	    VERIFY((is_substitution_failure<vi32<uint>, vi32<long>>));
	  }
      }
    else if constexpr (std::is_same_v<V, vshort>)
      {
	binary_op_return_type<vshort, schar, vshort>();
	binary_op_return_type<vshort, uchar, vshort>();
	binary_op_return_type<vi16<short>, schar, vi16<short>>();
	binary_op_return_type<vi16<short>, uchar, vi16<short>>();
	binary_op_return_type<vi16<short>, short, vi16<short>>();
	binary_op_return_type<vi16<short>, int, vi16<short>>();
	binary_op_return_type<vi16<short>, vi16<schar>>();
	binary_op_return_type<vi16<short>, vi16<uchar>>();
	binary_op_return_type<vi16<short>, vi16<short>>();

	binary_op_return_type<vi16<int>, vi16<short>>();
	binary_op_return_type<vi16<long>, vi16<short>>();
	binary_op_return_type<vi16<llong>, vi16<short>>();
	binary_op_return_type<vi16<float>, vi16<short>>();
	binary_op_return_type<vi16<double>, vi16<short>>();

	VERIFY((is_substitution_failure<vi16<short>, double>));
	VERIFY((is_substitution_failure<vi16<short>, llong>));
	VERIFY((is_substitution_failure<vshort, vushort>));
	VERIFY((is_substitution_failure<vshort, ushort>));
	VERIFY((is_substitution_failure<vshort, uint>));
	VERIFY((is_substitution_failure<vshort, long>));
	VERIFY((is_substitution_failure<vshort, ulong>));
	VERIFY((is_substitution_failure<vshort, llong>));
	VERIFY((is_substitution_failure<vshort, ullong>));
	VERIFY((is_substitution_failure<vshort, float>));
	VERIFY((is_substitution_failure<vshort, double>));
	VERIFY((is_substitution_failure<vshort, vi16<schar>>));
	VERIFY((is_substitution_failure<vshort, vi16<uchar>>));
	VERIFY((is_substitution_failure<vshort, vi16<short>>));
	VERIFY((is_substitution_failure<vshort, vi16<ushort>>));
	VERIFY((is_substitution_failure<vshort, vi16<int>>));
	VERIFY((is_substitution_failure<vshort, vi16<uint>>));
	VERIFY((is_substitution_failure<vshort, vi16<long>>));
	VERIFY((is_substitution_failure<vshort, vi16<ulong>>));
	VERIFY((is_substitution_failure<vshort, vi16<llong>>));
	VERIFY((is_substitution_failure<vshort, vi16<ullong>>));
	VERIFY((is_substitution_failure<vshort, vi16<float>>));
	VERIFY((is_substitution_failure<vshort, vi16<double>>));
	VERIFY((is_substitution_failure<vi16<short>, vshort>));
	VERIFY((is_substitution_failure<vi16<short>, vushort>));
	VERIFY((is_substitution_failure<vi16<short>, ushort>));
	VERIFY((is_substitution_failure<vi16<short>, uint>));
	VERIFY((is_substitution_failure<vi16<short>, long>));
	VERIFY((is_substitution_failure<vi16<short>, ulong>));
	VERIFY((is_substitution_failure<vi16<short>, ullong>));
	VERIFY((is_substitution_failure<vi16<short>, float>));
	VERIFY((is_substitution_failure<vi16<short>, vi16<ushort>>));
	VERIFY((is_substitution_failure<vi16<short>, vi16<uint>>));
	VERIFY((is_substitution_failure<vi16<short>, vi16<ulong>>));
	VERIFY((is_substitution_failure<vi16<short>, vi16<ullong>>));
      }
    else if constexpr (std::is_same_v<V, vushort>)
      {
	binary_op_return_type<vushort, uchar, vushort>();
	binary_op_return_type<vushort, uint, vushort>();
	binary_op_return_type<vi16<ushort>, uchar, vi16<ushort>>();
	binary_op_return_type<vi16<ushort>, ushort, vi16<ushort>>();
	binary_op_return_type<vi16<ushort>, int, vi16<ushort>>();
	binary_op_return_type<vi16<ushort>, uint, vi16<ushort>>();
	binary_op_return_type<vi16<ushort>, vi16<uchar>>();
	binary_op_return_type<vi16<ushort>, vi16<ushort>>();

	binary_op_return_type<vi16<int>, vi16<ushort>>();
	binary_op_return_type<vi16<long>, vi16<ushort>>();
	binary_op_return_type<vi16<llong>, vi16<ushort>>();
	binary_op_return_type<vi16<uint>, vi16<ushort>>();
	binary_op_return_type<vi16<ulong>, vi16<ushort>>();
	binary_op_return_type<vi16<ullong>, vi16<ushort>>();
	binary_op_return_type<vi16<float>, vi16<ushort>>();
	binary_op_return_type<vi16<double>, vi16<ushort>>();

	VERIFY((is_substitution_failure<vi16<ushort>, llong>));
	VERIFY((is_substitution_failure<vi16<ushort>, ullong>));
	VERIFY((is_substitution_failure<vi16<ushort>, double>));
	VERIFY((is_substitution_failure<vushort, schar>));
	VERIFY((is_substitution_failure<vushort, short>));
	VERIFY((is_substitution_failure<vushort, vshort>));
	VERIFY((is_substitution_failure<vushort, long>));
	VERIFY((is_substitution_failure<vushort, ulong>));
	VERIFY((is_substitution_failure<vushort, llong>));
	VERIFY((is_substitution_failure<vushort, ullong>));
	VERIFY((is_substitution_failure<vushort, float>));
	VERIFY((is_substitution_failure<vushort, double>));
	VERIFY((is_substitution_failure<vushort, vi16<schar>>));
	VERIFY((is_substitution_failure<vushort, vi16<uchar>>));
	VERIFY((is_substitution_failure<vushort, vi16<short>>));
	VERIFY((is_substitution_failure<vushort, vi16<ushort>>));
	VERIFY((is_substitution_failure<vushort, vi16<int>>));
	VERIFY((is_substitution_failure<vushort, vi16<uint>>));
	VERIFY((is_substitution_failure<vushort, vi16<long>>));
	VERIFY((is_substitution_failure<vushort, vi16<ulong>>));
	VERIFY((is_substitution_failure<vushort, vi16<llong>>));
	VERIFY((is_substitution_failure<vushort, vi16<ullong>>));
	VERIFY((is_substitution_failure<vushort, vi16<float>>));
	VERIFY((is_substitution_failure<vushort, vi16<double>>));
	VERIFY((is_substitution_failure<vi16<ushort>, schar>));
	VERIFY((is_substitution_failure<vi16<ushort>, short>));
	VERIFY((is_substitution_failure<vi16<ushort>, vshort>));
	VERIFY((is_substitution_failure<vi16<ushort>, vushort>));
	VERIFY((is_substitution_failure<vi16<ushort>, long>));
	VERIFY((is_substitution_failure<vi16<ushort>, ulong>));
	VERIFY((is_substitution_failure<vi16<ushort>, float>));
	VERIFY((is_substitution_failure<vi16<ushort>, vi16<schar>>));
	VERIFY((is_substitution_failure<vi16<ushort>, vi16<short>>));
      }
    else if constexpr (std::is_same_v<V, vchar>)
      {
	binary_op_return_type<vi8<char>, char, vi8<char>>();
	binary_op_return_type<vi8<char>, int, vi8<char>>();
	binary_op_return_type<vi8<char>, vi8<char>, vi8<char>>();

	if constexpr (vi8<schar>::size() <= simd_abi::max_fixed_size<short>)
	  {
	    VERIFY(!(is_substitution_failure<vi8<char>, vi8<short>>));
	    VERIFY(!(is_substitution_failure<vi8<char>, vi8<int>>));
	    VERIFY(!(is_substitution_failure<vi8<char>, vi8<long>>));
	    VERIFY(!(is_substitution_failure<vi8<char>, vi8<llong>>));
	    COMPARE((is_substitution_failure<vi8<char>, vi8<ushort>>),
		    std::is_signed_v<char>);
	    COMPARE((is_substitution_failure<vi8<char>, vi8<uint>>),
		    std::is_signed_v<char>);
	    COMPARE((is_substitution_failure<vi8<char>, vi8<ulong>>),
		    std::is_signed_v<char>);
	    COMPARE((is_substitution_failure<vi8<char>, vi8<ullong>>),
		    std::is_signed_v<char>);
	    if constexpr (std::is_signed_v<char>)
	      {
		binary_op_return_type<vi8<short>, vi8<char>>();
		binary_op_return_type<vi8<int>, vi8<char>>();
		binary_op_return_type<vi8<long>, vi8<char>>();
		binary_op_return_type<vi8<llong>, vi8<char>>();
	      }
	    else
	      {
		binary_op_return_type<vi8<ushort>, vi8<char>>();
		binary_op_return_type<vi8<uint>, vi8<char>>();
		binary_op_return_type<vi8<ulong>, vi8<char>>();
		binary_op_return_type<vi8<ullong>, vi8<char>>();
	      }
	    binary_op_return_type<vi8<float>, vi8<char>>();
	    binary_op_return_type<vi8<double>, vi8<char>>();
	  }

	VERIFY((is_substitution_failure<vi8<char>, llong>));
	VERIFY((is_substitution_failure<vi8<char>, double>));
	VERIFY((is_substitution_failure<vchar, vxchar>));
	VERIFY((is_substitution_failure<vchar, xchar>));
	VERIFY((is_substitution_failure<vchar, short>));
	VERIFY((is_substitution_failure<vchar, ushort>));
	COMPARE((is_substitution_failure<vchar, uint>), std::is_signed_v<char>);
	VERIFY((is_substitution_failure<vchar, long>));
	VERIFY((is_substitution_failure<vchar, ulong>));
	VERIFY((is_substitution_failure<vchar, llong>));
	VERIFY((is_substitution_failure<vchar, ullong>));
	VERIFY((is_substitution_failure<vchar, float>));
	VERIFY((is_substitution_failure<vchar, double>));
	VERIFY((is_substitution_failure<vchar, vi8<char>>));
	VERIFY((is_substitution_failure<vchar, vi8<uchar>>));
	VERIFY((is_substitution_failure<vchar, vi8<schar>>));
	VERIFY((is_substitution_failure<vchar, vi8<short>>));
	VERIFY((is_substitution_failure<vchar, vi8<ushort>>));
	VERIFY((is_substitution_failure<vchar, vi8<int>>));
	VERIFY((is_substitution_failure<vchar, vi8<uint>>));
	VERIFY((is_substitution_failure<vchar, vi8<long>>));
	VERIFY((is_substitution_failure<vchar, vi8<ulong>>));
	VERIFY((is_substitution_failure<vchar, vi8<llong>>));
	VERIFY((is_substitution_failure<vchar, vi8<ullong>>));
	VERIFY((is_substitution_failure<vchar, vi8<float>>));
	VERIFY((is_substitution_failure<vchar, vi8<double>>));
	VERIFY((is_substitution_failure<vi8<char>, vchar>));
	VERIFY((is_substitution_failure<vi8<char>, vuchar>));
	VERIFY((is_substitution_failure<vi8<char>, vschar>));
	VERIFY((is_substitution_failure<vi8<char>, xchar>));
	VERIFY((is_substitution_failure<vi8<char>, short>));
	VERIFY((is_substitution_failure<vi8<char>, ushort>));
	COMPARE((is_substitution_failure<vi8<char>, uint>),
		std::is_signed_v<char>);
	VERIFY((is_substitution_failure<vi8<char>, long>));
	VERIFY((is_substitution_failure<vi8<char>, ulong>));
	VERIFY((is_substitution_failure<vi8<char>, ullong>));
	VERIFY((is_substitution_failure<vi8<char>, float>));

	// conversion between any char types must fail because the dst type's
	// integer conversion rank isn't greater (as required by 9.6.4p4.3)
	VERIFY((is_substitution_failure<vi8<char>, vi8<schar>>));
	VERIFY((is_substitution_failure<vi8<char>, vi8<uchar>>));
      }
    else if constexpr (std::is_same_v<V, vschar>)
      {
	binary_op_return_type<vi8<schar>, schar, vi8<schar>>();
	binary_op_return_type<vi8<schar>, int, vi8<schar>>();
	binary_op_return_type<vi8<schar>, vi8<schar>, vi8<schar>>();

	if constexpr (vi8<schar>::size() <= simd_abi::max_fixed_size<short>)
	  {
	    binary_op_return_type<vi8<short>, vi8<schar>>();
	    binary_op_return_type<vi8<int>, vi8<schar>>();
	    binary_op_return_type<vi8<long>, vi8<schar>>();
	    binary_op_return_type<vi8<llong>, vi8<schar>>();
	    binary_op_return_type<vi8<float>, vi8<schar>>();
	    binary_op_return_type<vi8<double>, vi8<schar>>();
	  }

	VERIFY((is_substitution_failure<vi8<schar>, llong>));
	VERIFY((is_substitution_failure<vi8<schar>, double>));
	VERIFY((is_substitution_failure<vschar, vuchar>));
	VERIFY((is_substitution_failure<vschar, uchar>));
	VERIFY((is_substitution_failure<vschar, short>));
	VERIFY((is_substitution_failure<vschar, ushort>));
	VERIFY((is_substitution_failure<vschar, uint>));
	VERIFY((is_substitution_failure<vschar, long>));
	VERIFY((is_substitution_failure<vschar, ulong>));
	VERIFY((is_substitution_failure<vschar, llong>));
	VERIFY((is_substitution_failure<vschar, ullong>));
	VERIFY((is_substitution_failure<vschar, float>));
	VERIFY((is_substitution_failure<vschar, double>));
	VERIFY((is_substitution_failure<vschar, vi8<schar>>));
	VERIFY((is_substitution_failure<vschar, vi8<uchar>>));
	VERIFY((is_substitution_failure<vschar, vi8<short>>));
	VERIFY((is_substitution_failure<vschar, vi8<ushort>>));
	VERIFY((is_substitution_failure<vschar, vi8<int>>));
	VERIFY((is_substitution_failure<vschar, vi8<uint>>));
	VERIFY((is_substitution_failure<vschar, vi8<long>>));
	VERIFY((is_substitution_failure<vschar, vi8<ulong>>));
	VERIFY((is_substitution_failure<vschar, vi8<llong>>));
	VERIFY((is_substitution_failure<vschar, vi8<ullong>>));
	VERIFY((is_substitution_failure<vschar, vi8<float>>));
	VERIFY((is_substitution_failure<vschar, vi8<double>>));
	VERIFY((is_substitution_failure<vi8<schar>, vschar>));
	VERIFY((is_substitution_failure<vi8<schar>, vuchar>));
	VERIFY((is_substitution_failure<vi8<schar>, uchar>));
	VERIFY((is_substitution_failure<vi8<schar>, short>));
	VERIFY((is_substitution_failure<vi8<schar>, ushort>));
	VERIFY((is_substitution_failure<vi8<schar>, uint>));
	VERIFY((is_substitution_failure<vi8<schar>, long>));
	VERIFY((is_substitution_failure<vi8<schar>, ulong>));
	VERIFY((is_substitution_failure<vi8<schar>, ullong>));
	VERIFY((is_substitution_failure<vi8<schar>, float>));
	VERIFY((is_substitution_failure<vi8<schar>, vi8<uchar>>));
	VERIFY((is_substitution_failure<vi8<schar>, vi8<ushort>>));
	VERIFY((is_substitution_failure<vi8<schar>, vi8<uint>>));
	VERIFY((is_substitution_failure<vi8<schar>, vi8<ulong>>));
	VERIFY((is_substitution_failure<vi8<schar>, vi8<ullong>>));
      }
    else if constexpr (std::is_same_v<V, vuchar>)
      {
	VERIFY((is_substitution_failure<vi8<uchar>, llong>));

	binary_op_return_type<vuchar, uint, vuchar>();
	binary_op_return_type<vi8<uchar>, uchar, vi8<uchar>>();
	binary_op_return_type<vi8<uchar>, int, vi8<uchar>>();
	binary_op_return_type<vi8<uchar>, uint, vi8<uchar>>();
	binary_op_return_type<vi8<uchar>, vi8<uchar>, vi8<uchar>>();

	if constexpr (vi8<schar>::size() <= simd_abi::max_fixed_size<short>)
	  {
	    binary_op_return_type<vi8<short>, vi8<uchar>>();
	    binary_op_return_type<vi8<ushort>, vi8<uchar>>();
	    binary_op_return_type<vi8<int>, vi8<uchar>>();
	    binary_op_return_type<vi8<uint>, vi8<uchar>>();
	    binary_op_return_type<vi8<long>, vi8<uchar>>();
	    binary_op_return_type<vi8<ulong>, vi8<uchar>>();
	    binary_op_return_type<vi8<llong>, vi8<uchar>>();
	    binary_op_return_type<vi8<ullong>, vi8<uchar>>();
	    binary_op_return_type<vi8<float>, vi8<uchar>>();
	    binary_op_return_type<vi8<double>, vi8<uchar>>();
	  }

	VERIFY((is_substitution_failure<vi8<uchar>, ullong>));
	VERIFY((is_substitution_failure<vi8<uchar>, double>));
	VERIFY((is_substitution_failure<vuchar, schar>));
	VERIFY((is_substitution_failure<vuchar, vschar>));
	VERIFY((is_substitution_failure<vuchar, short>));
	VERIFY((is_substitution_failure<vuchar, ushort>));
	VERIFY((is_substitution_failure<vuchar, long>));
	VERIFY((is_substitution_failure<vuchar, ulong>));
	VERIFY((is_substitution_failure<vuchar, llong>));
	VERIFY((is_substitution_failure<vuchar, ullong>));
	VERIFY((is_substitution_failure<vuchar, float>));
	VERIFY((is_substitution_failure<vuchar, double>));
	VERIFY((is_substitution_failure<vuchar, vi8<schar>>));
	VERIFY((is_substitution_failure<vuchar, vi8<uchar>>));
	VERIFY((is_substitution_failure<vuchar, vi8<short>>));
	VERIFY((is_substitution_failure<vuchar, vi8<ushort>>));
	VERIFY((is_substitution_failure<vuchar, vi8<int>>));
	VERIFY((is_substitution_failure<vuchar, vi8<uint>>));
	VERIFY((is_substitution_failure<vuchar, vi8<long>>));
	VERIFY((is_substitution_failure<vuchar, vi8<ulong>>));
	VERIFY((is_substitution_failure<vuchar, vi8<llong>>));
	VERIFY((is_substitution_failure<vuchar, vi8<ullong>>));
	VERIFY((is_substitution_failure<vuchar, vi8<float>>));
	VERIFY((is_substitution_failure<vuchar, vi8<double>>));
	VERIFY((is_substitution_failure<vi8<uchar>, schar>));
	VERIFY((is_substitution_failure<vi8<uchar>, vschar>));
	VERIFY((is_substitution_failure<vi8<uchar>, vuchar>));
	VERIFY((is_substitution_failure<vi8<uchar>, short>));
	VERIFY((is_substitution_failure<vi8<uchar>, ushort>));
	VERIFY((is_substitution_failure<vi8<uchar>, long>));
	VERIFY((is_substitution_failure<vi8<uchar>, ulong>));
	VERIFY((is_substitution_failure<vi8<uchar>, float>));
	VERIFY((is_substitution_failure<vi8<uchar>, vi8<schar>>));
      }
  }
