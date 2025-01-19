// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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
#include "bits/main.h"

using std::experimental::simd_cast;

template <typename V, bool ConstProp, typename F>
  auto
  gen(const F& fun)
  {
    if constexpr (ConstProp)
      return V(fun);
    else
      return make_value_unknown(V(fun));
  }

template <typename V, bool ConstProp>
  void
  split_concat()
  {
    using T = typename V::value_type;
    if constexpr (V::size() * 3
		    <= std::experimental::simd_abi::max_fixed_size<T>)
      {
	V a(0), b(1), c(2);
	auto x = concat(a, b, c);
	COMPARE(x.size(), a.size() * 3);
	std::size_t i = 0;
	for (; i < a.size(); ++i)
	  {
	    COMPARE(x[i], T(0));
	  }
	for (; i < 2 * a.size(); ++i)
	  {
	    COMPARE(x[i], T(1));
	  }
	for (; i < 3 * a.size(); ++i)
	  {
	    COMPARE(x[i], T(2));
	  }
      }

    if constexpr (V::size() >= 4)
      {
	const V a = gen<V, ConstProp>([](auto i) -> T { return i; });
	constexpr auto N0 = V::size() / 4u;
	constexpr auto N1 = V::size() - 2 * N0;
	using V0 = std::experimental::simd<
		     T, std::experimental::simd_abi::deduce_t<T, N0>>;
	using V1 = std::experimental::simd<
		     T, std::experimental::simd_abi::deduce_t<T, N1>>;
	{
	  auto x = std::experimental::split<N0, N0, N1>(a);
	  COMPARE(std::tuple_size<decltype(x)>::value, 3u);
	  COMPARE(std::get<0>(x), V0([](auto i) -> T { return i; }));
	  COMPARE(std::get<1>(x), V0([](auto i) -> T { return i + N0; }));
	  COMPARE(std::get<2>(x), V1([](auto i) -> T { return i + 2 * N0; }));
	  auto b = concat(std::get<1>(x), std::get<2>(x), std::get<0>(x));
	  // a and b may have different types if a was fixed_size<N> such that
	  // another ABI tag exists with equal N, then b will have the
	  // non-fixed-size ABI tag.
	  COMPARE(a.size(), b.size());
	  COMPARE(
	    b, decltype(b)([](auto i) -> T { return (N0 + i) % V::size(); }));
	}
	{
	  auto x = std::experimental::split<N0, N1, N0>(a);
	  COMPARE(std::tuple_size<decltype(x)>::value, 3u);
	  COMPARE(std::get<0>(x), V0([](auto i) -> T { return i; }));
	  COMPARE(std::get<1>(x), V1([](auto i) -> T { return i + N0; }));
	  COMPARE(std::get<2>(x), V0([](auto i) -> T { return i + N0 + N1; }));
	  auto b = concat(std::get<1>(x), std::get<2>(x), std::get<0>(x));
	  // a and b may have different types if a was fixed_size<N> such that
	  // another ABI tag exists with equal N, then b will have the
	  // non-fixed-size ABI tag.
	  COMPARE(a.size(), b.size());
	  COMPARE(
	    b, decltype(b)([](auto i) -> T { return (N0 + i) % V::size(); }));
	}
	{
	  auto x = std::experimental::split<N1, N0, N0>(a);
	  COMPARE(std::tuple_size<decltype(x)>::value, 3u);
	  COMPARE(std::get<0>(x), V1([](auto i) -> T { return i; }));
	  COMPARE(std::get<1>(x), V0([](auto i) -> T { return i + N1; }));
	  COMPARE(std::get<2>(x), V0([](auto i) -> T { return i + N0 + N1; }));
	  auto b = concat(std::get<1>(x), std::get<2>(x), std::get<0>(x));
	  // a and b may have different types if a was fixed_size<N> such that
	  // another ABI tag exists with equal N, then b will have the
	  // non-fixed-size ABI tag.
	  COMPARE(a.size(), b.size());
	  COMPARE(
	    b, decltype(b)([](auto i) -> T { return (N1 + i) % V::size(); }));
	}
      }

    if constexpr (V::size() % 3 == 0)
      {
	const V a = gen<V, ConstProp>([](auto i) -> T { return i; });
	constexpr auto N0 = V::size() / 3;
	using V0 = std::experimental::simd<
		     T, std::experimental::simd_abi::deduce_t<T, N0>>;
	using V1 = std::experimental::simd<
	  T, std::experimental::simd_abi::deduce_t<T, 2 * N0>>;
	{
	  auto [x, y, z] = std::experimental::split<N0, N0, N0>(a);
	  COMPARE(x, V0([](auto i) -> T { return i; }));
	  COMPARE(y, V0([](auto i) -> T { return i + N0; }));
	  COMPARE(z, V0([](auto i) -> T { return i + N0 * 2; }));
	  auto b = concat(x, y, z);
	  COMPARE(a.size(), b.size());
	  COMPARE(b, simd_cast<decltype(b)>(a));
	  COMPARE(simd_cast<V>(b), a);
	}
	{
	  auto [x, y] = std::experimental::split<N0, 2 * N0>(a);
	  COMPARE(x, V0([](auto i) -> T { return i; }));
	  COMPARE(y, V1([](auto i) -> T { return i + N0; }));
	  auto b = concat(x, y);
	  COMPARE(a.size(), b.size());
	  COMPARE(b, simd_cast<decltype(b)>(a));
	  COMPARE(simd_cast<V>(b), a);
	}
	{
	  auto [x, y] = std::experimental::split<2 * N0, N0>(a);
	  COMPARE(x, V1([](auto i) -> T { return i; }));
	  COMPARE(y, V0([](auto i) -> T { return i + 2 * N0; }));
	  auto b = concat(x, y);
	  COMPARE(a.size(), b.size());
	  COMPARE(b, simd_cast<decltype(b)>(a));
	  COMPARE(simd_cast<V>(b), a);
	}
      }

    if constexpr ((V::size() & 1) == 0)
      {
	using std::experimental::simd;
	using std::experimental::simd_abi::deduce_t;
	using V0 = simd<T, deduce_t<T, V::size()>>;
	using V2 = simd<T, deduce_t<T, 2>>;
	using V3 = simd<T, deduce_t<T, V::size() / 2>>;

	const V a = gen<V, ConstProp>([](auto i) -> T { return i; });

	std::array<V2, V::size() / 2> v2s = std::experimental::split<V2>(a);
	int offset = 0;
	for (V2 test : v2s)
	  {
	    COMPARE(test, V2([&](auto i) -> T { return i + offset; }));
	    offset += 2;
	  }
	COMPARE(concat(v2s), simd_cast<V0>(a));

	std::array<V3, 2> v3s = std::experimental::split<V3>(a);
	COMPARE(v3s[0], V3([](auto i) -> T { return i; }));
	COMPARE(v3s[1], V3([](auto i) -> T { return i + V3::size(); }));
	COMPARE(concat(v3s), simd_cast<V0>(a));
      }
  }

template <typename V>
  void
  test()
  {
    split_concat<V, true>();
    split_concat<V, false>();
  }
