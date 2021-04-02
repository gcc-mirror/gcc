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
#include "bits/conversions.h"

using std::experimental::simd_cast;
using std::experimental::static_simd_cast;

template <class T, size_t N>
  struct gen_cast
  {
    std::array<T, N> data;

    template <class V>
      gen_cast(const V& v)
      {
	for (size_t i = 0; i < V::size(); ++i)
	  {
	    data[i] = static_cast<T>(v[i]);
	  }
      }

    template <class I>
      constexpr T
      operator()(I)
      { return data[I::value]; }
  };

template <class V, class To>
  struct gen_seq_t
  {
    using From = typename V::value_type;
    const size_t N = cvt_input_data<From, To>.size();
    size_t offset = 0;

    constexpr void
    operator++()
    { offset += V::size(); }

    explicit constexpr operator bool() const
    { return offset < N; }

    template <class I>
      constexpr From
      operator()(I) const
      {
	size_t i = I::value + offset;
	return i < N ? cvt_input_data<From, To>[i] : From(i);
      }
  };

template <class To>
  struct foo
  {
    template <class T>
      auto
      operator()(const T& v) -> decltype(simd_cast<To>(v));
  };

template <typename V, typename To>
  void
  casts()
  {
    using From = typename V::value_type;
    constexpr auto N = V::size();
    if constexpr (N <= std::experimental::simd_abi::max_fixed_size<To>)
      {
	using W = std::experimental::fixed_size_simd<To, N>;

	if constexpr (std::is_integral_v<From>)
	  {
	    using A = typename V::abi_type;
	    using TU = std::make_unsigned_t<From>;
	    using TS = std::make_signed_t<From>;
	    COMPARE(typeid(static_simd_cast<TU>(V())),
		    typeid(std::experimental::simd<TU, A>));
	    COMPARE(typeid(static_simd_cast<TS>(V())),
		    typeid(std::experimental::simd<TS, A>));
	  }

	using is_simd_cast_allowed
	  = decltype(vir::test::sfinae_is_callable_t<const V&>(foo<To>()));

	COMPARE(is_simd_cast_allowed::value,
		std::__digits<From>::value <= std::__digits<To>::value
		  && std::__finite_max<From>::value
		  <= std::__finite_max<To>::value
		  && !(std::is_signed<From>::value
		       && std::is_unsigned<To>::value));

	if constexpr (is_simd_cast_allowed::value)
	  {
	    for (gen_seq_t<V, To> gen_seq; gen_seq; ++gen_seq)
	      {
		const V seq(gen_seq);
		COMPARE(simd_cast<V>(seq), seq);
		COMPARE(simd_cast<W>(seq), W(gen_cast<To, N>(seq)))
		  << "seq = " << seq;
		auto test = simd_cast<To>(seq);
		// decltype(test) is not W if
		// a) V::abi_type is not fixed_size and
		// b.1) V::value_type and To are integral and of equal rank or
		// b.2) V::value_type and To are equal
		COMPARE(test, decltype(test)(gen_cast<To, N>(seq)));
		if (std::is_same<To, From>::value)
		  {
		    COMPARE(typeid(decltype(test)), typeid(V));
		  }
	      }
	  }

	for (gen_seq_t<V, To> gen_seq; gen_seq; ++gen_seq)
	  {
	    const V seq(gen_seq);
	    COMPARE(static_simd_cast<V>(seq), seq);
	    COMPARE(static_simd_cast<W>(seq), W(gen_cast<To, N>(seq))) << '\n'
	      << seq;
	    auto test = static_simd_cast<To>(seq);
	    // decltype(test) is not W if
	    // a) V::abi_type is not fixed_size and
	    // b.1) V::value_type and To are integral and of equal rank or
	    // b.2) V::value_type and To are equal
	    COMPARE(test, decltype(test)(gen_cast<To, N>(seq)));
	    if (std::is_same<To, From>::value)
	      {
		COMPARE(typeid(decltype(test)), typeid(V));
	      }
	  }
      }
  }

template <typename V>
  void
  test()
  {
    casts<V, long double>();
    casts<V, double>();
    casts<V, float>();
    casts<V, long long>();
    casts<V, unsigned long long>();
    casts<V, unsigned long>();
    casts<V, long>();
    casts<V, int>();
    casts<V, unsigned int>();
    casts<V, short>();
    casts<V, unsigned short>();
    casts<V, char>();
    casts<V, signed char>();
    casts<V, unsigned char>();
    casts<V, char32_t>();
    casts<V, char16_t>();
    casts<V, wchar_t>();
  }
