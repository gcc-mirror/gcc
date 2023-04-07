// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

template <typename V>
  void
  test()
  {
    using M = typename V::mask_type;
    using T = typename V::value_type;
    constexpr auto min = std::__finite_min_v<T>;
    constexpr auto norm_min = std::__norm_min_v<T>;
    constexpr auto max = std::__finite_max_v<T>;
    { // compares
      COMPARE(V(0) == make_vec<V>({0, 1}, 0), make_mask<M>({1, 0}));
      COMPARE(V(0) == make_vec<V>({0, 1, 2}, 0), make_mask<M>({1, 0, 0}));
      COMPARE(V(1) == make_vec<V>({0, 1, 2}, 0), make_mask<M>({0, 1, 0}));
      COMPARE(V(2) == make_vec<V>({0, 1, 2}, 0), make_mask<M>({0, 0, 1}));
      COMPARE(V(0) < make_vec<V>({0, 1, 2}, 0), make_mask<M>({0, 1, 1}));

      constexpr T half = genHalfBits<T>();
      for (T lo_ : {min, T(min + 1), T(-1), T(0), norm_min, T(1), T(half - 1),
		    half, T(half + 1), T(max - 1)})
	{
	  for (T hi_ : {T(min + 1), T(-1), T(0), norm_min, T(1), T(half - 1),
			half, T(half + 1), T(max - 1), max})
	    {
	      if (hi_ <= lo_)
		continue;

	      for (std::size_t pos = 0; pos < V::size(); ++pos)
		{
		  V lo = lo_;
		  V hi = hi_;
		  lo[pos] = 0; // have a different value in the vector in case
		  hi[pos] = 1; // this affects neighbors
		  COMPARE(hi, hi);
		  VERIFY(all_of(hi != lo)) << "hi: " << hi << ", lo: " << lo;
		  VERIFY(all_of(lo != hi)) << "hi: " << hi << ", lo: " << lo;
		  VERIFY(none_of(hi != hi)) << "hi: " << hi << ", lo: " << lo;
		  VERIFY(none_of(hi == lo)) << "hi: " << hi << ", lo: " << lo;
		  VERIFY(none_of(lo == hi)) << "hi: " << hi << ", lo: " << lo;
		  VERIFY(all_of(lo < hi)) << "hi: " << hi << ", lo: " << lo
					  << ", lo < hi: " << (lo < hi);
		  VERIFY(none_of(hi < lo)) << "hi: " << hi << ", lo: " << lo;
		  VERIFY(none_of(hi <= lo)) << "hi: " << hi << ", lo: " << lo;
		  VERIFY(all_of(hi <= hi)) << "hi: " << hi << ", lo: " << lo;
		  VERIFY(all_of(hi > lo)) << "hi: " << hi << ", lo: " << lo;
		  VERIFY(none_of(lo > hi)) << "hi: " << hi << ", lo: " << lo;
		  VERIFY(all_of(hi >= lo)) << "hi: " << hi << ", lo: " << lo;
		  VERIFY(all_of(hi >= hi)) << "hi: " << hi << ", lo: " << lo;
		}
	    }
	}
    }
    { // subscripting
      V x = max;
      for (std::size_t i = 0; i < V::size(); ++i)
	{
	  COMPARE(x[i], max);
	  x[i] = 0;
	}
      COMPARE(x, V{0});
      for (std::size_t i = 0; i < V::size(); ++i)
	{
	  COMPARE(x[i], T(0));
	  x[i] = max;
	}
      COMPARE(x, V{max});
      COMPARE(typeid(x[0] * x[0]), typeid(T() * T()));
      COMPARE(typeid(x[0] * T()), typeid(T() * T()));
      COMPARE(typeid(T() * x[0]), typeid(T() * T()));
      COMPARE(typeid(x * x[0]), typeid(x));
      COMPARE(typeid(x[0] * x), typeid(x));

      x = V([](auto i) -> T { return i; });
      for (std::size_t i = 0; i < V::size(); ++i)
	{
	  COMPARE(x[i], T(i));
	}
      for (std::size_t i = 0; i + 1 < V::size(); i += 2)
	{
	  using std::swap;
	  swap(x[i], x[i + 1]);
	}
      for (std::size_t i = 0; i + 1 < V::size(); i += 2)
	{
	  COMPARE(x[i], T(i + 1)) << x;
	  COMPARE(x[i + 1], T(i)) << x;
	}
      x = 1;
      V y = 0;
      COMPARE(x[0], T(1));
      x[0] = y[0]; // make sure non-const smart_reference assignment works
      COMPARE(x[0], T(0));
      x = 1;
      x[0] = x[0]; // self-assignment on smart_reference
      COMPARE(x[0], T(1));

      std::experimental::simd<typename V::value_type,
			      std::experimental::simd_abi::scalar>
      z = 2;
      x[0] = z[0];
      COMPARE(x[0], T(2));
      x = 3;
      z[0] = x[0];
      COMPARE(z[0], T(3));

      // TODO: check that only value-preserving conversions happen on subscript
      // assignment
    }
    { // not
      V x = 0;
      COMPARE(!x, M{true});
      V y = 1;
      COMPARE(!y, M{false});
    }

    { // unary minus
      V x = 0;
      COMPARE(-x, V(T(-T(0))));
      V y = 1;
      COMPARE(-y, V(T(-T(1))));
    }

    { // plus
      V x = 0;
      V y = 0;
      COMPARE(x + y, x);
      COMPARE(x = x + T(1), V(1));
      COMPARE(x + x, V(2));
      y = make_vec<V>({1, 2, 3, 4, 5, 6, 7});
      COMPARE(x = x + y, make_vec<V>({2, 3, 4, 5, 6, 7, 8}));
      COMPARE(x = x + -y, V(1));
      COMPARE(x += y, make_vec<V>({2, 3, 4, 5, 6, 7, 8}));
      COMPARE(x, make_vec<V>({2, 3, 4, 5, 6, 7, 8}));
      COMPARE(x += -y, V(1));
      COMPARE(x, V(1));
    }

    { // minus
      V x = 1;
      V y = 0;
      COMPARE(x - y, x);
      COMPARE(x - T(1), y);
      COMPARE(y, x - T(1));
      COMPARE(x - x, y);
      y = make_vec<V>({1, 2, 3, 4, 5, 6, 7});
      COMPARE(x = y - x, make_vec<V>({0, 1, 2, 3, 4, 5, 6}));
      COMPARE(x = y - x, V(1));
      COMPARE(y -= x, make_vec<V>({0, 1, 2, 3, 4, 5, 6}));
      COMPARE(y, make_vec<V>({0, 1, 2, 3, 4, 5, 6}));
      COMPARE(y -= y, V(0));
      COMPARE(y, V(0));
    }

    { // multiplies
      V x = 1;
      V y = 0;
      COMPARE(x * y, y);
      COMPARE(x = x * T(2), V(2));
      COMPARE(x * x, V(4));
      y = make_vec<V>({1, 2, 3, 4, 5, 6, 7});
      COMPARE(x = x * y, make_vec<V>({2, 4, 6, 8, 10, 12, 14}));
      y = 2;
      // don't test norm_min/2*2 in the following. There's no guarantee, in
      // general, that the result isn't flushed to zero (e.g. NEON without
      // subnormals)
      for (T n :
      {T(max - 1), std::is_floating_point_v<T> ? T(norm_min * 3) : min})
	{
	  x = n / 2;
	  COMPARE(x * y, V(n));
	}
      if (std::is_integral<T>::value && std::is_unsigned<T>::value)
	{
	  // test modulo arithmetics
	  T n = max;
	  x = n;
	  for (T m : {T(2), T(7), T(max / 127), max})
	    {
	      y = m;
	      // if T is of lower rank than int, `n * m` will promote to int
	      // before executing the multiplication. In this case an overflow
	      // will be UB (and ubsan will warn about it). The solution is to
	      // cast to uint in that case.
	      using U
		= std::conditional_t<(sizeof(T) < sizeof(int)), unsigned, T>;
	      COMPARE(x * y, V(T(U(n) * U(m))));
	    }
	}
      x = 2;
      COMPARE(x *= make_vec<V>({1, 2, 3}), make_vec<V>({2, 4, 6}));
      COMPARE(x, make_vec<V>({2, 4, 6}));
    }

    // divides
    constexpr bool is_iec559 =
#ifdef __GCC_IEC_559
      __GCC_IEC_559 >= 2;
#elif defined __STDC_IEC_559__
      true;
#else
      false;
#endif
    if constexpr (std::is_floating_point_v<T> && !is_iec559)
      { // avoid testing subnormals and expect minor deltas for non-IEC559 float
	V x = 2;
	ULP_COMPARE(x / x, V(1), 1);
	ULP_COMPARE(T(3) / x, V(T(3) / T(2)), 1);
	ULP_COMPARE(x / T(3), V(T(2) / T(3)), 1);
	V y = make_vec<V>({1, 2, 3, 4, 5, 6, 7});
	ULP_COMPARE(y / x,
		    make_vec<V>(
		      {T(.5), T(1), T(1.5), T(2), T(2.5), T(3), T(3.5)}),
		    1);

	test_values<V>({norm_min * 1024, T(1), T(), T(-1), max / 1024,
			max / 4.1, max, min},
		       [&](V a) {
			 V b = 2;
			 V ref([&](auto i) { return a[i] / 2; });
			 ULP_COMPARE(a / b, ref, 1);
			 where(a == 0, a) = 1;
			 // -freciprocal-math together with flush-to-zero makes
			 // the following range restriction necessary (i.e.
			 // 1/|a| must be >= min). Intel vrcpps and vrcp14ps
			 // need some extra slack (use 1.1 instead of 1).
			 where(abs(a) >= T(1.1) / norm_min, a) = 1;
			 ULP_COMPARE(a / a, V(1), 1) << "\na = " << a;
			 ref = V([&](auto i) { return 2 / a[i]; });
			 ULP_COMPARE(b / a, ref, 1) << "\na = " << a;
			 ULP_COMPARE(b /= a, ref, 1);
			 ULP_COMPARE(b, ref, 1);
		       });
      }
    else
      {
	V x = 2;
	COMPARE(x / x, V(1));
	COMPARE(T(3) / x, V(T(3) / T(2)));
	COMPARE(x / T(3), V(T(2) / T(3)));
	V y = make_vec<V>({1, 2, 3, 4, 5, 6, 7});
	COMPARE(y / x,
		make_vec<V>({T(.5), T(1), T(1.5), T(2), T(2.5), T(3), T(3.5)}));

	y = make_vec<V>({max, norm_min});
	V ref = make_vec<V>({T(max / 2), T(norm_min / 2)});
	COMPARE(y / x, ref);

	y = make_vec<V>({norm_min, max});
	ref = make_vec<V>({T(norm_min / 2), T(max / 2)});
	COMPARE(y / x, ref);

	y = make_vec<V>({max, T(norm_min + 1)});
	COMPARE(y / y, V(1));

	ref = make_vec<V>({T(2 / max), T(2 / (norm_min + 1))});
	COMPARE(x / y, ref);
	COMPARE(x /= y, ref);
	COMPARE(x, ref);
      }

    { // increment & decrement
      const V from0 = make_vec<V>({0, 1, 2, 3}, 4);
      V x = from0;
      COMPARE(x++, from0);
      COMPARE(x, from0 + 1);
      COMPARE(++x, from0 + 2);
      COMPARE(x, from0 + 2);

      COMPARE(x--, from0 + 2);
      COMPARE(x, from0 + 1);
      COMPARE(--x, from0);
      COMPARE(x, from0);
    }
  }
