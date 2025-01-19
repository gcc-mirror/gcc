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

template <class V>
  struct Convertible
  {
    operator V() const { return V(4); }
  };

template <class M, class T>
  constexpr bool
  where_is_ill_formed_impl(M, const T&, float)
  {
    return true;
  }

template <class M, class T>
  constexpr auto
  where_is_ill_formed_impl(M m, const T& v, int)
    -> std::conditional_t<true, bool, decltype(std::experimental::where(m, v))>
  {
    return false;
  }

template <class M, class T>
  constexpr bool
  where_is_ill_formed(M m, const T& v)
  {
    return where_is_ill_formed_impl(m, v, int());
  }

template <typename T>
  void
  where_fundamental()
  {
    using std::experimental::where;
    T x = T();
    where(true, x) = x + 1;
    COMPARE(x, T(1));
    where(false, x) = x - 1;
    COMPARE(x, T(1));
    where(true, x) += T(1);
    COMPARE(x, T(2));
  }

template <typename V>
  void
  test()
  {
    using M = typename V::mask_type;
    using T = typename V::value_type;
    where_fundamental<T>();
    VERIFY(!(sfinae_is_callable<V>(
	       [](auto x) -> decltype(where(true, x))* { return nullptr; })));

    const V indexes([](int i) { return i + 1; });
    const M alternating_mask = make_mask<M>({true, false});
    V x = 0;
    where(alternating_mask, x) = indexes;
    COMPARE(alternating_mask, x == indexes);

    where(!alternating_mask, x) = T(2);
    COMPARE(!alternating_mask, x == T(2)) << x;

    where(!alternating_mask, x) = Convertible<V>();
    COMPARE(!alternating_mask, x == T(4));

    x = 0;
    COMPARE(x, T(0));
    where(alternating_mask, x) += indexes;
    COMPARE(alternating_mask, x == indexes);

    x = 10;
    COMPARE(x, T(10));
    where(!alternating_mask, x) += T(1);
    COMPARE(!alternating_mask, x == T(11));
    where(alternating_mask, x) -= Convertible<V>();
    COMPARE(alternating_mask, x == T(6));
    constexpr bool fast_math =
#ifdef __FAST_MATH__
      true;
#else
      false;
#endif
    if constexpr (fast_math && std::is_floating_point_v<T>)
      where(alternating_mask, x) *= T(.5);
    else
    where(alternating_mask, x) /= T(2);
    COMPARE(alternating_mask, x == T(3)) << "\nx = " << x;
    where(alternating_mask, x) *= T(3);
    COMPARE(alternating_mask, x == T(9));
    COMPARE(!alternating_mask, x == T(11));

    x = 10;
    where(alternating_mask, x)++;
    COMPARE(alternating_mask, x == T(11));
    ++where(alternating_mask, x);
    COMPARE(alternating_mask, x == T(12));
    where(alternating_mask, x)--;
    COMPARE(alternating_mask, x == T(11));
    --where(alternating_mask, x);
    --where(alternating_mask, x);
    COMPARE(alternating_mask, x == T(9));
    COMPARE(alternating_mask, -where(alternating_mask, x) == T(-T(9)));

    const auto y = x;
    VERIFY(where_is_ill_formed(true, y));
    VERIFY(where_is_ill_formed(true, x));
    VERIFY(where_is_ill_formed(true, V(x)));

    M test = alternating_mask;
    where(alternating_mask, test) = M(true);
    COMPARE(test, alternating_mask);
    where(alternating_mask, test) = M(false);
    COMPARE(test, M(false));
    where(alternating_mask, test) = M(true);
    COMPARE(test, alternating_mask);
  }
