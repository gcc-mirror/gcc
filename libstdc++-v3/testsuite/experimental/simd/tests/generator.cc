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
  struct call_generator
  {
    template <class F>
      auto
      operator()(const F& f) -> decltype(V(f));
  };

using schar = signed char;
using uchar = unsigned char;
using ullong = unsigned long long;

template <typename V>
  void
  test()
  {
    using T = typename V::value_type;
    V x([](int) { return T(1); });
    COMPARE(x, V(1));
    // unconditionally returns int from generator lambda
    x = V([](int) { return 1; });
    COMPARE(x, V(1));
    x = V([](auto i) { return T(i); });
    COMPARE(x, V([](T i) { return i; }));

    VERIFY((// that int always works
	sfinae_is_callable<int (&)(int)>(call_generator<V>())));
    COMPARE(sfinae_is_callable<schar (&)(int)>(call_generator<V>()),
	    std::is_signed<T>::value);
    COMPARE(sfinae_is_callable<uchar (&)(int)>(call_generator<V>()),
	    !(std::is_signed_v<T> && sizeof(T) <= sizeof(uchar)));
    COMPARE(sfinae_is_callable<float (&)(int)>(call_generator<V>()),
	    (std::is_floating_point<T>::value));

    COMPARE(sfinae_is_callable<ullong (&)(int)>(call_generator<V>()),
      std::__finite_max_v<T> >= std::__finite_max_v<ullong>
      && std::__digits_v<T> >= std::__digits_v<ullong>);
  }
