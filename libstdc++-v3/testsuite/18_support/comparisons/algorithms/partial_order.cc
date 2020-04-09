// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <compare>
#include <limits>
#include <testsuite_hooks.h>

using std::partial_order;
using std::partial_ordering;

void
test01()
{
  int one = 1, two = 2;

  VERIFY( partial_order(one, two) == partial_ordering::less );
  VERIFY( partial_order(one, one) == partial_ordering::equivalent );
  VERIFY( partial_order(two, one) == partial_ordering::greater );
  static_assert( noexcept(partial_order(1, 1)) );
}

constexpr partial_ordering different_cv_quals(int i, const int j)
{
  return partial_order(i, j);
}

void
test02()
{
  int fortytwo = 42, nines = 999, lots = 1000;
  VERIFY( different_cv_quals(fortytwo, nines) == partial_ordering::less );
  VERIFY( different_cv_quals(-nines, -nines) == partial_ordering::equivalent );
  VERIFY( different_cv_quals(-nines, -lots) == partial_ordering::greater );
}

void
test03()
{
  double zero = 0.0;
  VERIFY( partial_order(zero, zero) == partial_ordering::equivalent );
  VERIFY( partial_order(-zero, -zero) == partial_ordering::equivalent );
  VERIFY( partial_order(-zero, zero) == partial_ordering::equivalent );
  VERIFY( partial_order(zero, -zero) == partial_ordering::equivalent );
  static_assert( noexcept(partial_order(zero, 1.0)) );
  static_assert( partial_order(0.0, 1.0) == std::partial_ordering::less );

  double min = std::numeric_limits<double>::lowest();
  double max = std::numeric_limits<double>::max();
  double nan = std::numeric_limits<double>::quiet_NaN();
  double inf = std::numeric_limits<double>::infinity();
  double denorm = std::numeric_limits<double>::denorm_min();
  double smallest = std::numeric_limits<double>::min();
  double epsilon = std::numeric_limits<double>::epsilon();
  VERIFY( partial_order(denorm, smallest) == partial_ordering::less );
  VERIFY( partial_order(denorm, 0.0) == partial_ordering::greater );
  VERIFY( partial_order(0.0, nan) == partial_ordering::unordered );
  VERIFY( partial_order(nan, nan) == partial_ordering::unordered );
  VERIFY( partial_order(nan, 0.0) == partial_ordering::unordered );
  VERIFY( partial_order(-nan, 0.0) == partial_ordering::unordered );
  VERIFY( partial_order(-nan, min) == partial_ordering::unordered );
  VERIFY( partial_order(-inf, min) == partial_ordering::less );
  VERIFY( partial_order(-nan, -inf) == partial_ordering::unordered );
  VERIFY( partial_order(-inf, -nan) == partial_ordering::unordered );
  VERIFY( partial_order(max, inf) == partial_ordering::less );
  VERIFY( partial_order(inf, max) == partial_ordering::greater );
  VERIFY( partial_order(inf, nan) == partial_ordering::unordered );
  VERIFY( partial_order(1.0, 1.0+epsilon) == partial_ordering::less );
}

namespace N
{
  struct X { int i; };

  constexpr partial_ordering operator<=>(X l, X r)
  {
    if (l.i < 0 && r.i < 0)
      return partial_ordering::equivalent;
    return r.i <=> l.i;
  }

  constexpr bool operator==(X l, X r) { return std::is_eq(l <=> r); }

  static_assert(std::three_way_comparable<X>);
}

void
test04()
{
  using N::X;
  X one{1};
  X negone{-1};

  VERIFY( partial_order(one, X{1}) == partial_ordering::equivalent );
  VERIFY( partial_order(negone, X{-2}) == partial_ordering::equivalent );
  VERIFY( partial_order(one, X{2}) == partial_ordering::greater );
  static_assert( !noexcept(partial_order(X{1}, X{2})) );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
}
