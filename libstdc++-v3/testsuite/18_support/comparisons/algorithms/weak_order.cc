// Copyright (C) 2019 Free Software Foundation, Inc.
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

using std::weak_order;
using std::weak_ordering;

void
test01()
{
  int one = 1, two = 2;

  VERIFY( weak_order(one, two) == weak_ordering::less );
  VERIFY( weak_order(one, one) == weak_ordering::equivalent );
  VERIFY( weak_order(two, one) == weak_ordering::greater );
  static_assert( noexcept(weak_order(1, 1)) );
}

constexpr weak_ordering different_cv_quals(int i, const int j)
{
  return weak_order(i, j);
}

void
test02()
{
  int fortytwo = 42, nines = 999, lots = 1000;

  VERIFY( different_cv_quals(fortytwo, nines) == weak_ordering::less );
  VERIFY( different_cv_quals(-nines, -nines) == weak_ordering::equivalent );
  VERIFY( different_cv_quals(-nines, -lots) == weak_ordering::greater );
}

void
test03()
{
  double zero = 0.0;
  VERIFY( weak_order(zero, zero) == weak_ordering::equivalent );
  VERIFY( weak_order(-zero, -zero) == weak_ordering::equivalent );
  VERIFY( weak_order(-zero, zero) == weak_ordering::equivalent );
  VERIFY( weak_order(zero, -zero) == weak_ordering::equivalent );

  double min = std::numeric_limits<double>::lowest();
  double max = std::numeric_limits<double>::max();
  double nan = std::numeric_limits<double>::quiet_NaN();
  double inf = std::numeric_limits<double>::infinity();
  double denorm = std::numeric_limits<double>::denorm_min();
  double smallest = std::numeric_limits<double>::min();
  double epsilon = std::numeric_limits<double>::epsilon();
  VERIFY( weak_order(denorm, smallest) == weak_ordering::less );
  VERIFY( weak_order(denorm, 0.0) == weak_ordering::greater );
  VERIFY( weak_order(0.0, nan) == weak_ordering::less );
  VERIFY( weak_order(nan, nan) == weak_ordering::equivalent );
  VERIFY( weak_order(nan, -nan) == weak_ordering::greater );
  VERIFY( weak_order(-nan, nan) == weak_ordering::less );
  VERIFY( weak_order(nan, 0.0) == weak_ordering::greater );
  VERIFY( weak_order(-nan, 0.0) == weak_ordering::less );
  VERIFY( weak_order(-nan, min) == weak_ordering::less );
  VERIFY( weak_order(-inf, min) == weak_ordering::less );
  VERIFY( weak_order(-nan, -inf) == weak_ordering::less );
  VERIFY( weak_order(-inf, -nan) == weak_ordering::greater );
  VERIFY( weak_order(max, inf) == weak_ordering::less );
  VERIFY( weak_order(inf, max) == weak_ordering::greater );
  VERIFY( weak_order(inf, nan) == weak_ordering::less );
  VERIFY( weak_order(1.0, 1.0+epsilon) == weak_ordering::less );
}

namespace N
{
  struct X { int i; };

  constexpr weak_ordering operator<=>(X l, X r)
  {
    if (l.i < 0 && r.i < 0)
      return weak_ordering::equivalent;
    return r.i <=> l.i;
  }
}

void
test04()
{
  using N::X;
  X one{1};
  X negone{-1};

  VERIFY( weak_order(one, X{1}) == weak_ordering::equivalent );
  VERIFY( weak_order(negone, X{-2}) == weak_ordering::equivalent );
  VERIFY( weak_order(one, X{2}) == weak_ordering::greater );
  static_assert( !noexcept(weak_order(X{1}, X{2})) );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
}
