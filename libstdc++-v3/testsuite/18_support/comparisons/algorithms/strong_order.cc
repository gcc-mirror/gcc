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
// { dg-do compile { target c++2a } }

#include <compare>
#include <limits>

using std::strong_order;
using std::strong_ordering;

static_assert( strong_order(1, 2) == strong_ordering::less );
static_assert( strong_order(1, 1) == strong_ordering::equal );
static_assert( strong_order(2, 1) == strong_ordering::greater );
static_assert( noexcept(strong_order(1, 1)) );

constexpr strong_ordering different_cv_quals(int i, const int j)
{
  return strong_order(i, j);
}
static_assert( different_cv_quals(42, 999) == strong_ordering::less );
static_assert( different_cv_quals(-999, -999) == strong_ordering::equal );
static_assert( different_cv_quals(-99, -111) == strong_ordering::greater );

namespace N
{
  struct X { int i; };

  constexpr strong_ordering operator<=>(X l, X r)
  {
    if (l.i < 0 && r.i < 0)
      return strong_ordering::equivalent;
    return r.i <=> l.i;
  }
}
using N::X;

static_assert( strong_order(X{1}, X{1}) == strong_ordering::equal );
static_assert( strong_order(X{-1}, X{-2}) == strong_ordering::equivalent );
static_assert( strong_order(X{1}, X{2}) == strong_ordering::greater );
static_assert( !noexcept(strong_order(X{1}, X{2})) );
