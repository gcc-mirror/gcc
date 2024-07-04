// { dg-do run { target c++20 } }

// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// Tuple

#include <tuple>
#include <testsuite_hooks.h>

using namespace std;

template<typename T>
bool self_consistent(const T& x)
{
  return std::is_eq(x <=> x) && x == x && !(x != x) && x <= x && !(x < x);
}

void
test01()
{
  int i=0;
  int j=0;
  int k=2;
  tuple<int, int, int> a(0, 0, 0);
  tuple<int, int, int> b(0, 0, 1);
  tuple<int& , int& , int&> c(i,j,k);
  tuple<const int&, const int&, const int&> d(c);
  VERIFY( self_consistent(a) );
  VERIFY( self_consistent(b) );
  VERIFY( self_consistent(c) );
  VERIFY( self_consistent(d) );
  VERIFY( !(a > a) && !(b > b) );
  VERIFY( a >= a && b >= b );
  VERIFY( a < b && !(b < a) && a <= b && !(b <= a) );
  VERIFY( b > a && !(a > b) && b >= a && !(a >= b) );

  VERIFY( std::is_lt(a <=> b) );
  VERIFY( std::is_gt(b <=> a) );
  VERIFY( std::is_gt(c <=> a) );
  VERIFY( std::is_eq(c <=> d) );

  static_assert( std::is_same_v<decltype(a <=> d), std::strong_ordering> );
}

template<typename T, typename U, typename C>
constexpr bool
check_compare(T&& t, U&& u, C c)
{
  using R = std::compare_three_way_result_t<T, U>;
  static_assert( std::same_as<C, R> );
  return (t <=> u) == c;
}

void
test02()
{
  using std::strong_ordering;
  using std::weak_ordering;
  using std::partial_ordering;

  using T0 = std::tuple<>;
  static_assert( check_compare(T0(), T0(), strong_ordering::equal) );

  using Ti = std::tuple<int>;
  using Tu = std::tuple<unsigned>;
  static_assert( check_compare(Ti(1), Tu(1u), weak_ordering::equivalent) );
  static_assert( check_compare(Ti(1), Tu(2u), weak_ordering::less) );
  static_assert( check_compare(Ti(-1), Tu(1u), weak_ordering::greater) );

  using Tii = std::tuple<int, int>;
  using Tlu = std::tuple<long, unsigned>;
  static_assert( check_compare(Tii(1, 2), Tlu(2l, 1u), weak_ordering::less) );

  using Tid = std::tuple<int, double>;
  static_assert( check_compare(Tii(3, 4), Tid(2, 0.9), partial_ordering::greater) );

  static_assert( !std::three_way_comparable_with<T0, Ti> );
  static_assert( !std::three_way_comparable_with<Ti, Tii> );
  static_assert( !std::three_way_comparable_with<Ti, Tid> );
}

int main()
{
  test01();
  test02();
}
