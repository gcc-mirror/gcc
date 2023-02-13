// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }

#include <numeric>
#include <climits>
#include <testsuite_hooks.h>

constexpr struct testcase { unsigned long long p, q, r; } testcases[] = {
  { 5, 8, 1 },
  { 6, 35, 1 },
  { 30, 42, 6 },
  { 24, 60, 12 },
  { 55, 144, 1 },
  { 105, 252, 21 },
  { 253, 22121, 11 },
  { 1386, 3213, 63 },
  { 2028, 2049, 3 },
  { 46391, 62527, 2017 },
  { 63245986, 39088169, 1 },
  { 77160074263, 47687519812, 1 },
  { 77160074264, 47687519812, 4 },
};

template<typename P, typename Q>
constexpr bool
check(P p, Q q, unsigned long long r)
{
  using R = std::common_type_t<P, Q>;
  static_assert( std::is_same_v<decltype(std::gcd(p, q)), R> );
  static_assert( std::is_same_v<decltype(std::gcd(q, p)), R> );
  R r1 = std::gcd(p, q);
  // Check non-negative, so conversion to unsigned long doesn't alter value.
  VERIFY( r1 >= 0 );
  // Check for expected result
  VERIFY( (unsigned long long)r1 == r );
  // Check reversing arguments doesn't change result
  VERIFY( std::gcd(q, p) == r1 );

  P pabs = p < 0 ? -p : p;
  VERIFY( std::gcd(p, p) == pabs );
  VERIFY( std::gcd(0, p) == pabs );
  VERIFY( std::gcd(p, 0) == pabs );
  VERIFY( std::gcd(1, p) == 1 );
  VERIFY( std::gcd(p, 1) == 1 );
  Q qabs = q < 0 ? -q : q;
  VERIFY( std::gcd(q, q) == qabs );
  VERIFY( std::gcd(0, q) == qabs );
  VERIFY( std::gcd(q, 0) == qabs );
  VERIFY( std::gcd(1, q) == 1 );
  VERIFY( std::gcd(q, 1) == 1 );
  VERIFY( std::gcd(r, r) == r );
  VERIFY( std::gcd(0, r) == r );
  VERIFY( std::gcd(r, 0) == r );
  VERIFY( std::gcd(1, r) == 1 );
  VERIFY( std::gcd(r, 1) == 1 );

  return true;
}

constexpr bool
test01()
{
  for (auto t : testcases)
  {
    check(t.p, t.q, t.r);

    if (t.p <= LONG_MAX && t.q <= LONG_MAX)
    {
      check( (long)t.p,  (long)t.p, t.p);
      check(-(long)t.p,  (long)t.p, t.p);
      check(-(long)t.p, -(long)t.p, t.p);

      check( (long)t.p, t.q, t.r);
      check(-(long)t.p, t.q, t.r);

      check(t.p,  (long)t.q, t.r);
      check(t.p, -(long)t.q, t.r);

      check( (long)t.p,  (long)t.q, t.r);
      check( (long)t.p, -(long)t.q, t.r);
      check(-(long)t.p,  (long)t.q, t.r);
      check(-(long)t.p, -(long)t.q, t.r);
    }

    if (t.p <= INT_MAX && t.q <= INT_MAX)
    {
      check((long)t.p,  (int)t.q, t.r);
      check(-(int)t.p, (long)t.q, t.r);

      check( (int)t.p, (unsigned)t.q, t.r);
      check(-(int)t.p, (unsigned)t.q, t.r);

      check(-(int)t.p,  -(int)t.q, t.r);
      check(-(int)t.p, -(long)t.q, t.r);
    }

    if (t.p <= SHRT_MAX && t.q <= SHRT_MAX)
    {
      check(  (long)t.p, (short)t.q, t.r);
      check(-(short)t.p,  (long)t.q, t.r);

      check( (short)t.p, (unsigned short)t.q, t.r);
      check(-(short)t.p, (unsigned short)t.q, t.r);

      check(-(short)t.p, -(short)t.q, t.r);
      check(-(short)t.p,  -(long)t.q, t.r);
    }
  }
  return true;
}


int main()
{
  static_assert( test01() );  // constexpr
  VERIFY( test01() );	      // non-constexpr
}
