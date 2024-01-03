// Copyright (C) 2015-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++14 } }
// { dg-add-options no_pch }

#include <experimental/numeric>

#ifndef __cpp_lib_experimental_gcd_lcm
# error "Feature-test macro for gcd missing"
#elif __cpp_lib_experimental_gcd_lcm != 201411
# error "Feature-test macro for gcd has wrong value"
#endif

#include <experimental/type_traits>

using std::experimental::fundamentals_v2::gcd;
using std::experimental::is_same_v;

static_assert( gcd(1071, 462) == 21, "" );
static_assert( gcd(2000, 20) == 20, "" );
static_assert( gcd(2011, 17) == 1, "GCD of two primes is 1" );
static_assert( gcd(200, 200) == 200, "GCD of equal numbers is that number" );
static_assert( gcd(0, 13) == 13, "GCD of any number and 0 is that number" );
static_assert( gcd(29, 0) == 29, "GCD of any number and 0 is that number" );
static_assert( gcd(0, 0) == 0, "Zarro Boogs found" );

static_assert(gcd(1u, 2) == 1, "unsigned and signed");
static_assert(gcd(9, 6u) == 3, "unsigned and signed");
static_assert(gcd(3, 4u) == 1, "signed and unsigned");
static_assert(gcd(32u, 24) == 8, "signed and unsigned");
static_assert(gcd(1u, -2) == 1, "unsigned and negative");
static_assert(gcd(-21, 28u) == 7, "unsigned and negative");
static_assert(gcd(-3, 4u) == 1, "negative and unsigned");
static_assert(gcd(33u, -44) == 11, "negative and unsigned");
static_assert(gcd(5u, 6u) == 1, "unsigned and unsigned");
static_assert(gcd(54u, 36u) == 18, "unsigned and unsigned");
static_assert(gcd(-5, -6) == 1, "negative and negative");
static_assert(gcd(-50, -60) == 10, "negative and negative");

static_assert( is_same_v<decltype(gcd(1l, 1)), long>, "" );
static_assert( is_same_v<decltype(gcd(1ul, 1ull)), unsigned long long>, "" );

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
  static_assert( is_same_v<decltype(gcd(p, q)), R>, "" );
  static_assert( is_same_v<decltype(gcd(q, p)), R>, "" );
  R r1 = gcd(p, q);
  // Check non-negative, so conversion to unsigned long doesn't alter value.
  VERIFY( r1 >= 0 );
  // Check for expected result
  VERIFY( (unsigned long long)r1 == r );
  // Check reversing arguments doesn't change result
  VERIFY( gcd(q, p) == r1 );

  P pabs = p < 0 ? -p : p;
  VERIFY( gcd(p, p) == pabs );
  VERIFY( gcd(0, p) == pabs );
  VERIFY( gcd(p, 0) == pabs );
  VERIFY( gcd(1, p) == 1 );
  VERIFY( gcd(p, 1) == 1 );
  Q qabs = q < 0 ? -q : q;
  VERIFY( gcd(q, q) == qabs );
  VERIFY( gcd(0, q) == qabs );
  VERIFY( gcd(q, 0) == qabs );
  VERIFY( gcd(1, q) == 1 );
  VERIFY( gcd(q, 1) == 1 );
  VERIFY( gcd(r, r) == r );
  VERIFY( gcd(0, r) == r );
  VERIFY( gcd(r, 0) == r );
  VERIFY( gcd(1, r) == 1 );
  VERIFY( gcd(r, 1) == 1 );

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
