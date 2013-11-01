// { dg-options " -std=gnu++1y " }
// { dg-do compile }

// Copyright (C) 2013 Free Software Foundation, Inc.
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

// 20.3.3 Comparisons

#include <functional>

struct R { };

struct L
{
  L operator+(const R&) const { return *this; }
  L operator-(const R&) const { return *this; }
  L operator*(const R&) const { return *this; }
  L operator/(const R&) const { return *this; }
  L operator%(const R&) const { return *this; }
  L operator-() const { return *this; }

  bool operator==(const R&) const { return true; }
  bool operator!=(const R&) const { return false; }
  bool operator<(const R&) const { return false; }
  bool operator<=(const R&) const { return true; }
  bool operator>(const R&) const { return false; }
  bool operator>=(const R&) const { return true; }

  bool operator&&(const R&) const { return true; }
  bool operator||(const R&) const { return true; }
  bool operator!() const { return false; }

  int operator&(const R&) const { return 1; }
  int operator|(const R&) const { return 1; }
  int operator^(const R&) const { return 0; }
  int operator~() const { return 0; }
};

L l;
R r;

// test unary function objects
template<typename F, typename Check = typename F::is_transparent>
bool
test1(F f)
{
  f(l);
  return true;
}

// test binary function objects
template<typename F, typename Check = typename F::is_transparent>
bool
test2(F f)
{
  f(l, r);
  return true;
}

auto plus       = test2( std::plus<>() );
auto minus      = test2( std::minus<>() );
auto multiplies = test2( std::multiplies<>() );
auto divides    = test2( std::divides<>() );
auto modulus    = test2( std::modulus<>() );
auto negate     = test1( std::negate<>() );

auto equal_to       = test2( std::equal_to<>() );
auto not_equal_to   = test2( std::not_equal_to<>() );
auto greater        = test2( std::greater<>() );
auto less           = test2( std::less<>() );
auto greater_equal  = test2( std::greater_equal<>() );
auto less_equal     = test2( std::less_equal<>() );

auto logical_and    = test2( std::logical_and<>() );
auto logical_or     = test2( std::logical_or<>() );
auto logical_not    = test1( std::logical_not<>() );

auto bit_and        = test2( std::bit_and<>() );
auto bit_or         = test2( std::bit_or<>() );
auto bit_xor        = test2( std::bit_xor<>() );
auto bit_not        = test1( std::bit_not<>() );
