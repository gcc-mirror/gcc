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

// { dg-options "-std=gnu++2a -D_GLIBCXX_ASSERTIONS" }
// { dg-do run { target c++2a } }
// { dg-xfail-run-if "__glibcxx_assert in ceil2 should fail" { *-*-* } }

#include <bit>

// P1355R2: not a constant expression if the result is not representable

template<auto N, typename = void>
  struct ceil2_valid
  : std::false_type { };

template<auto N>
  struct ceil2_valid<N, std::void_t<char[(std::ceil2(N), 1)]>>
  : std::true_type { };

template<typename T>
  constexpr T max = std::numeric_limits<T>::max();
template<typename T>
  constexpr T maxpow2 = T(1) << (std::numeric_limits<T>::digits - 1);

static_assert( ceil2_valid<maxpow2<unsigned char>>() );
static_assert( !ceil2_valid<maxpow2<unsigned char> + (unsigned char)1>() );

static_assert( !ceil2_valid<max<unsigned char>>() );
static_assert( !ceil2_valid<max<unsigned char> - (unsigned char)1>() );

static_assert( ceil2_valid<maxpow2<unsigned short>>() );
static_assert( !ceil2_valid<maxpow2<unsigned short> + (unsigned short)1>() );
static_assert( !ceil2_valid<max<unsigned short>>() );
static_assert( !ceil2_valid<max<unsigned short> - (unsigned short)1>() );

static_assert( ceil2_valid<maxpow2<unsigned int>>() );
static_assert( !ceil2_valid<maxpow2<unsigned int> + 1u>() );
static_assert( !ceil2_valid<max<unsigned int>>() );
static_assert( !ceil2_valid<max<unsigned int> - 1u>() );

static_assert( ceil2_valid<maxpow2<unsigned long>>() );
static_assert( !ceil2_valid<maxpow2<unsigned long> + 1ul>() );
static_assert( !ceil2_valid<max<unsigned long>>() );
static_assert( !ceil2_valid<max<unsigned long> - 1ul>() );

static_assert( ceil2_valid<maxpow2<unsigned long long>>() );
static_assert( !ceil2_valid<maxpow2<unsigned long long> + 1ull>() );
static_assert( !ceil2_valid<max<unsigned long long>>() );
static_assert( !ceil2_valid<max<unsigned long long> - 1ull>() );

void
test01()
{
  std::ceil2( maxpow2<unsigned> + 1u ); // should fail __glibcxx_assert
}

int main()
{
  test01();
}
