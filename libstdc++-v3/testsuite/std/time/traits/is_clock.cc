// Copyright (C) 2020 Free Software Foundation, Inc.
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

#include <chrono>
#include <slow_clock.h>

namespace chrono = std::chrono;

static_assert( chrono::is_clock<chrono::system_clock>::value );
static_assert( chrono::is_clock_v<chrono::system_clock> );

static_assert( chrono::is_clock<chrono::high_resolution_clock>::value );
static_assert( chrono::is_clock_v<chrono::high_resolution_clock> );

static_assert( chrono::is_clock<chrono::steady_clock>::value );
static_assert( chrono::is_clock_v<chrono::steady_clock> );

static_assert(chrono::is_clock<chrono::file_clock>::value);
static_assert(chrono::is_clock_v<chrono::file_clock>);

// Clock<xxx_clock> will not use the specialization of is_clock<xxx_clock>
template<typename C> struct Clock : C { };

static_assert( chrono::is_clock<Clock<chrono::system_clock>>::value );
static_assert( chrono::is_clock_v<Clock<chrono::system_clock>> );

static_assert( chrono::is_clock<Clock<chrono::high_resolution_clock>>::value );
static_assert( chrono::is_clock_v<Clock<chrono::high_resolution_clock>> );

static_assert( chrono::is_clock<Clock<chrono::steady_clock>>::value );
static_assert( chrono::is_clock_v<Clock<chrono::steady_clock>> );

static_assert(chrono::is_clock<Clock<chrono::file_clock>>::value);
static_assert(chrono::is_clock_v<Clock<chrono::file_clock>>);

static_assert( chrono::is_clock<__gnu_test::slow_clock>::value );
static_assert( chrono::is_clock_v<__gnu_test::slow_clock> );

// Negative tests

static_assert( ! chrono::is_clock<int>::value );
static_assert( ! chrono::is_clock_v<int> );

static_assert( ! chrono::is_clock<void>::value );
static_assert( ! chrono::is_clock_v<void> );

struct not_a_clock_1
{
  using rep = int;
  using period = std::ratio<4, 2>;
  using duration = chrono::duration<long, period>; // different rep
  using time_point = chrono::time_point<not_a_clock_1>;
  static constexpr bool is_steady = false;
  static time_point now();
};

static_assert( ! chrono::is_clock<not_a_clock_1>::value );
static_assert( ! chrono::is_clock_v<not_a_clock_1> );

struct not_a_clock_2
{
  using rep = int;
  using period = int; // not a std::ratio
  using duration = chrono::duration<rep>;
  using time_point = chrono::time_point<not_a_clock_2>;
  static constexpr bool is_steady = false;
  static time_point now();
};

static_assert( ! chrono::is_clock<not_a_clock_2>::value );
static_assert( ! chrono::is_clock_v<not_a_clock_2> );

struct not_a_clock_3
{
  using rep = int;
  using period = std::ratio<1>;
  using duration = chrono::duration<rep>;
  // wrong duration:
  using time_point = chrono::time_point<not_a_clock_3, chrono::duration<long>>;
  static constexpr bool is_steady = false;
  static time_point now();
};

static_assert( ! chrono::is_clock<not_a_clock_3>::value );
static_assert( ! chrono::is_clock_v<not_a_clock_3> );

struct not_a_clock_4
{
  using rep = int;
  using period = std::ratio<1>;
  using duration = chrono::duration<rep>;
  using time_point = chrono::time_point<not_a_clock_4>;
  static constexpr int is_steady = 0; // not a const bool
  static time_point now();
};

static_assert( ! chrono::is_clock<not_a_clock_4>::value );
static_assert( ! chrono::is_clock_v<not_a_clock_4> );

struct not_a_clock_5
{
  using rep = int;
  using period = std::ratio<1>;
  using duration = chrono::duration<rep>;
  using time_point = chrono::time_point<not_a_clock_5>;
  static constexpr bool is_steady = false;
  static int now(); // wrong return type
};

static_assert( ! chrono::is_clock<not_a_clock_5>::value );
static_assert( ! chrono::is_clock_v<not_a_clock_5> );

struct not_a_clock_6
{
  using rep = int;
  using period = std::ratio<1>;
  using duration = chrono::duration<rep>;
  using time_point = chrono::time_point<not_a_clock_6>;
  const bool is_steady = false; // not static
  static time_point now();
};

static_assert( ! chrono::is_clock<not_a_clock_6>::value );
static_assert( ! chrono::is_clock_v<not_a_clock_6> );
