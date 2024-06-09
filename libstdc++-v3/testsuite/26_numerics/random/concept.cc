// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }
// { dg-require-cstdint "" }

#include <random>

static_assert( std::uniform_random_bit_generator<std::default_random_engine> );
static_assert( std::uniform_random_bit_generator<std::minstd_rand0> );
static_assert( std::uniform_random_bit_generator<std::mt19937> );

struct G1
{
  unsigned char operator()();
  static constexpr unsigned char min() { return 0; }
  static constexpr unsigned char max() { return 10; }
};

static_assert( std::uniform_random_bit_generator<G1> );

struct G2
{
  unsigned operator()();
  static constexpr unsigned min() { return 0; }
  static constexpr unsigned max() { return -1U; }
};

static_assert( std::uniform_random_bit_generator<G2> );

struct G3
{
  unsigned long long operator()();
  static constexpr unsigned long long min() { return 0; }
  static constexpr unsigned long long max() { return -1ULL; }
};

static_assert( std::uniform_random_bit_generator<G3> );

struct G4
{
  unsigned operator()(int = 0, int = 0); // extra params, with default args
  static constexpr unsigned min(long = 0) { return 0; }
  static constexpr unsigned max(void* = nullptr) { return -1U; }
};

static_assert( std::uniform_random_bit_generator<G4> );

struct G5
{
  unsigned operator()() &; // ref-qualifier
  static constexpr unsigned min() { return 0; }
  static constexpr unsigned max() { return 10; }
};

static_assert( std::uniform_random_bit_generator<G5> );

struct G6
{
  unsigned operator()() const; // cv-qualifier
  static constexpr unsigned min() { return 0; }
  static constexpr unsigned max() { return 10; }
};

static_assert( std::uniform_random_bit_generator<G6> );

struct G7
{
  unsigned operator()() volatile; // cv-qualifier
  static constexpr unsigned min() { return 0; }
  static constexpr unsigned max() { return 10; }
};

static_assert( std::uniform_random_bit_generator<G7> );

struct G8
{
  unsigned operator()() const volatile; // cv-qualifiers
  static constexpr unsigned min() { return 0; }
  static constexpr unsigned max() { return 10; }
};

static_assert( std::uniform_random_bit_generator<G8> );

struct G9
{
  unsigned operator()() const volatile; // cv-qualifiers
  static constexpr unsigned min() { return 0; }
  static constexpr unsigned max() { return 10; }
};

static_assert( std::uniform_random_bit_generator<G9> );

struct G10
{
  unsigned operator()() const volatile & noexcept; // cv/ref/noexcept
  static constexpr unsigned min() noexcept { return 0; }
  static constexpr unsigned max() noexcept { return 10; }
};

static_assert( std::uniform_random_bit_generator<G10> );

// Negative tests.

static_assert( ! std::uniform_random_bit_generator<void> );
static_assert( ! std::uniform_random_bit_generator<int> );
static_assert( ! std::uniform_random_bit_generator<unsigned(*)()> );

struct N1
{
  unsigned operator()();
  constexpr unsigned min() { return 0; } // non-static
  static constexpr unsigned max() { return 1; }
};

static_assert( ! std::uniform_random_bit_generator<N1> );

struct N2
{
  unsigned operator()();
  static constexpr unsigned min() { return 0; }
  constexpr unsigned max() { return 1; } // non-static
};

static_assert( ! std::uniform_random_bit_generator<N2> );

struct N3
{
  unsigned operator()();
  // no N3::min()
  static constexpr unsigned max() { return 1; }
};

static_assert( ! std::uniform_random_bit_generator<N3> );

struct N4
{
  unsigned operator()();
  static constexpr unsigned min() { return 0; }
  // no N4::max()
};

static_assert( ! std::uniform_random_bit_generator<N4> );

struct N5
{
  // no operator()
  static constexpr unsigned min() { return 0; }
  static constexpr unsigned max() { return 1; }
};

static_assert( ! std::uniform_random_bit_generator<N5> );

struct N6
{
  int operator()(); // returns signed integral
  static constexpr unsigned min() { return 0; }
  static constexpr unsigned max() { return 1; }
};

static_assert( ! std::uniform_random_bit_generator<N6> );

struct N7
{
  unsigned operator()();
  static constexpr unsigned long min() { return 0; } // different return type
  static constexpr unsigned max() { return 1; }
};

static_assert( ! std::uniform_random_bit_generator<N7> );

struct N8
{
  unsigned operator()();
  static constexpr unsigned min() { return 0; }
  static constexpr unsigned long max() { return 1; } // different return type
};

static_assert( ! std::uniform_random_bit_generator<N8> );

struct N9
{
  unsigned operator()();
  static constexpr unsigned long min() { return 0; } // different return type
  static constexpr unsigned long max() { return 1; } // different return type
};

static_assert( ! std::uniform_random_bit_generator<N9> );

struct N10
{
  unsigned operator()() &&; // ref-qualifier
  static constexpr unsigned min() { return 0; }
  static constexpr unsigned max() { return 1; }
};

static_assert( ! std::uniform_random_bit_generator<N10> );

struct N11
{
  unsigned operator()() const &&; // ref-qualifier
  static constexpr unsigned min() { return 0; }
  static constexpr unsigned max() { return 1; }
};

static_assert( ! std::uniform_random_bit_generator<N11> );

struct N12
{
  unsigned operator()();
  static unsigned min() { return 0; } // not constexpr
  static constexpr unsigned max() { return 1; }
};

#if __cpp_implicit_constexpr
static_assert( std::uniform_random_bit_generator<N12> ); // LWG 3150
#else
static_assert( ! std::uniform_random_bit_generator<N12> ); // LWG 3150
#endif

struct N13
{
  unsigned operator()();
  static constexpr unsigned min() { return 0; }
  static unsigned max() { return 1; } // not constexpr
};

#if __cpp_implicit_constexpr
static_assert( std::uniform_random_bit_generator<N13> ); // LWG 3150
#else
static_assert( ! std::uniform_random_bit_generator<N13> ); // LWG 3150
#endif

struct N14
{
  unsigned operator()();
  static constexpr unsigned min() { return 1; }
  static constexpr unsigned max() { return 0; } // max not greater than min
};

static_assert( ! std::uniform_random_bit_generator<N14> ); // LWG 3150
