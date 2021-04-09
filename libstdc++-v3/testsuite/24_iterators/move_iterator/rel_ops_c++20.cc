// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <iterator>

template<int>
struct Iter
{
  using iterator_category = std::random_access_iterator_tag;
  using value_type = int;
  using pointer = int*;
  using reference = int&;
  using difference_type = std::ptrdiff_t;

  Iter();

  Iter& operator++();
  Iter operator++(int);
  Iter& operator--();
  Iter operator--(int);
  int& operator*() const;
  int* operator->() const;

  int& operator[](difference_type) const;

  Iter& operator+=(difference_type);
  Iter& operator-=(difference_type);

  template<int N> friend Iter operator+(Iter<N>, difference_type);
  template<int N> friend Iter operator+(difference_type, Iter<N>);
  template<int N> friend Iter operator-(Iter<N>, difference_type);
  template<int N> friend difference_type operator-(Iter<N>, Iter<N>);

  template<int N> friend bool operator==(Iter<N>, Iter<N>);
  template<int N> friend std::weak_ordering operator<=>(Iter<N>, Iter<N>);
};

static_assert( std::random_access_iterator<Iter<0>> );

int   operator==(Iter<0>, long*);
void* operator< (Iter<1>, long*);
bool& operator< (long*, Iter<2>);

using std::move_iterator;

static_assert( std::three_way_comparable<move_iterator<Iter<0>>> );

move_iterator<Iter<0>> l0{Iter<0>()};
move_iterator<Iter<1>> l1{Iter<1>()};
move_iterator<Iter<2>> l2{Iter<2>()};
move_iterator<long*> r{nullptr};

bool b0 = l0 == r;
bool b1 = l0 != r;
bool b2 = l1 < r;
bool b3 = l2 > r;
bool b4 = l2 <= r;
bool b5 = l1 >= r;

template<int N>
  concept has_eq
    = requires (move_iterator<Iter<N>> l, move_iterator<long*> r)
      { l == r; };

template<int N>
  concept has_ne
    = requires (move_iterator<Iter<N>> l, move_iterator<long*> r)
      { l != r; };

template<int N>
  concept has_lt
    = requires (move_iterator<Iter<N>> l, move_iterator<long*> r)
      { l < r; };

template<int N>
  concept has_gt
    = requires (move_iterator<Iter<N>> l, move_iterator<long*> r)
      { l > r; };

template<int N>
  concept has_le
    = requires (move_iterator<Iter<N>> l, move_iterator<long*> r)
      { l <= r; };

template<int N>
  concept has_ge
    = requires (move_iterator<Iter<N>> l, move_iterator<long*> r)
      { l >= r; };

static_assert( has_eq<0> );
static_assert( ! has_eq<1> );
static_assert( ! has_eq<2> );

static_assert( has_ne<0> ); // uses synthesized operator!=
static_assert( ! has_ne<1> );
static_assert( ! has_ne<2> );

static_assert( ! has_lt<0> );
static_assert( has_lt<1> );
static_assert( ! has_lt<2> );

static_assert( ! has_gt<0> );
static_assert( ! has_gt<1> );
static_assert( has_gt<2> );

static_assert( ! has_le<0> );
static_assert( ! has_le<1> );
static_assert( has_le<2> );

static_assert( ! has_ge<0> );
static_assert( has_ge<1> );
static_assert( ! has_ge<2> );

int arr[3] = { 1, 2, 3 };
constexpr std::move_iterator<int*> beg{std::begin(arr)};
constexpr std::move_iterator<const int*> cbeg{std::cbegin(arr)};
static_assert( beg == cbeg );
static_assert( beg <= cbeg );
static_assert( beg >= cbeg );
static_assert( std::is_eq(beg <=> cbeg) );
constexpr std::move_iterator<const int*> cend{std::cend(arr)};
static_assert( beg != cend );
static_assert( beg < cend );
static_assert( cend > beg );
static_assert( beg <= cend );
static_assert( cend >= beg );
static_assert( std::is_lt(beg <=> cend) );
