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

  // Define the full set of operators for same-type comparisons
  template<int N> friend bool operator==(Iter<N>, Iter<N>); // synthesizes !=
  template<int N> friend bool operator<(Iter<N>, Iter<N>);
  template<int N> friend bool operator>(Iter<N>, Iter<N>);
  template<int N> friend bool operator<=(Iter<N>, Iter<N>);
  template<int N> friend bool operator>=(Iter<N>, Iter<N>);
};

static_assert( std::random_access_iterator<Iter<0>> );

// Define a single kind of mixed-type comparison for each specialization.
int   operator==(Iter<0>, long*);
void* operator!=(Iter<1>, long*);
bool& operator< (Iter<2>, long*);
int   operator> (Iter<3>, long*);
void* operator<=(Iter<4>, long*);
bool& operator>=(Iter<5>, long*);

using std::reverse_iterator;

reverse_iterator<Iter<0>> l0{Iter<0>()};
reverse_iterator<Iter<1>> l1{Iter<1>()};
reverse_iterator<Iter<2>> l2{Iter<2>()};
reverse_iterator<Iter<3>> l3{Iter<3>()};
reverse_iterator<Iter<4>> l4{Iter<4>()};
reverse_iterator<Iter<5>> l5{Iter<5>()};
reverse_iterator<long*> r{nullptr};

bool b0 = l0 == r;
bool b1 = l1 != r;
bool b2 = l2 > r;
bool b3 = l3 < r;
bool b4 = l4 >= r;
bool b5 = l5 <= r;

template<int N>
  concept has_eq
    = requires (reverse_iterator<Iter<N>> l, reverse_iterator<long*> r)
      { l == r; };

template<int N>
  concept has_ne
    = requires (reverse_iterator<Iter<N>> l, reverse_iterator<long*> r)
      { l != r; };

template<int N>
  concept has_lt
    = requires (reverse_iterator<Iter<N>> l, reverse_iterator<long*> r)
      { l < r; };

template<int N>
  concept has_gt
    = requires (reverse_iterator<Iter<N>> l, reverse_iterator<long*> r)
      { l > r; };

template<int N>
  concept has_le
    = requires (reverse_iterator<Iter<N>> l, reverse_iterator<long*> r)
      { l <= r; };

template<int N>
  concept has_ge
    = requires (reverse_iterator<Iter<N>> l, reverse_iterator<long*> r)
      { l >= r; };

static_assert( has_eq<0> );
static_assert( ! has_eq<1> );
static_assert( ! has_eq<2> );
static_assert( ! has_eq<3> );
static_assert( ! has_eq<4> );
static_assert( ! has_eq<5> );

static_assert( has_ne<0> ); // uses synthesized operator!=
static_assert( has_ne<1> );
static_assert( ! has_ne<2> );
static_assert( ! has_ne<3> );
static_assert( ! has_ne<4> );
static_assert( ! has_ne<5> );

static_assert( ! has_lt<0> );
static_assert( ! has_lt<1> );
static_assert( ! has_lt<2> );
static_assert( has_lt<3> );
static_assert( ! has_lt<4> );
static_assert( ! has_lt<5> );

static_assert( ! has_gt<0> );
static_assert( ! has_gt<1> );
static_assert( has_gt<2> );
static_assert( ! has_gt<3> );
static_assert( ! has_gt<4> );
static_assert( ! has_gt<5> );

static_assert( ! has_le<0> );
static_assert( ! has_le<1> );
static_assert( ! has_le<2> );
static_assert( ! has_le<3> );
static_assert( ! has_le<4> );
static_assert( has_le<5> );

static_assert( ! has_ge<0> );
static_assert( ! has_ge<1> );
static_assert( ! has_ge<2> );
static_assert( ! has_ge<3> );
static_assert( has_ge<4> );
static_assert( ! has_ge<5> );

int arr[3] = { 1, 2, 3 };
constexpr std::reverse_iterator<int*> rbeg = std::rbegin(arr);
constexpr std::reverse_iterator<const int*> crbeg = std::crbegin(arr);
static_assert( rbeg == crbeg );
static_assert( rbeg <= crbeg );
static_assert( rbeg >= crbeg );
static_assert( std::is_eq(rbeg <=> crbeg) );
constexpr std::reverse_iterator<const int*> crend = std::crend(arr);
static_assert( rbeg != crend );
static_assert( rbeg < crend );
static_assert( crend > rbeg );
static_assert( rbeg <= crend );
static_assert( crend >= rbeg );
static_assert( std::is_lt(rbeg <=> crend) );
