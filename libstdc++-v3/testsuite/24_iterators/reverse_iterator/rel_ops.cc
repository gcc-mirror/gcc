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

// { dg-do compile }

#include <iterator>

template<int>
struct Iter
{
  typedef std::random_access_iterator_tag iterator_category;
  typedef int value_type;
  typedef int* pointer;
  typedef int& reference;
  typedef std::ptrdiff_t difference_type;

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

// Define a single kind of mixed-type comparison for each specialization.
int   operator==(Iter<0>, long*);
void* operator!=(Iter<1>, long*);
bool& operator< (Iter<2>, long*);
int   operator> (Iter<3>, long*);
void* operator<=(Iter<4>, long*);
bool& operator>=(Iter<5>, long*);

using std::reverse_iterator;

reverse_iterator< Iter<0> > l0;
reverse_iterator< Iter<1> > l1;
reverse_iterator< Iter<2> > l2;
reverse_iterator< Iter<3> > l3;
reverse_iterator< Iter<4> > l4;
reverse_iterator< Iter<5> > l5;
reverse_iterator<long*> r;

// PR libstdc++/94354
// Check that these operators use the correct operator on the
// underlying iterator types.
bool b0 = l0 == r;
bool b1 = l1 != r;
bool b2 = l2 > r;
bool b3 = l3 < r;
bool b4 = l4 >= r;
bool b5 = l5 <= r;

#if __cplusplus >= 201703L
int arr[3] = { 1, 2, 3 };
constexpr std::reverse_iterator<int*> rbeg = std::rbegin(arr);
constexpr std::reverse_iterator<const int*> crbeg = std::crbegin(arr);
static_assert( rbeg == crbeg );
static_assert( rbeg <= crbeg );
static_assert( rbeg >= crbeg );
constexpr std::reverse_iterator<const int*> crend = std::crend(arr);
static_assert( rbeg != crend );
static_assert( rbeg < crend );
static_assert( crend > rbeg );
static_assert( rbeg <= crend );
static_assert( crend >= rbeg );
#endif // C++17
