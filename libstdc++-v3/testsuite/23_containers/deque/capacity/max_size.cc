// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// { dg-do run }

#include <deque>
#include <stdexcept>
#include <limits>
#include <testsuite_hooks.h>

typedef std::deque<char> test_type;

typedef test_type::size_type size_type;
typedef test_type::difference_type difference_type;

const difference_type diffmax = std::numeric_limits<difference_type>::max();

void
test01()
{
  test_type v;
  VERIFY( v.max_size() <= diffmax );
}

void
test02()
{
  size_type n = size_type(diffmax) + 1;
  VERIFY( n > test_type().max_size() );

  try {
    test_type v(n);
    VERIFY( false );
  } catch (const std::length_error&) { }

  try {
    test_type v(n, 'x');
    VERIFY( false );
  } catch (const std::length_error&) { }

  try {
    test_type v(n, 'x', test_type::allocator_type());
    VERIFY( false );
  } catch (const std::length_error&) { }
}

#ifdef __GLIBCXX_TYPE_INT_N_0
template<typename T, typename U, bool = (sizeof(T) > sizeof(long long))>
  struct Base_
  {
    typedef T difference_type;
    typedef U size_type;
  };

template<typename T, typename U>
  struct Base_<T, U, false>
  {
    typedef long long difference_type;
    typedef unsigned long long size_type;
  };

typedef Base_<__GLIBCXX_TYPE_INT_N_0, unsigned __GLIBCXX_TYPE_INT_N_0> Base;
#else
struct Base
{
  typedef long long difference_type;
  typedef unsigned long long size_type;
};
#endif

// An iterator with a difference_type larger than ptrdiff_t
struct Iter : Base
{
  typedef std::random_access_iterator_tag iterator_category;
  typedef char value_type;
  typedef const char* pointer;
  typedef const char& reference;
  using Base::difference_type;

  Iter() : n(0) { }
  Iter(size_type n) : n(n) { }

  reference operator*() const { return value; }
  pointer operator->() const { return &value; }

  Iter& operator++() { ++n; return *this; }
  Iter operator++(int) { Iter tmp(*this); ++n; return tmp; }
  Iter& operator--() { --n; return *this; }
  Iter operator--(int) { Iter tmp(*this); --n; return tmp; }

  Iter& operator+=(difference_type d) { n += d; return *this; }
  Iter& operator-=(difference_type d) { n -= d; return *this; }

  difference_type operator-(const Iter& rhs) const { return n - rhs.n; }

  reference operator[](difference_type d) const { return value; }

  bool operator==(const Iter& rhs) const { return n == rhs.n; }
  bool operator!=(const Iter& rhs) const { return n != rhs.n; }
  bool operator<(const Iter& rhs) const { return n < rhs.n; }
  bool operator>(const Iter& rhs) const { return n > rhs.n; }
  bool operator<=(const Iter& rhs) const { return n <= rhs.n; }
  bool operator>=(const Iter& rhs) const { return n >= rhs.n; }

private:
  size_type n;
  static const char value = 'x';
};

const char Iter::value;

Iter operator+(Iter i, Iter::difference_type n) { return i += n; }
Iter operator+(Iter::difference_type n, Iter i) { return i += n; }
Iter operator-(Iter::difference_type n, Iter i) { return i -= n; }

void
test03()
{
  Iter first, last(Iter::size_type(diffmax) + 1);
  VERIFY( std::distance(first, last) > test_type().max_size() );

  try {
    test_type vec(first, last);
    VERIFY(false);
  } catch (const std::length_error&) { }
}

int
main()
{
  test01();
  test02();
  test03();
}
