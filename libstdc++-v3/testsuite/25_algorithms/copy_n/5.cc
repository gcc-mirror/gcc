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

// { dg-do run { target c++11 } }

#include <algorithm>
#include <testsuite_iterators.h>

void
test01()
{
  // Negative sizes should be a no-op

  const int from[2] = { 1, 2 };
  __gnu_test::input_container<const int> f(from);
  int* to = nullptr;
  std::copy_n(f.begin(), -1, to);

  std::copy_n(from, -20000, to); // random access

  __gnu_test::random_access_container<const int> f2(from);
  std::copy_n(f2.end(), -1, to);
  std::copy_n(f2.begin(), -1, to);
}

struct Size
{
  operator long() const { return 2L; }

  void operator++() = delete;
  void operator--() = delete;
  void operator++(int) = delete;
  void operator--(int) = delete;

  template<typename T> friend void operator+(Size, T) = delete;
  template<typename T> friend void operator+(T, Size) = delete;
  template<typename T> friend void operator-(Size, T) = delete;
  template<typename T> friend void operator-(T, Size) = delete;
  template<typename T> friend void operator==(Size, T) = delete;
  template<typename T> friend void operator==(T, Size) = delete;
  template<typename T> friend void operator!=(Size, T) = delete;
  template<typename T> friend void operator!=(T, Size) = delete;
  template<typename T> friend void operator<(Size, T) = delete;
  template<typename T> friend void operator<(T, Size) = delete;
  template<typename T> friend void operator<=(Size, T) = delete;
  template<typename T> friend void operator<=(T, Size) = delete;
  template<typename T> friend void operator>(Size, T) = delete;
  template<typename T> friend void operator>(T, Size) = delete;
  template<typename T> friend void operator>=(Size, T) = delete;
  template<typename T> friend void operator>=(T, Size) = delete;
};

void
test02()
{
  // C++20 only requires that Size is convertible to an integral type,
  // it doesn't need to support any arithmetic or relational expressions.

  const int from[3] = { 1, 2, 3 };
  __gnu_test::input_container<const int> f(from);
  int to[3] = { };
  __gnu_test::output_container<int> t(to);
  Size s;
  std::copy_n(f.begin(), s, t.begin());
  VERIFY( to[0] == 1 );
  VERIFY( to[1] == 2 );
  VERIFY( to[2] == 0 );

  const int from2[3] = { 11, 22, 33 };
  __gnu_test::random_access_container<const int> f2(from2);
  __gnu_test::output_container<int> t2(to);
  std::copy_n(f2.begin(), s, t2.begin());
  VERIFY( to[0] == 11 );
  VERIFY( to[1] == 22 );
  VERIFY( to[2] == 0 );
}

int
main()
{
  test01();
  test02();
}
