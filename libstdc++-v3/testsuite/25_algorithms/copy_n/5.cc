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

// { dg-do run { target c++11 } }

#include <algorithm>
#include <testsuite_iterators.h>

void
test01()
{
  // Negative sizes should be a no-op

  using namespace __gnu_test;

  const int from[2] = { 1, 2 };
  test_container<const int, input_iterator_wrapper> f(from);
  int* to = nullptr;
  std::copy_n(f.begin(), -1, to);

  std::copy_n(from, -20000, to); // random access

  test_container<const int, random_access_iterator_wrapper> f2(from);
  std::copy_n(f2.end(), -1, to);
  std::copy_n(f2.begin(), -1, to);
}

int
main()
{
  test01();
}
