// Copyright (C) 2017 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <vector>
#include <testsuite_iterators.h>

template<typename T>
  using input_iterator_seq
    = __gnu_test::test_container<T, __gnu_test::input_iterator_wrapper>;

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

void
test01()
{
  std::vector<unsigned> s0;

  std::vector s1 = s0;
  check_type<std::vector<unsigned>>(s1);

  std::vector s2 = std::move(s0);
  check_type<std::vector<unsigned>>(s2);

  const std::vector s3 = s0;
  check_type<const std::vector<unsigned>>(s3);

  const std::vector s4 = s3;
  check_type<const std::vector<unsigned>>(s4);
}

void
test02()
{
  unsigned a[1] = {};
  input_iterator_seq<unsigned> seq(a);

  std::vector s1(seq.begin(), seq.end());
  check_type<std::vector<unsigned>>(s1);

  std::vector s2(seq.begin(), seq.end(), std::allocator<unsigned>());
  check_type<std::vector<unsigned>>(s2);

  std::vector s3(1U, 2L);
  check_type<std::vector<long>>(s3);

  std::vector s4(1U, 2L, std::allocator<long>());
  check_type<std::vector<long>>(s4);
}
