// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

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

// C++17 29.8.3 [reduce]

#include <numeric>
#include <iterator>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

struct T
{
  T(int);
  T(T&&); // MoveConstructible
  T& operator=(T&&); // not required by the standard, but it needs to be
  T operator+(const T&) const;
};

void
test01()
{
  T t[1]{1};
  std::reduce(t, t+1, T(0));

  using __gnu_test::test_container;
  using __gnu_test::input_iterator_wrapper;
  test_container<T, input_iterator_wrapper> con(t);
  std::reduce(con.begin(), con.end(), T(0));
}

struct Op
{
  T operator()(T&, T&) const&;

  // The standard does *not* require invoking as an rvalue to be supported.
  T operator()(T&, T&) && = delete;

  // The standard does *not* require rvalue arguments to be supported
  // (this is almost certainly a defect and should be allowed).
  T operator()(T&&, T&&) const = delete;
};

void
test02()
{
  T t[1]{1};
  std::reduce(t, t+1, T(0), Op());

  using __gnu_test::test_container;
  using __gnu_test::input_iterator_wrapper;
  test_container<T, input_iterator_wrapper> con(t);
  std::reduce(con.begin(), con.end(), T(0), Op());
}
