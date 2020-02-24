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

#include <testsuite_iterators.h>

using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;

template<typename T>
struct move_only_wrapper : input_iterator_wrapper<T>
{
  using input_iterator_wrapper<T>::input_iterator_wrapper;

  move_only_wrapper()
    : input_iterator_wrapper<T>(nullptr, nullptr)
  { }

  move_only_wrapper(const move_only_wrapper&) = delete;
  move_only_wrapper&
  operator=(const move_only_wrapper&) = delete;

  move_only_wrapper(move_only_wrapper&&) = default;
  move_only_wrapper&
  operator=(move_only_wrapper&&) = default;

  using input_iterator_wrapper<T>::operator++;

  move_only_wrapper&
  operator++()
  {
    input_iterator_wrapper<T>::operator++();
    return *this;
  }
};

static_assert(std::input_iterator<move_only_wrapper<int>>);
static_assert(!std::forward_iterator<move_only_wrapper<int>>);
static_assert(!std::copyable<move_only_wrapper<int>>);

// LWG 3390
void
test01()
{
  int x[] = {1,2,3,4};
  test_range<int, move_only_wrapper> rx(x);
  auto it = std::make_move_iterator(rx.begin());
}
