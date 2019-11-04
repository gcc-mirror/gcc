// Copyright (C) 2016-2019 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>

template class std::allocator<void>;

void
test01()
{
  int i;
  using alloc_type = std::allocator<void>;
  alloc_type a;
  std::allocator_traits<alloc_type>::construct(a, &i, 42);
  VERIFY( i == 42 );
  std::allocator_traits<alloc_type>::destroy(a, &i);
}

// These properties are formally unspecified, but have always been true for
// the libstdc++ definition of allocator<void>.
static_assert(
    std::is_trivially_default_constructible<std::allocator<void>>::value,
    "explicit specialization has trivial default constructor");
static_assert(
    std::is_trivially_copy_constructible<std::allocator<void>>::value,
    "explicit specialization has trivial copy constructor");
static_assert(
    std::is_trivially_move_constructible<std::allocator<void>>::value,
    "explicit specialization has trivial move constructor");
static_assert(
    std::is_trivially_destructible<std::allocator<void>>::value,
    "explicit specialization has trivial destructor");

#if __cplusplus > 201703L
// C++20 removes the allocator<void> explicit specialization, so it can now be
// constructed using the converting constructor from other specializations.
static_assert( std::is_constructible_v<std::allocator<void>,
				       std::allocator<int>> );
#endif

int
main()
{
  test01();
}
