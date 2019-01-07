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

int
main()
{
  test01();
}
