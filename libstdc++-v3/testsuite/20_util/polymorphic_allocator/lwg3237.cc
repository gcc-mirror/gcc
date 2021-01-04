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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <memory_resource>
#include <testsuite_hooks.h>

struct large { alignas(1024) int i; };

void
test01()
{
  std::pmr::polymorphic_allocator<large> a;
  large* p = nullptr;
  try
  {
    p = a.allocate(std::size_t(-1) / 256);
    VERIFY( false );
  }
  catch (const std::bad_array_new_length&)
  {
  }

  std::pmr::polymorphic_allocator<int> a2;
  try
  {
    p = a2.allocate_object<large>(std::size_t(-1) / 256);
    VERIFY( false );
  }
  catch (const std::bad_array_new_length&)
  {
  }
}

int
main()
{
  test01();
}
