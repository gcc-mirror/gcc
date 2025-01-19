// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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
// { dg-require-effective-target hosted }

#include <memory>
#include <new>
#include <limits>
#include <testsuite_hooks.h>

// LWG 3190. std::allocator::allocate sometimes returns too little storage

void
test01()
{
  struct A { char biiiiig[1 << 16]; };
  std::allocator<A> a;
  try
  {
    std::size_t max = std::numeric_limits<std::size_t>::max() / sizeof(A);
    A* p = a.allocate(max + 1);
    throw p;
  }
#if __cplusplus >= 201103L
  catch (const std::bad_array_new_length&)
  {
  }
#endif
  catch (const std::bad_alloc&)
  {
    VERIFY( __cplusplus < 201103L );
  }
}

int
main()
{
  test01();
}
