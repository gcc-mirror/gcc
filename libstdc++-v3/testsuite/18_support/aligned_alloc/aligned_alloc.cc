// Copyright (C) 2018 Free Software Foundation, Inc.
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
// { dg-do run { target c++17 } }
// { dg-require-cstdint "" }

#include <cstdlib>
#include <cstdint>
#include <testsuite_hooks.h>

void
test01()
{
#ifdef _GLIBCXX_HAVE_ALIGNED_ALLOC
  void* p = std::aligned_alloc(256, 1);
  if (p)
  {
    VERIFY( (reinterpret_cast<std::uintptr_t>(p) % 256) == 0 );
    std::free(p);
  }
#endif
}

int
main()
{
  test01();
}
