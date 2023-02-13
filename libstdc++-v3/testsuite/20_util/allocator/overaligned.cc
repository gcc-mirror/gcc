// Copyright (C) 2016-2023 Free Software Foundation, Inc.
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

// { dg-options "-faligned-new" }
// { dg-do run { target { c++11 && { c++17 || std_allocator_new } } } }
// { dg-require-cstdint "" }
// { dg-require-effective-target hosted }

#include <memory>
#include <cstddef>
#include <cstdint>
#include <testsuite_hooks.h>

constexpr std::size_t align = alignof(std::max_align_t) * 4;

struct X {
  alignas(align) char c;
};

void
test01()
{
  std::allocator<X> a;
  X* p1 = a.allocate(1);
  VERIFY( (reinterpret_cast<std::uintptr_t>(p1) % align) == 0 );
  a.deallocate(p1, 1);
  X* p2 = a.allocate(20);
  VERIFY( (reinterpret_cast<std::uintptr_t>(p2) % align) == 0 );
  a.deallocate(p2, 20);
}

int
main()
{
  test01();
}
