// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

// { dg-options "-O2" }
// { dg-do run { target c++20 } }

#include <memory>
#include <cstdint>
#include <testsuite_hooks.h>

void
test01()
{
  int i = 0;
  int* p = std::assume_aligned<alignof(int)>(&i);
  VERIFY( p == &i );
}

[[gnu::noipa,gnu::noinline]]
int*
create_aligned(std::size_t alignment, void* p, std::size_t n)
{
  return ::new(std::align(alignment, sizeof(int), p, n)) int(42);
}

extern "C" void undefined(); // call to this should be optimized away

void
test02()
{
  unsigned char buf[sizeof(int) * 128];
  int* p = create_aligned(64, buf + 1, sizeof(buf) - 1);
  int* q = std::assume_aligned<64>(p);
  if ((std::uintptr_t)q % 64)
    undefined();
  VERIFY( p == q );
}

int main()
{
  test01();
  test02();
}
