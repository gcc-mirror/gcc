// { dg-options "-std=gnu++20" }
// { dg-do run { target c++2a } }
// { dg-xfail-run-if "AIX operator new" { powerpc-ibm-aix* } }

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

// C++20 20.11.1.5 unique_ptr creation [unique.ptr.create]

#include <memory>
#include <cstdlib>
#include <cstring>
#include <testsuite_hooks.h>

void* operator new(std::size_t n)
{
  void* p = std::malloc(n);
  std::memset(p, 0xaa, n);
  return p;
}

void operator delete(void* p) { std::free(p); }
void operator delete(void* p, std::size_t) { std::free(p); }

void
test01()
{
  std::unique_ptr<int> a = std::make_unique_for_overwrite<int>();
  VERIFY( a != nullptr );
  unsigned char buf[sizeof(int)];
  std::memcpy(buf, a.get(), sizeof(buf));
  for (unsigned char c : buf)
    VERIFY( c == 0xaa );
}

void
test02()
{
  std::unique_ptr<int[]> a = std::make_unique_for_overwrite<int[]>(3);
  VERIFY( a != nullptr );
  unsigned char buf[3 * sizeof(int)];
  std::memcpy(buf, a.get(), sizeof(buf));
  for (unsigned char c : buf)
    VERIFY( c == 0xaa );
}

int
main()
{
  test01();
  test02();
}
