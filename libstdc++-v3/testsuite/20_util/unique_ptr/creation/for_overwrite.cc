// { dg-options "-fno-lifetime-dse -O0" }
// { dg-do run { target c++20 } }
// { dg-xfail-run-if "AIX operator new" { powerpc-ibm-aix* } }
// { dg-require-effective-target hosted }
// { dg-add-options no_pch }

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

// C++20 20.11.1.5 unique_ptr creation [unique.ptr.create]

#include <memory>

#ifndef __cpp_lib_smart_ptr_for_overwrite
# error "Feature-test macro for make_unique_for_overwrite missing in <memory>"
#elif __cpp_lib_smart_ptr_for_overwrite < 202002L
# error "Feature-test macro for make_unique_for_overwrite has wrong value in <memory>"
#endif

#include <cstdlib>
#include <cstring>
#include <testsuite_hooks.h>

void* operator new(std::size_t n)
{
  void* p = std::malloc(n);
  std::memset(p, 0xaa, n);
  return p;
}

void operator delete(void* p) noexcept { std::free(p); }
void operator delete(void* p, std::size_t) noexcept { std::free(p); }

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

void
test03()
{
  // Type with non-trivial initialization should still be default-initialized.
  struct NonTriv
  {
    int init = 0xbb;
    int uninit;
  };
  std::unique_ptr<NonTriv> a = std::make_unique_for_overwrite<NonTriv>();
  VERIFY( a->init == 0xbb );
  std::unique_ptr<NonTriv[]> b = std::make_unique_for_overwrite<NonTriv[]>(2);
  VERIFY( b[1].init == 0xbb );
}

int
main()
{
  test01();
  test02();
  test03();
}
