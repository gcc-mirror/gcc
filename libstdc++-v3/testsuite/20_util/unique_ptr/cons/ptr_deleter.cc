// { dg-options "-std=gnu++0x" }
// { dg-do run }

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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

// 20.9.10 Template class unique_ptr [unique.ptr]

#include <memory>
#include <testsuite_hooks.h>

static int count;

void del(int* p) { ++count; delete p; }
void vdel(int* p) { ++count; delete[] p; }

void
test01()
{
  bool test __attribute__((unused)) = true;
  count = 0;
  {
    std::unique_ptr<int, void(*)(int*)> p(nullptr, del);
  }
  VERIFY( count == 0 );
  {
    std::unique_ptr<int, void(*)(int*)> p(new int, del);
  }
  VERIFY( count == 1 );
}

void
test02()
{
  bool test __attribute__((unused)) = true;
  count = 0;
  {
    std::unique_ptr<int[], void(*)(int*)> p(nullptr, vdel);
  }
  VERIFY( count == 0 );
  {
    std::unique_ptr<int[], void(*)(int*)> p(new int[1], vdel);
  }
  VERIFY( count == 1 );
}

int
main()
{
  test01();
  test02();
  return 0;
}

