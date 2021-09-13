// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

// Copyright (C) 2010-2021 Free Software Foundation, Inc.
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

#include <future>
#include <testsuite_hooks.h>

int do_work1(int value) { return value; }
int do_work2(int value) { return value * 2; }

int work1(int value)
{
  auto handle = std::async([=] { return do_work2(value); });
  int tmp = do_work1(value);
  return tmp + handle.get();
}

int work2(int value)
{
  auto handle = std::async(do_work2, value);
  int tmp = do_work1(value);
  return tmp + handle.get();
}

// libstdc++/42819
void test01()
{
  VERIFY( work1(1) == 3 );
  VERIFY( work2(2) == 6 );
}

int main()
{
  test01();
  return 0;
}
