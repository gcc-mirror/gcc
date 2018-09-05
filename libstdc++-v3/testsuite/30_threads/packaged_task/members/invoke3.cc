// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2009-2018 Free Software Foundation, Inc.
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

int& inc(int& i) { ++i; return i; }

void test01()
{
  bool test = false;

  std::packaged_task<void(int&)> p1(inc);

  int i1 = 0;
  p1(i1);

  VERIFY( i1 == 1 );

  try
  {
    p1(i1);
  }
  catch (const std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::promise_already_satisfied );
    test = true;
  }

  VERIFY( i1 == 1 );
  VERIFY( test );
}

int main()
{
  test01();
  return 0;
}
