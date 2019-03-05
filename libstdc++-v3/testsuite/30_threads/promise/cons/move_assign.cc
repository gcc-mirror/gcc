// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2009-2019 Free Software Foundation, Inc.
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

void test01()
{
  using namespace std;

  // move assign
  promise<int> p1;
  p1.set_value(3);
  promise<int> p2;
  p2 = move(p1);
  VERIFY( p2.get_future().get() == 3 );
  try
  {
    p1.get_future();
    VERIFY( false );
  }
  catch (future_error& e)
  {
    VERIFY(e.code() == make_error_code(future_errc::no_state));
  }
}

int main()
{
  test01();
  return 0;
}
