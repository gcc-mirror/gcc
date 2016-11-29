// { dg-do run { target c++11 } }
// { dg-require-atomic-builtins "" }

// 2008-05-25  Sebastian Redl  <sebastian.redl@getdesigned.at>

// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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

// current_exception() under various conditions.

#include <exception>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  exception_ptr ep = current_exception();
  VERIFY( ep == 0 );
}

void test02()
{
  using namespace std;

  try {
    throw 0;
  } catch(...) {
    exception_ptr ep = current_exception();
    VERIFY( ep != 0 );
  }
}

void test03()
{
  using namespace std;

  try {
    throw exception();
  } catch(std::exception&) {
    exception_ptr ep = current_exception();
    VERIFY( ep != 0 );
  }
}

void test04()
{
  using namespace std;

  try {
    throw 0;
  } catch(...) {
    exception_ptr ep1 = current_exception();
    try {
      throw 0;
    } catch(...) {
      exception_ptr ep2 = current_exception();
      VERIFY( ep1 != ep2 );
    }
    exception_ptr ep3 = current_exception();
    // Not guaranteed by standard, but by this implementation.
    VERIFY( ep1 == ep3 );
  }
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
