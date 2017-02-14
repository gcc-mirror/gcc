// { dg-do run { target c++11 } }
// Copyright (C) 2009-2017 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 20.7.11 Function template bind

#include <functional>
#include <testsuite_hooks.h>

struct X
{
  typedef int result_type;
  int operator()(int i) const { return i+1; }
  bool b;
};

void test01()
{
  using std::bind;
  using std::ref;
  ::X x = { true };

  // test bind<R> form
  bind<void>(ref(x), 1)();
  VERIFY( bind<long>(ref(x), 1)() == 2 );
  bind<void>(&::X::b, ref(x))();
  VERIFY( bind<int>(&::X::b, ref(x))() == 1 );
}

int main()
{
  test01();
  return 0;
}
