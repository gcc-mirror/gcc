// 2005-03-20 Douglas Gregor <doug.gregor -at- gmail.com>
//
// Copyright (C) 2005-2016 Free Software Foundation, Inc.
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

// 3.6 function object binders
#include <tr1/functional>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

using namespace __gnu_test;

struct X
{
  X() : bar(17) {}

  int foo(int x, int y) { return bar + x + y; }

  int bar;

private:
  X(const X&);
  X& operator=(const X&);
};

// Operations on empty function<> objects
void test01()
{
  using std::tr1::bind;
  using std::tr1::ref;
  ::X x;

  VERIFY( bind(&::X::foo, ref(x), 1, 2)() == 20 );
  VERIFY( bind<int>(&::X::foo, ref(x), 1, 2)() == 20 );
  VERIFY( bind(&::X::bar, ref(x))() == 17 );
  VERIFY( bind<int>(&::X::bar, ref(x))() == 17 );
}

int main()
{
  test01();
  return 0;
}
