// Copyright (C) 2010-2017 Free Software Foundation, Inc.
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

// 20.7.11 Function template bind

// { dg-do run { target c++11 } }

#include <functional>
#include <testsuite_hooks.h>

using namespace std::placeholders;

int inc(int& i) { return ++i; }

void test01()
{
  int counter = 0;
  std::bind(&inc, _1)(counter);
  VERIFY(counter == 1 );
  std::bind(&inc, std::ref(counter))();
  VERIFY(counter == 2 );
}

struct Inc
{
  int operator()(int& i) const { return ++i; }
  void operator()(int&&) const { }

  int f(int& i) const { return ++i; }
};

void test02()
{
  int counter = 0;
  std::bind(Inc(), _1)(counter);
  VERIFY(counter == 1 );
  std::bind(&Inc::f, Inc(), std::ref(counter))();
  VERIFY(counter == 2 );
}

int main()
{
  test01();
  test02();
}
