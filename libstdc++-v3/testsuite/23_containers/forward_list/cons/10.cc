// { dg-do run { target c++11 } }

// Copyright (C) 2012-2024 Free Software Foundation, Inc.
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

// 23.3.4.2 forward_list construction [forwardlist.cons]

#include <forward_list>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

struct Counter
{
  Counter() { ++create; }
  Counter(const Counter&) { ++create; }
  ~Counter() { ++destroy; }

  static int create;
  static int destroy;
};

int Counter::create = 0;
int Counter::destroy = 0;

void test01()
{
  typedef __gnu_test::uneq_allocator<Counter> alloc;
  typedef std::forward_list<Counter, alloc> list;

  {
    Counter c;

    list l( list(10, c, alloc(1)), alloc(2) );

    VERIFY( Counter::create == 21 );
    VERIFY( Counter::destroy == 10 );
  }
  VERIFY( Counter::destroy == 21 );
}

int main()
{
  test01();
}
