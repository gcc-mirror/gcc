// Copyright (C) 2022-2024 Free Software Foundation, Inc.
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
//
// { dg-do run { target c++11 xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <forward_list>

#include <testsuite_hooks.h>

void test01()
{
  typedef typename std::forward_list<int>::iterator It;
  It end1, end2;

  {
    std::forward_list<int> fl;
    fl.push_front(1);

    end1 = end2 = fl.end();
    VERIFY( end1 == end2 );
  }

  (void)(end1 == end2);
}

int main()
{
  test01();
  return 0;
}
