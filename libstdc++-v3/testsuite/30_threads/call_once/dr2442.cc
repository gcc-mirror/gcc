// { dg-do run { target c++11 } }
// { dg-additional-options "-pthread" { target pthread } }

// Copyright (C) 2016-2024 Free Software Foundation, Inc.
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

#include <mutex>
#include <testsuite_hooks.h>

void f(int& a, int&& b) { a = 1; b = 2; }

void
test01()
{
  // LWG 2442. call_once() shouldn't DECAY_COPY()
  std::once_flag once;
  int i = 0;
  int j = 0;
  call_once(once, f, i, std::move(j));
  VERIFY( i == 1 );
  VERIFY( j == 2 );
}

int
main()
{
  test01();
}
