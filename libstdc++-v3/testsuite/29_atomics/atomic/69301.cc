// Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-require-atomic-builtins "" }

#include <atomic>
#include <testsuite_hooks.h>

struct NonDefaultConstructible
{
  NonDefaultConstructible(int i) : val(i) { }
  int val;
};

template class std::atomic<NonDefaultConstructible>;

void
test01()
{
  std::atomic<NonDefaultConstructible> a(1);
  const auto n1 = a.exchange(2);
  VERIFY( n1.val == 1 );
  const auto n2 = a.load();
  VERIFY( n2.val == 2 );
}

void
test02()
{
  volatile std::atomic<NonDefaultConstructible> a(1);
  const auto n1 = a.exchange(2);
  VERIFY( n1.val == 1 );
  const auto n2 = a.load();
  VERIFY( n2.val == 2 );
}

int
main()
{
  test01();
  test02();
}
