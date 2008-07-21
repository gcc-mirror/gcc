// Copyright (C) 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.
//
// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// { dg-options "-std=gnu++0x" }

#include <unordered_set>
#include <testsuite_hooks.h>

using namespace std;

int test01()
{
  bool test __attribute__((unused)) = true;

  unordered_multiset<int> m({ 1, 5, 5, 37 });
  VERIFY(m.size() == 4);
  VERIFY(m.count(1) == 1);
  VERIFY(m.count(5) == 2);
  VERIFY(m.count(37) == 1);
  VERIFY(m.count(42) == 0);

  m = { 28, 37, 37, 37, 102 };
  VERIFY(m.size() == 5);
  VERIFY(m.count(28) == 1);
  VERIFY(m.count(37) == 3);
  VERIFY(m.count(102) == 1);
  VERIFY(m.count(1) == 0);

  m.insert({ 42, 42 });
  VERIFY(m.size() == 7);
  VERIFY(m.count(28) == 1);
  VERIFY(m.count(37) == 3);
  VERIFY(m.count(102) == 1);
  VERIFY(m.count(42) == 2);
  VERIFY(m.count(1) == 0);

  return test;
}

int main()
{
  __gnu_test::set_memory_limits();
  test01();
}
