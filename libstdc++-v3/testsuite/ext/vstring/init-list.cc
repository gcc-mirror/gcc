// Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }

#include <ext/vstring.h>
#include <testsuite_hooks.h>

void test01(void)
{
  __gnu_cxx::__vstring s1 = { 'a', 'b', 'c' };
  VERIFY(s1 == "abc");

  s1 = { 'd', 'e', 'f' };
  VERIFY(s1 == "def");

  s1 += { 'g', 'h', 'i' };
  VERIFY(s1 == "defghi");

  s1.append({ 'j', 'k', 'l' });
  VERIFY(s1 == "defghijkl");

  s1.assign({ 'm', 'n', 'o' });
  VERIFY(s1 == "mno");

  // There aren't actually overloads of insert and replace taking size_type
  // and initializer_list, but test the usage anyway.
  s1.insert(2, { 'p', 'q', 'r' });
  VERIFY(s1 == "mnpqro");

  s1.replace(2, 3, { 's', 't', 'u' });
  VERIFY(s1 == "mnstuo");

  __gnu_cxx::__vstring::iterator i1, i2;

  i1 = s1.begin()+2;
  s1.insert(i1, { 'v', 'w', 'x' });
  VERIFY(s1 == "mnvwxstuo");

  i1 = s1.begin()+2;
  i2 = i1+6;
  s1.replace(i1, i2, { 'y', 'z' });
  VERIFY(s1 == "mnyzo");
}

int main()
{
  __gnu_test::set_memory_limits();
  test01();
  return 0;
}
