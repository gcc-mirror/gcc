// 2004-01-13  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2022 Free Software Foundation, Inc.
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

// 21.3.6.8 basic_string::compare [lib.string::compare]

#include <testsuite_string.h>
#include <testsuite_hooks.h>

// libstdc++/13650
void test01()
{
  using namespace __gnu_test;

  const char lit_01[] = { 'w', 'e', '\0', 'r', 'd' };
  const char lit_02[] = { 'w', 'e', 'i', '\0', 'd' };

  const char lit_ref_a[] = { 'w', 'e', '\0', 'q', 'd' };
  const string str_a(lit_ref_a, 5);
  VERIFY( str_a.compare(0, 5, lit_01, 5) < 0 );

  const char lit_ref_b[] = { 'w', 'e', 'i' };
  const string str_b(lit_ref_b, 3);
  VERIFY( str_b.compare(0, 3, lit_02, 5) < 0 );
}

int main()
{
  test01();
  return 0;
}
