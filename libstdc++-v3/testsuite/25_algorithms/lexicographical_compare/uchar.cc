// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-options "-fchar8_t" }
// { dg-do run }

#include <algorithm>
#include <testsuite_hooks.h>

// Check that the memcmp optimization in std::lexicographical_compare
// still works for mixed-type comparisons.

void
test01()
{
  unsigned char a = 0;
  char8_t b = 1;
  VERIFY( std::lexicographical_compare(&a, &a + 1, &b, &b + 1) );
  VERIFY( ! std::lexicographical_compare(&b, &b + 1, &a, &a + 1) );
}

void
test02()
{
  // N.B. if char is signed this won't actually use the memcmp optimization.
  unsigned char a = 0;
  char b = 1;
  VERIFY( std::lexicographical_compare(&a, &a + 1, &b, &b + 1) );
  VERIFY( ! std::lexicographical_compare(&b, &b + 1, &a, &a + 1) );
}

void
test03()
{
  // N.B. if char is signed this won't actually use the memcmp optimization.
  char a = 0;
  char8_t b = 1;
  VERIFY( std::lexicographical_compare(&a, &a + 1, &b, &b + 1) );
  VERIFY( ! std::lexicographical_compare(&b, &b + 1, &a, &a + 1) );
}

int main()
{
  test01();
  test02();
  test03();
}
