// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <algorithm>
#include <testsuite_hooks.h>

using std::signed_integral;

namespace ranges = std::ranges;

template<signed_integral T>
void
test01()
{
  T i[] = { -1 };
  T j[] = { 1 };

  VERIFY( ranges::lexicographical_compare(i, j) );
  VERIFY( !ranges::lexicographical_compare(i, j, ranges::greater{}) );

  VERIFY( !ranges::lexicographical_compare(j, i) );
  VERIFY( ranges::lexicographical_compare(j, i, ranges::greater{}) );
}

template<signed_integral T>
void
test02()
{
  T i[] = { -5 };
  T j[] = { -5, 3 };

  VERIFY( ranges::lexicographical_compare(i, j) );
  VERIFY( ranges::lexicographical_compare(i, j, ranges::greater{}) );

  VERIFY( !ranges::lexicographical_compare(j, i) );
  VERIFY( !ranges::lexicographical_compare(j, i, ranges::greater{}) );
}

template<signed_integral T>
void
test03()
{
  T i[] = { -10 };
  T j[] = { -5, 3 };

  VERIFY( ranges::lexicographical_compare(i, j) );
  VERIFY( !ranges::lexicographical_compare(i, j, ranges::greater{}) );

  VERIFY( !ranges::lexicographical_compare(j, i) );
  VERIFY( ranges::lexicographical_compare(j, i, ranges::greater{}) );
}

template<signed_integral T>
void
test04()
{
  T i[] = { -2 };
  T j[] = { -5, 3 };

  VERIFY( !ranges::lexicographical_compare(i, j) );
  VERIFY( ranges::lexicographical_compare(i, j, ranges::greater{}) );

  VERIFY( ranges::lexicographical_compare(j, i) );
  VERIFY( !ranges::lexicographical_compare(j, i, ranges::greater{}) );
}

void
test05()
{
  unsigned i[] = { 1 };
  unsigned j[] = { 256 };

  VERIFY( ranges::lexicographical_compare(i, j) );
  VERIFY( !ranges::lexicographical_compare(i, j, ranges::greater{}) );

  VERIFY( !ranges::lexicographical_compare(j, i) );
  VERIFY( ranges::lexicographical_compare(j, i, ranges::greater{}) );
}

void
test06()
{
  signed char i[] = { 100, 1 };
  unsigned char j[] = { 100 };

  VERIFY( !ranges::lexicographical_compare(i, j) );
  VERIFY( !ranges::lexicographical_compare(i, j, ranges::greater{}) );

  VERIFY( ranges::lexicographical_compare(j, i) );
  VERIFY( ranges::lexicographical_compare(j, i, ranges::greater{}) );
}

void
test07()
{
  char i[] = { 95, 1 };
  unsigned char j[] = { 100 };

  VERIFY( ranges::lexicographical_compare(i, j) );
  VERIFY( !ranges::lexicographical_compare(i, j, ranges::greater{}) );

  VERIFY( !ranges::lexicographical_compare(j, i) );
  VERIFY( ranges::lexicographical_compare(j, i, ranges::greater{}) );
}

void
test08()
{
  signed char i[] = { 112, 1 };
  signed char j[] = { 87 };

  VERIFY( !ranges::lexicographical_compare(i, j) );
  VERIFY( ranges::lexicographical_compare(i, j, ranges::greater{}) );

  VERIFY( ranges::lexicographical_compare(j, i) );
  VERIFY( !ranges::lexicographical_compare(j, i, ranges::greater{}) );
}

void
test09()
{
  char i[] = { 1 };
  unsigned char j[] = { 100 };

  VERIFY( ranges::lexicographical_compare(i, j) );
  VERIFY( !ranges::lexicographical_compare(i, j, ranges::greater{}) );

  VERIFY( !ranges::lexicographical_compare(j, i) );
  VERIFY( ranges::lexicographical_compare(j, i, ranges::greater{}) );
}

int
main()
{
  test01<signed char>();
  test01<int>();

  test02<signed char>();
  test02<int>();

  test03<signed char>();
  test03<int>();

  test04<signed char>();
  test04<int>();

  test05();
  test06();
  test07();
  test08();
  test09();
}
