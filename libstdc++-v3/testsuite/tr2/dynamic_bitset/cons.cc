// Copyright (C) 2019 Free Software Foundation, Inc.
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

#include <tr2/dynamic_bitset>
#include <testsuite_hooks.h>

void
test01()
{
  std::tr2::dynamic_bitset<> a;
  VERIFY( a.size() == 0 );
  VERIFY( a.empty() );
  std::tr2::dynamic_bitset<> b(1);
  VERIFY( b.size() == 1 );
  VERIFY( !b.empty() );
  VERIFY( a != b );
}

void
test02()
{
  std::tr2::dynamic_bitset<> a(1, 0); // { 0 }
  std::tr2::dynamic_bitset<> b(2, 2); // { 0, 1 }
  VERIFY( a != b );
}

void
test03()
{
  std::tr2::dynamic_bitset<> a;
  a.resize(1);                        // { 0 }
  std::tr2::dynamic_bitset<> b(2, 2); // { 0, 1 }
  VERIFY( a != b );
}

void
test04()
{
  std::tr2::dynamic_bitset<> a(3, 2); // { 0, 1, 0 }
  std::tr2::dynamic_bitset<> b(2, 2); // { 0, 1 }
  VERIFY( a != b );
}

void
test05()
{
  std::tr2::dynamic_bitset<unsigned short> a(1, 0); // { 0 }
  std::tr2::dynamic_bitset<unsigned short> b(2, 2); // { 0, 1 }
  VERIFY( a != b );
}

void
test06()
{
  std::tr2::dynamic_bitset<unsigned short> a;
  a.resize(1);                                      // { 0 }
  std::tr2::dynamic_bitset<unsigned short> b(2, 2); // { 0, 1 }
  VERIFY( a != b );
}

void
test07()
{
  std::tr2::dynamic_bitset<unsigned short> a(3, 2); // { 0, 1, 0 }
  std::tr2::dynamic_bitset<unsigned short> b(2, 2); // { 0, 1 }
  VERIFY( a != b );
}

void
test08()
{
  std::tr2::dynamic_bitset<> a(65, -1ULL);
  std::tr2::dynamic_bitset<> b(64, -1ULL);
  b.push_back(0);
  VERIFY( a == b );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
}
