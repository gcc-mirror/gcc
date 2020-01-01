// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

#include <utility>
#include <testsuite_hooks.h>

void
test01()
{
  std::pair<std::pair<int, int>, int> p;
#if __cplusplus >= 201103L
  static_assert(sizeof(p) == (3 * sizeof(int)), "PR libstdc++/87822");
#endif
  VERIFY( (void*)&p == (void*)&p.first );
  VERIFY( (void*)&p == (void*)&p.first.first );
}

struct empty { };

void
test02()
{
  std::pair<std::pair<empty, empty>, empty> p;
#if __cplusplus >= 201103L
  static_assert(sizeof(p) == (3 * sizeof(empty)), "PR libstdc++/87822");
#endif
  VERIFY( (void*)&p == (void*)&p.first );
}

void
test03()
{
  typedef std::pair<int, int> int_pair;
  typedef std::pair<int_pair, int_pair> int_pair_pair;
  std::pair<int_pair_pair, int_pair_pair> p;
#if __cplusplus >= 201103L
  static_assert(sizeof(int_pair_pair) == (2 * sizeof(int_pair)), "nested");
  static_assert(sizeof(p) == (2 * sizeof(int_pair_pair)), "nested again");
#endif
  VERIFY( (void*)&p == (void*)&p.first );
  VERIFY( (void*)&p == (void*)&p.first.first );
  VERIFY( (void*)&p == (void*)&p.first.first.first );
}

int main()
{
  test01();
  test02();
  test03();
}
