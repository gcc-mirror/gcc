// { dg-require-namedlocale "de_DE.UTF-8" }
// { dg-do run { target c++11 } }

// 2014-04-14 Rüdiger Sonderfeld  <ruediger@c-plusplus.de>

// Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

// 27.7.5. (C++11) Extended manipulators [ext.manip]: put_time

#include <locale>
#include <sstream>
#include <iomanip>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  locale loc_c = locale::classic();
  locale loc_de = locale("de_DE.UTF-8");
  VERIFY( loc_de != loc_c );
  wostringstream oss;
  oss.imbue(loc_de);
  const tm time1 = __gnu_test::test_tm(0, 0, 12, 4, 3, 71, 0, 93, 0);
  oss << put_time(&time1, L"%A %Y");
  VERIFY(oss.str() == L"Sonntag 1971");
}

int main()
{
  test01();
}
