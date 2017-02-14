// { dg-do run { target c++11 } }

// 2014-04-14 Rüdiger Sonderfeld  <ruediger@c-plusplus.de>

// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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
  locale loc_c = locale::classic();
  istringstream iss;
  iss.imbue(loc_c);
  iss.str("12:01:30  1971");
  tm time1;
  iss >> get_time(&time1, "%H:%M:%S %Y");
  VERIFY( static_cast<bool>(iss) );
  VERIFY(time1.tm_hour == 12);
  VERIFY(time1.tm_min == 1);
  VERIFY(time1.tm_sec == 30);
  VERIFY(time1.tm_year == 71);
}

int main()
{
  test01();
}
