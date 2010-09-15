// { dg-require-namedlocale "en_GB" }

// 2010-01-06  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010 Free Software Foundation
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

// 22.2.5.1.1 time_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/26701
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  typedef istreambuf_iterator<char> iterator_type;

  locale loc_en = locale("en_GB");
  
  tm tm0 = __gnu_test::test_tm(0, 0, 0, 0, 0, 0, 0, 0, 0);

  iterator_type end;

  istringstream iss; 
  iss.imbue(loc_en);
  const time_get<char>& tg = use_facet<time_get<char> >(iss.getloc());

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  iss.str("01/02/2003");
  iterator_type is_it0(iss);
  
  errorstate = good;
  tg.get_date(is_it0, end, iss, errorstate, &tm0);
  VERIFY( errorstate == ios_base::eofbit );
  VERIFY( tm0.tm_year + 1900 == 2003 );
  VERIFY( tm0.tm_mon + 1 == 2 );
  VERIFY( tm0.tm_mday == 1 );
}

int main()
{
  test01();
  return 0;
}
