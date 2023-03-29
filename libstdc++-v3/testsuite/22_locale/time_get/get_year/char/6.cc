// Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

void test01()
{
  using namespace std;

  typedef istreambuf_iterator<char> iterator_type;

  locale loc_c = locale::classic();

  iterator_type end;

  istringstream iss;
  iss.imbue(loc_c);
  const time_get<char>& tim_get = use_facet<time_get<char> >(iss.getloc());
  ios_base::iostate errorstate = ios_base::goodbit;

  iss.str("69");
  iterator_type is_it01(iss);
  tm time01;
  tim_get.get_year(is_it01, end, iss, errorstate, &time01);
  VERIFY( time01.tm_year == 1969 - 1900 );
  VERIFY( errorstate == ios_base::eofbit );

  iss.str("68 ");
  iterator_type is_it02(iss);
  tm time02;
  errorstate = ios_base::goodbit;
  iterator_type ret02 = tim_get.get_year(is_it02, end, iss, errorstate,
					 &time02);
  VERIFY( time02.tm_year == 2068 - 1900 );
  VERIFY( errorstate == ios_base::goodbit );
  VERIFY( *ret02 == ' ' );

  iss.str("0069");
  iterator_type is_it03(iss);
  tm time03;
  errorstate = ios_base::goodbit;
  iterator_type ret03 = tim_get.get_year(is_it03, end, iss, errorstate,
					 &time03);
  VERIFY( time03.tm_year == 69 - 1900 );
  VERIFY( errorstate == ios_base::eofbit );

  iss.str("0068");
  iterator_type is_it04(iss);
  tm time04;
  errorstate = ios_base::goodbit;
  iterator_type ret04 = tim_get.get_year(is_it04, end, iss, errorstate,
					 &time04);
  VERIFY( time04.tm_year == 68 - 1900 );
  VERIFY( errorstate == ios_base::eofbit );
}

int main()
{
  test01();
  return 0;
}
