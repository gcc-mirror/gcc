// { dg-require-namedlocale "is_IS.ISO8859-1" }

// 2003-10-27 Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2003-2021 Free Software Foundation, Inc.
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

// libstdc++/12750
void test01()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> iterator_type;

  // basic construction
  locale loc_is = locale(ISO_8859(1,is_IS));

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;

  wistringstream iss;
  const time_get<wchar_t>& tim_get = use_facet<time_get<wchar_t> >(iss.getloc()); 

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // create "C" time objects
  const tm time_bday01 = __gnu_test::test_tm(0, 0, 12, 2, 9, 103, 4, 274, -1);
  const tm time_bday02 = __gnu_test::test_tm(0, 0, 12, 26, 9, 103, 0, 298, -1);

  // inspection of named locales, is_IS
  iss.imbue(loc_is);

  iss.str(L"Fim  2.Okt 2003");
  iterator_type is_it01(iss);
  tm time01;
  errorstate = good;
  tim_get.get_date(is_it01, end, iss, errorstate, &time01);
  VERIFY( time01.tm_mon == time_bday01.tm_mon );
  VERIFY( time01.tm_mday == time_bday01.tm_mday );
  VERIFY( time01.tm_year == time_bday01.tm_year );
  VERIFY( errorstate == ios_base::eofbit );

  iss.str(L"Sun 26.Okt 2003");
  iterator_type is_it02(iss);
  tm time02;
  errorstate = good;
  tim_get.get_date(is_it02, end, iss, errorstate, &time02);
  VERIFY( time02.tm_mon == time_bday02.tm_mon );
  VERIFY( time02.tm_mday == time_bday02.tm_mday );
  VERIFY( time02.tm_year == time_bday02.tm_year );
  VERIFY( errorstate == ios_base::eofbit );
}

int main()
{
  test01();
  return 0;
}
