// { dg-do run { target c++11 } }

// Copyright (C) 2021-2023 Free Software Foundation, Inc.
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

#include <locale>
#include <sstream>
#include <iterator>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std;

  locale loc_c = locale::classic();

  wistringstream iss;
  iss.imbue(loc_c);
  const time_get<wchar_t>& tget = use_facet<time_get<wchar_t>>(iss.getloc());
  typedef istreambuf_iterator<wchar_t> iter;
  const iter end;

  tm time;
  ios_base::iostate err = ios_base::badbit;

  iss.str(L"20:48:01 MAR 31 2016");
  wstring format = L"%H:%M:%S %b %d %Y";
  auto ret = tget.get(iter(iss), end, iss, err, &time,
		      format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2016 - 1900 );
  VERIFY( time.tm_mon == 2 );
  VERIFY( time.tm_mday == 31 );
  VERIFY( time.tm_hour == 20 );
  VERIFY( time.tm_min == 48 );
  VERIFY( time.tm_sec == 01 );

  iss.str(L"21:38:11 apr 30 2017");
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2017 - 1900 );
  VERIFY( time.tm_mon == 3 );
  VERIFY( time.tm_mday == 30 );
  VERIFY( time.tm_hour == 21 );
  VERIFY( time.tm_min == 38 );
  VERIFY( time.tm_sec == 11 );

  iss.str(L"22:28:21 mAy 29 2018");
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2018 - 1900 );
  VERIFY( time.tm_mon == 4 );
  VERIFY( time.tm_mday == 29 );
  VERIFY( time.tm_hour == 22 );
  VERIFY( time.tm_min == 28 );
  VERIFY( time.tm_sec == 21 );

  iss.str(L"23:18:31 JuN 28 2019");
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2019 - 1900 );
  VERIFY( time.tm_mon == 5 );
  VERIFY( time.tm_mday == 28 );
  VERIFY( time.tm_hour == 23 );
  VERIFY( time.tm_min == 18 );
  VERIFY( time.tm_sec == 31 );
}

int
main()
{
  test01();
  return 0;
}
