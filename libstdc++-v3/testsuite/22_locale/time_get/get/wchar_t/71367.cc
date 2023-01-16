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

  iss.str(L"01:38:12 PM");
  wstring format = L"%I:%M:%S %p";
  auto ret = tget.get(iter(iss), end, iss, err, &time,
		      format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 13 );
  VERIFY( time.tm_min == 38 );
  VERIFY( time.tm_sec == 12 );

  iss.str(L"11:17:42 PM");
  format = L"%r";
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 23 );
  VERIFY( time.tm_min == 17 );
  VERIFY( time.tm_sec == 42 );
}

int
main()
{
  test01();
  return 0;
}
