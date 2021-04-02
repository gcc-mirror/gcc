// { dg-do run { target c++11 } }
// { dg-require-namedlocale "de_DE.UTF-8" }

// 2014-04-14 Rüdiger Sonderfeld  <ruediger@c-plusplus.de>

// Copyright (C) 2014-2021 Free Software Foundation, Inc.
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

// 22.4.5.1.1 (C++11) time_get members [locale.time.get.members]

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

#ifdef TEST_TIMEGET_VERBOSE
#  include <iostream>
#  define PRINT(x) cout << #x << ": " << x << endl
#  define TESTHEAD(x) cout << x << endl
#else
#  define PRINT(x) do {} while(false)
#  define TESTHEAD(x) do {} while(false)
#endif

void test02()
{
  using namespace std;

  locale loc_c = locale::classic();
  locale loc_de = locale("de_DE.UTF-8");
  VERIFY( loc_de != loc_c );

  wistringstream iss;
  iss.imbue(loc_de);
  const time_get<wchar_t>& tget = use_facet<time_get<wchar_t>>(iss.getloc());
  typedef istreambuf_iterator<wchar_t> iter;
  const iter end;

  ios_base::iostate err;
  tm time;

  TESTHEAD("German locale test");
  iss.str(L"Montag, den 14. April 2014");
  wstring format = L"%A, den %d. %B %Y";
  auto ret = tget.get(iter(iss), end, iss, err, &time,
                      format.data(), format.data()+format.size());
  PRINT(err);
  VERIFY(err == ios_base::eofbit);
  PRINT(time.tm_year);
  VERIFY(time.tm_year == 114);
  PRINT(time.tm_mon);
  VERIFY(time.tm_mon == 3);
  PRINT(time.tm_wday);
  VERIFY(time.tm_wday == 1);
  PRINT(time.tm_mday);
  VERIFY(time.tm_mday == 14);
  VERIFY(end == end);

  TESTHEAD("German locale: Check case-insensitivity");
  tm time2;
  iss.str(L"Montag, den 14. April 2014");
  format = L"%A, DEN %d. %B %Y"; // check case-insensitivity
  ret = tget.get(iter(iss), end, iss, err, &time2,
                 format.data(), format.data()+format.size());
  PRINT(err);
  VERIFY(err == ios_base::eofbit);
  PRINT(time2.tm_year);
  VERIFY(time2.tm_year == 114);
  PRINT(time2.tm_mon);
  VERIFY(time2.tm_mon == 3);
  PRINT(time2.tm_wday);
  VERIFY(time2.tm_wday == 1);
  PRINT(time2.tm_mday);
  VERIFY(time2.tm_mday == 14);
  VERIFY(end == end);

  TESTHEAD("German locale: Check single");
  iss.str(L"Mittwoch");
  ret = tget.get(iter(iss), end, iss, err, &time, L'A');
  PRINT(err);
  VERIFY(err == ios_base::eofbit);
  PRINT(time.tm_wday);
  VERIFY(time.tm_wday == 3);
  VERIFY(end == end);
}

int main()
{
  test02();
}
