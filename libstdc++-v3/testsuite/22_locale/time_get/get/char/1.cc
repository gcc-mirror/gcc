// { dg-do run { target c++11 } }

// 2014-04-14 Rüdiger Sonderfeld  <ruediger@c-plusplus.de>

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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
#include <iterator>
#include <testsuite_hooks.h>

#ifdef TEST_TIMEGET_VERBOSE
#  include <iostream>
#  define PRINT(x) cout << #x << ": " << x << endl
#  define TESTHEAD(x) cout << x << endl
#else
#  define PRINT(x) do {} while(false)
#  define TESTHEAD(x) do {} while(false)
#endif

void test01()
{
  using namespace std;

  locale loc_c = locale::classic();

  istringstream iss;
  iss.imbue(loc_c);
  const time_get<char>& tget = use_facet<time_get<char>>(iss.getloc());
  typedef istreambuf_iterator<char> iter;
  const iter end;

  tm time;
  ios_base::iostate err = ios_base::badbit;

  // check regular operations with format string
  TESTHEAD("regular operations");
  iss.str("d 2014-04-14 01:09:35");
  string format = "d %Y-%m-%d %H:%M:%S";
  auto ret = tget.get(iter(iss), end, iss, err, &time,
                      format.data(), format.data()+format.size());
  PRINT(err);
  VERIFY(err == ios_base::eofbit);
  VERIFY(ret == end);
  PRINT(time.tm_year);
  VERIFY(time.tm_year == 114);
  PRINT(time.tm_mon);
  VERIFY(time.tm_mon == 3);
  PRINT(time.tm_mday);
  VERIFY(time.tm_mday == 14);
  PRINT(time.tm_hour);
  VERIFY(time.tm_hour == 1);
  PRINT(time.tm_min);
  VERIFY(time.tm_min == 9);
  PRINT(time.tm_sec);
  VERIFY(time.tm_sec == 35);

  TESTHEAD("check eof");
  iss.str("2020  ");
  format = "%Y";
  ret = tget.get(iter(iss), end, iss, err, &time,
                 format.data(), format.data()+format.size());
  VERIFY(err != ios_base::eofbit);
  VERIFY(time.tm_year == 120);
  VERIFY(ret != end);

  TESTHEAD("check broken format");
  iss.str("2014-04-14 01:09:35");
  format = "%";
  ret = tget.get(iter(iss), end, iss, err, &time,
                 format.data(), format.data()+format.size());
  VERIFY(err == ios_base::failbit);

  TESTHEAD("check single format letter version");
  iss.str("2020");
  ret = tget.get(iter(iss), end, iss, err, &time, 'Y');
  VERIFY(err == ios_base::eofbit);
  VERIFY(time.tm_year == 120);
  VERIFY(ret == end);

  TESTHEAD("check skipping of space");
  iss.str("2010    07 01");
  format = "%Y %m %d";
  ret = tget.get(iter(iss), end, iss, err, &time,
                 format.data(), format.data()+format.size());
  VERIFY(err == ios_base::eofbit);
  VERIFY(time.tm_year == 110);
  VERIFY(time.tm_mon == 6);
  VERIFY(time.tm_mday == 1);
  VERIFY(ret == end);

  TESTHEAD("check mismatch");
  iss.str("year: 1970");
  format = "jahr: %Y";
  ret = tget.get(iter(iss), end, iss, err, &time,
                 format.data(), format.data()+format.size());
  VERIFY(err == ios_base::failbit);
  VERIFY(ret == iter(iss));

  TESTHEAD("check case insensitive match");
  iss.str("yEaR: 1980");
  format = "YeAR: %Y";
  ret = tget.get(iter(iss), end, iss, err, &time,
                 format.data(), format.data()+format.size());
  VERIFY(err == ios_base::eofbit);
  VERIFY(ret == end);
  VERIFY(time.tm_year == 80);
}

int main()
{
  test01();
  return 0;
}
