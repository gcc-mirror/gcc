// { dg-do run { target c++11 } }

// Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

  istringstream iss;
  iss.imbue(loc_c);
  const time_get<char>& tget = use_facet<time_get<char>>(iss.getloc());
  typedef istreambuf_iterator<char> iter;
  const iter end;

  tm time;
  ios_base::iostate err = ios_base::badbit;

  iss.str("PM01:38:12");
  string format = "%p%I:%M:%S";
  time = tm();
  auto ret = tget.get(iter(iss), end, iss, err, &time,
		      format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_hour == 13 );
  VERIFY( time.tm_min == 38 );
  VERIFY( time.tm_sec == 12 );

  iss.str("05 37");
  format = "%C %y";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 537 - 1900 );

  iss.str("68");
  format = "%y";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2068 - 1900 );

  iss.str("69");
  format = "%y";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 1969 - 1900 );

  iss.str("03-Feb-2003");
  format = "%d-%b-%Y";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2003 - 1900 );
  VERIFY( time.tm_mon == 1 );
  VERIFY( time.tm_mday == 3 );
  VERIFY( time.tm_wday == 1 );
  VERIFY( time.tm_yday == 33 );

  iss.str("16-Dec-2020");
  format = "%d-%b-%Y";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2020 - 1900 );
  VERIFY( time.tm_mon == 11 );
  VERIFY( time.tm_mday == 16 );
  VERIFY( time.tm_wday == 3 );
  VERIFY( time.tm_yday == 350 );

  iss.str("16-Dec-2021");
  format = "%d-%b-%Y";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2021 - 1900 );
  VERIFY( time.tm_mon == 11 );
  VERIFY( time.tm_mday == 16 );
  VERIFY( time.tm_wday == 4 );
  VERIFY( time.tm_yday == 349 );

  iss.str("253 2020");
  format = "%j %Y";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2020 - 1900 );
  VERIFY( time.tm_mon == 8 );
  VERIFY( time.tm_mday == 9 );
  VERIFY( time.tm_wday == 3 );
  VERIFY( time.tm_yday == 252 );

  iss.str("233 2021");
  format = "%j %Y";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2021 - 1900 );
  VERIFY( time.tm_mon == 7 );
  VERIFY( time.tm_mday == 21 );
  VERIFY( time.tm_wday == 6 );
  VERIFY( time.tm_yday == 232 );

  iss.str("2020 23 3");
  format = "%Y %U %w";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2020 - 1900 );
  VERIFY( time.tm_mon == 5 );
  VERIFY( time.tm_mday == 10 );
  VERIFY( time.tm_wday == 3 );
  VERIFY( time.tm_yday == 161 );

  iss.str("2020 23 3");
  format = "%Y %W %w";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2020 - 1900 );
  VERIFY( time.tm_mon == 5 );
  VERIFY( time.tm_mday == 10 );
  VERIFY( time.tm_wday == 3 );
  VERIFY( time.tm_yday == 161 );

  iss.str("2021 43 Fri");
  format = "%Y %W %a";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2021 - 1900 );
  VERIFY( time.tm_mon == 9 );
  VERIFY( time.tm_mday == 29 );
  VERIFY( time.tm_wday == 5 );
  VERIFY( time.tm_yday == 301 );

  iss.str("2024 23 3");
  format = "%Y %U %w";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2024 - 1900 );
  VERIFY( time.tm_mon == 5 );
  VERIFY( time.tm_mday == 12 );
  VERIFY( time.tm_wday == 3 );
  VERIFY( time.tm_yday == 163 );

  iss.str("2024 23 3");
  format = "%Y %W %w";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 2024 - 1900 );
  VERIFY( time.tm_mon == 5 );
  VERIFY( time.tm_mday == 5 );
  VERIFY( time.tm_wday == 3 );
  VERIFY( time.tm_yday == 156 );

  // As an extension, parse also 4 digit years.
  iss.str("0068");
  format = "%y";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 68 - 1900 );

  iss.str("0069");
  format = "%y";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 69 - 1900 );

  iss.str("1492");
  format = "%y";
  time = tm();
  ret = tget.get(iter(iss), end, iss, err, &time,
		 format.data(), format.data()+format.size());
  VERIFY( err == ios_base::eofbit );
  VERIFY( ret == end );
  VERIFY( time.tm_year == 1492 - 1900 );
}

int
main()
{
  test01();
  return 0;
}
